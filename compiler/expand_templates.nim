
import fexpr_core
import passutils, ccodegen, compileutils

import options
import strutils, sequtils
import tables

proc expandDeftype*(scope: Scope, fexpr: var FExpr, argtypes: seq[Symbol]): FExpr

proc applyInstance*(sym: Symbol, instance: Symbol): bool =
  if sym.kind in {symbolVar, symbolRef} and instance.kind in {symbolVar, symbolRef}:
    return sym.wrapped.applyInstance(instance.wrapped):
  elif sym.kind == symbolMove and instance.kind == symbolMove:
    return sym.wrapped.applyInstance(instance.wrapped)
  elif sym.kind == symbolFuncType and instance.kind == symbolFuncType:
    for i in 0..<sym.argtypes.len:
      if not sym.argtypes[i].applyInstance(instance.argtypes[i]):
        return false
    return sym.rettype.applyInstance(instance.rettype)
  elif instance.kind in {symbolVar, symbolRef}:
    return sym.applyInstance(instance.wrapped)
  elif sym.kind == symbolMove:
    return sym.wrapped.applyInstance(instance)
  elif instance.kind == symbolMove:
    return sym.applyInstance(instance.wrapped)
  elif sym.kind == symbolGenerics:
    if sym.instance.isSome:
      if not sym.instance.get.spec(instance):
        return false
    else:
      sym.instance = some(instance)
    return true
  elif sym.kind == symbolTypeGenerics:
    instance.fexpr.assert(instance.kind == symbolTypeGenerics)
    assert(sym.types.len == instance.types.len)
    for i in 0..<sym.types.len:
      if not sym.types[i].applyInstance(instance.types[i]):
        return false
    sym.instance = some(instance)
    return true
  else:
    sym.instance = some(instance)
    return true

proc expandSymbol*(scope: Scope, sym: Symbol): Symbol =
  if sym.kind == symbolVar:
    result = scope.varsym(scope.expandSymbol(sym.wrapped))
  elif sym.kind == symbolRef:
    result = scope.refsym(scope.expandSymbol(sym.wrapped))
  elif sym.kind == symbolMove:
    result = scope.movesym(scope.expandSymbol(sym.wrapped))
  elif sym.kind == symbolFuncType:
    result = scope.symbol(name("Fn"), symbolFuncType, sym.fexpr)
    result.argtypes = @[]
    for t in sym.argtypes:
      result.argtypes.add(scope.expandSymbol(t))
    result.rettype = scope.expandSymbol(sym.rettype)
  elif sym.kind == symbolGenerics:
    if sym.instance.isNone:
      sym.fexpr.error("cannot instantiate $#." % $sym)
    result = sym.instance.get
  elif sym.kind == symbolTypeGenerics:
    var extypes = newSeq[Symbol]()
    for t in sym.types:
      extypes.add(scope.expandSymbol(t))
    result = scope.expandDeftype(sym.fexpr, extypes).symbol
  else:
    result = sym

proc expandType*(scope: Scope, fexpr: FExpr): FExpr =
  fexpr.assert(fexpr.kind == fexprSymbol)
  return fsymbol(fexpr.span, scope.expandSymbol(fexpr.symbol))

proc expandGenerics*(generics: FExpr) =
  for g in generics.mitems:
    g.assert(g.kind == fexprSymbol)
    if g.symbol.kind != symbolGenerics:
      continue
    if g.symbol.instance.isNone:
      g.error("cannot instantiate $#." % $g)
    g.symbol = g.symbol.instance.get
      
proc expandDeftype*(scope: Scope, fexpr: var FExpr, argtypes: seq[Symbol]): FExpr =
  fexpr.assert(fexpr.hasDeftype)

  let expanded = fexpr.copy

  for i, arg in argtypes:
    if not expanded.deftype.generics[i].symbol.applyInstance(arg):
      expanded.deftype.generics[i].error("generics type not match: $#, $#" % $[$expanded.deftype.generics[i].symbol.instance.get, $arg])
  defer:
    for g in fexpr.deftype.generics:
      g.symbol.instance = none(Symbol)
  expanded.deftype.generics.expandGenerics()

  let typename = expanded.deftype.name.symbol.name
  let manglingname = name(codegenMangling(expanded.deftype.name.symbol, argtypes, @[], internal = true))

  let specopt = fexpr.internalScope.getDecl(manglingname)
  if specopt.isSome:
    let fsym = fsymbol(fexpr.span, specopt.get)
    return fsym
  
  for b in expanded.deftype.body.mitems:
    let extype = scope.expandType(b[1])
    b[1] = extype
    # b = fseq(b.span, @[b[0], extype])

  for argtype in argtypes:
    if argtype.kind == symbolTypeGenerics:
      for g in fexpr.deftype.generics:
        g.symbol.instance = none(Symbol)
      discard scope.expandDeftype(argtype.fexpr, argtype.types)

  let tsym = fexpr.internalScope.symbol(typename, symbolTypeGenerics, expanded)
  tsym.types = expanded.deftype.generics.mapIt(it.symbol)
  let fsym = fsymbol(fexpr.span, tsym)
  expanded.deftype.name = fsym

  discard fexpr.internalScope.addDecl(manglingname, tsym)
  scope.ctx.globaltoplevels.add(expanded)

  return fsym
    
proc expandDefn*(rootPass: PassProcType, scope: Scope, fexpr: FExpr, argtypes: seq[Symbol]): FExpr =
  fexpr.assert(fexpr.hasDefn)
  fexpr.assert(fexpr.defn.args.len == argtypes.len)

  let expanded = fexpr.copy

  expanded.defn.generics.expandGenerics()

  let funcname = fexpr.defn.name.symbol.name
  let genericstypes = expanded.defn.generics.mapIt(it.symbol)

  let specopt = fexpr.internalScope.getSpecFunc(procname(funcname, argtypes, genericstypes))
  if specopt.isSome:
    let fsym = fsymbol(fexpr.span, specopt.get.sym)
    return fsym

  let exscope = fexpr.internalScope.extendScope()
  for arg in expanded.defn.args:
    let extype = scope.expandType(arg[1])
    let argcopy = arg[0].copy
    argcopy.symbol = argcopy.symbol.symcopy
    argcopy.symbol.fexpr.typ = extype.symbol
    arg[0] = argcopy
    arg[1].symbol = extype.symbol.symcopy
    let status = exscope.addDecl(name(argcopy), argcopy.symbol)
    if not status:
      argcopy.error("redefinition $# variable." % $arg[0])
  expanded.defn.ret = scope.expandType(expanded.defn.ret)

  let sym = fexpr.internalScope.symbol(funcname, expanded.defn.name.symbol.kind, expanded)
  let fsym = fsymbol(fexpr.span, sym)
  fexpr.defn.name = fsym
  let pd = ProcDecl(
    isInternal: false,
    name: funcname,
    argtypes: expanded.defn.args.mapIt(it[1].symbol),
    generics: genericstypes,
    returntype: expanded.defn.ret.symbol,
    sym: sym,
    fexpr: expanded
  )
  fexpr.internalScope.addSpecFunc(pd)

  if scope.importscopes.hasKey(name("flori_current_scope")):
    exscope.importScope(name("flori_current_scope"), scope.importscopes[name("flori_current_scope")])
  else:
    exscope.importScope(name("flori_current_scope"), scope.top)
  exscope.rootPass(expanded.defn.body)
  if expanded.defn.body.len != 0:
    if not expanded.defn.body[^1].typ.spec(expanded.defn.ret.symbol):
      expanded.defn.body[^1].error("function expect $# return type, actually $#" % [$expanded.defn.ret.symbol, $expanded.defn.body[^1].typ])
  scope.ctx.globaltoplevels.add(expanded)

  return fsym

proc expandMacrofn*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr, argtypes: seq[Symbol]): FExpr =
  for i, arg in fexpr.defn.args:
    arg[1].assert(arg[1].kind == fexprSymbol)
    if not arg[1].symbol.applyInstance(argtypes[i]):
      arg.error("argtype not mat5ch: $#, $#" % [$arg[1].symbol.instance.get, $argtypes[i]])
  defer:
    for g in fexpr.defn.generics:
      g.symbol.instance = none(Symbol)
  result = expandDefn(rootPass, scope, fexpr, argtypes)
  let mp = MacroProc(importname: codegenMangling(result.symbol, result.symbol.fexpr.defn.generics.mapIt(it.symbol), result.symbol.fexpr.defn.args.mapIt(it[1].symbol)) & "_macro")
  result.symbol.macroproc = mp
  result.symbol.kind = symbolMacro
  
  scope.ctx.macroprocs.add(mp)
  scope.ctx.reloadMacroLibrary(scope.top)