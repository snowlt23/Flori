
import fexpr_core, scopeout, marking
import passutils, ccodegen, compileutils

import options
import strutils, sequtils

proc expandDeftype*(scope: Scope, fexpr: var FExpr, argtypes: seq[Symbol]): FExpr

proc applyInstance*(sym: Symbol, instance: Symbol) =
  if sym.kind in {symbolVar, symbolRef} and instance.kind in {symbolVar, symbolRef}:
    sym.wrapped.applyInstance(instance.wrapped)
    if instance.marking.isSome:
      sym.instmarking = some(instance.marking.get)
  elif instance.kind in {symbolVar, symbolRef}:
    sym.applyInstance(instance.wrapped)
  elif sym.kind == symbolGenerics:
    sym.instance = some(instance)
  elif sym.kind == symbolTypeGenerics:
    assert(sym.types.len == instance.types.len)
    sym.instance = some(instance)
    for i in 0..<sym.types.len:
      sym.types[i].applyInstance(instance.types[i])
  else:
    sym.instance = some(instance)

proc expandSymbol*(scope: Scope, sym: Symbol): Symbol =
  if sym.kind == symbolVar:
    result = scope.varsym(scope.expandSymbol(sym.wrapped))
    if sym.instmarking.isSome:
      result.marking = some(sym.instmarking.get.copy)
  elif sym.kind == symbolRef:
    result = scope.refsym(scope.expandSymbol(sym.wrapped))
    if sym.instmarking.isSome:
      result.marking = some(sym.instmarking.get.copy)
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
    if g.symbol.instance.isNone:
      g.error("cannot instantiate $#." % $g)
    g.symbol = g.symbol.instance.get
    if g.symbol.instmarking.isSome:
      g.symbol.marking = some(g.symbol.instmarking.get.copy)
      
proc expandDeftype*(scope: Scope, fexpr: var FExpr, argtypes: seq[Symbol]): FExpr =
  fexpr.assert(fexpr.hasDeftype)

  let expanded = fexpr.copy

  for i, arg in argtypes:
    expanded.deftype.generics[i].symbol.applyInstance(arg)
  defer:
    for g in fexpr.deftype.generics:
      g.symbol.instance = none(Symbol)
  expanded.deftype.generics.expandGenerics()

  let typename = expanded.deftype.name.symbol.name
  let manglingname = genManglingName(typename, argtypes)

  let specopt = fexpr.internalScope.getDecl(manglingname)
  if specopt.isSome:
    let fsym = fsymbol(fexpr.span, specopt.get)
    return fsym
  
  for b in expanded.deftype.body.mitems:
    let extype = scope.expandType(b[1])
    b = fseq(b.span, @[b[0], extype])

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

  for i, arg in expanded.defn.args:
    arg[1].assert(arg[1].kind == fexprSymbol)
    arg[1].symbol.applyInstance(argtypes[i])
  defer:
    for g in fexpr.defn.generics:
      g.symbol.instance = none(Symbol)
    for arg in fexpr.defn.args:
      if arg[1].symbol.kind == symbolRef:
        arg[1].symbol.instmarking = none(Marking)
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
    if extype.symbol.kind == symbolRef and extype.symbol.marking.isSome:
      argcopy.symbol.fexpr.marking = extype.symbol.marking.get
    else:
      argcopy.symbol.fexpr.marking = newMarking(extype.symbol)
    arg[0] = argcopy
    arg[1].symbol = extype.symbol.symcopy
    if extype.symbol.kind == symbolRef and extype.symbol.marking.isSome:
      arg[1].symbol.marking = some(extype.symbol.marking.get.copy)
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

  exscope.importScope(name("flori_current_scope"), scope.top)
  exscope.rootPass(expanded.defn.body)
  scope.ctx.globaltoplevels.add(expanded)
  if not expanded.internalPragma.nodestruct:
    expandDestructor(rootPass, exscope, expanded.defn.body)

  return fsym

proc expandMacrofn*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr, argtypes: seq[Symbol]): FExpr =
  result = expandDefn(rootPass, scope, fexpr, argtypes)
  let mp = MacroProc(importname: codegenMangling(result.symbol, result.symbol.fexpr.defn.generics.mapIt(it.symbol), result.symbol.fexpr.defn.args.mapIt(it[1].symbol)) & "_macro")
  result.symbol.macroproc = mp
  result.symbol.kind = symbolMacro
  
  scope.ctx.macroprocs.add(mp)
  scope.ctx.reloadMacroLibrary(scope.top)
