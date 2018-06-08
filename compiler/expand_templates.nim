
import fcore, passmacro

import options
import strutils, sequtils
import tables

proc expandDeftype*(scope: FScope, fexpr: var FExpr, argtypes: seq[Symbol]): FExpr

proc replaceIdent*(fexpr: var FExpr, ident: FExpr, by: FExpr) =
  case fexpr.kind
  of fexprContainer:
    for son in fexpr.mitems:
      replaceIdent(son, ident, by)
  of fexprIdent, fexprSymbol:
    if $fexpr == $ident:
      fexpr = by
  else:
    discard

proc applyInstance*(sym: Symbol, instance: Symbol): bool =
  if sym.kind in {symbolVar, symbolRef} and instance.kind in {symbolVar, symbolRef}:
    return sym.wrapped.applyInstance(instance.wrapped):
  elif sym.kind == symbolFuncType and instance.kind == symbolFuncType:
    for i in 0..<sym.argtypes.len:
      if not sym.argtypes[i].applyInstance(instance.argtypes[i]):
        return false
    return sym.rettype.applyInstance(instance.rettype)
  elif instance.kind in {symbolVar, symbolRef}:
    return sym.applyInstance(instance.wrapped)
  elif sym.kind == symbolGenerics:
    if sym.instance.isSome and not sym.instance.get.spec(instance):
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

proc expandSymbol*(scope: FScope, sym: Symbol): Symbol =
  if sym.kind == symbolVar:
    result = varsym(scope.expandSymbol(sym.wrapped))
  elif sym.kind == symbolRef:
    result = refsym(scope.expandSymbol(sym.wrapped))
  elif sym.kind == symbolFuncType:
    result = scope.symbol(fntypeString, symbolFuncType, sym.fexpr)
    var argtypes = newSeq[Symbol]()
    for t in sym.argtypes:
      argtypes.add(scope.expandSymbol(t))
    result.obj.argtypes = iarray(argtypes)
    result.obj.rettype = scope.expandSymbol(sym.rettype)
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

proc checkInstantiate*(generics: FExpr) =
  for g in generics:
    if g.kind != fexprSymbol:
      g.error("cannot instantiate $#." % $g)

proc expandGenerics*(generics: FExpr) =
  for g in generics.mitems:
    if g.kind != fexprSymbol:
      g.error("cannot instantiate $#." % $g)
    if g.symbol.kind == symbolGenerics:
      if g.symbol.instance.isNone:
        g.error("cannot instantiate $#." % $g)
      g = fsymbol(g.span, g.symbol.instance.get)
proc expandArgtypes*(scope: FScope, argdecls: FExpr) =
  for argdecl in argdecls.mitems:
    let tsym = fsymbol(argdecl[1].span, scope.expandSymbol(argdecl[1].symbol))
    argdecl = quoteFExpr(argdecl.span, "`embed `embed", [argdecl[0], tsym])

proc expandTemplate*(scope: FScope, fexpr: FExpr, generics: FExpr, genericstypes: seq[Symbol]): FExpr =
  result = fexpr.copy
  for i, g in generics:
    replaceIdent(result, g, fsymbol(g.span, genericstypes[i]))
      
proc expandDeftype*(scope: FScope, fexpr: var FExpr, argtypes: seq[Symbol]): FExpr =
  let parsed = parseDeftype(fexpr)
  
  let manglingname = genManglingName($fexpr[parsed.name], argtypes, @[])
  let specopt = fexpr.scope.get.getDecl(manglingname)
  if specopt.isSome:
    let fsym = fsymbol(fexpr.span, specopt.get)
    return fsym

  var expanded = fexpr.scope.get.expandTemplate(fexpr, fexpr[parsed.generics.get], argtypes)
  let tsym = expanded.scope.get.symbol(expanded[parsed.name].symbol.name, symbolTypeGenerics, expanded)
  tsym.obj.types = iarray(argtypes)
  let fsym = fsymbol(expanded.span, tsym)
  expanded[parsed.name] = fsym

  expanded.scope.get.addDecl(istring(manglingname), tsym)

  let exscope = expanded.scope.get.extendFScope()
  exscope.rootPass(expanded)
  
  # if parsed.body.isSome:
  #   for field in expanded[parsed.body.get]:
  #     let fieldtypesym = typescope.semType(field, 1)
  #     field[1] = fsymbol(field[1].span, fieldtypesym)
  
  return fsym
    
proc expandDefn*(scope: FScope, fexpr: var FExpr, genericstypes: seq[Symbol], argtypes: seq[Symbol]): FExpr =
  var parsed = parseDefn(fexpr)
  let generics = fexpr[parsed.generics.get].copy
  for i, gtype in genericstypes:
    if not applyInstance(generics[i].symbol, gtype.symcopy):
      generics[i].error("generics not match: $#, $#" % [$generics[i].symbol.instance.get, $gtype])
  for i, argtype in argtypes:
    if not applyInstance(fexpr[parsed.argdecls][i][1].symbol, argtype):
      fexpr[parsed.argdecls][i][1].error("argtype not match: $#, $#" % [$fexpr[parsed.argdecls][i][1].symbol.instance.get, $argtype])
  generics.expandGenerics()

  var expandseq = newSeq[FExpr]()
  expandseq.add(fexpr[0])
  expandseq.add(fexpr[parsed.name]) # name
  if parsed.generics.isSome: # generics
    expandseq.add(fexpr[parsed.generics.get])
  expandseq.add(fexpr[parsed.argdecls]) # argdecls
  # if parsed.retpre.isSome: # ret
  #   expandseq.add(fexpr[parsed.retpre.get])
  if parsed.ret.isSome: # ret
    expandseq.add(fexpr[parsed.ret.get])
  # if parsed.retgen.isSome: # ret
  #   expandseq.add(fexpr[parsed.retgen.get])
  if parsed.pragma.isSome: # pragma
    expandseq.add(fexpr[parsed.pragma.get-1])
    expandseq.add(fexpr[parsed.pragma.get])
  # body
  if parsed.body.isSome: # pragma
    expandseq.add(fexpr[parsed.body.get])
  
  var expanded = fseq(fexpr.span, iarray(expandseq))
  parsed = parseDefn(expanded)
  expanded.scope = fexpr.scope
  expanded.src = fexpr.src
  expanded.typ = fexpr.typ
  expanded[parsed.generics.get] = generics
  if parsed.body.isSome:
    expanded[parsed.body.get] = scope.expandTemplate(expanded[parsed.body.get], fexpr[parsed.generics.get], generics.mapIt(it.symbol))
  scope.expandArgtypes(expanded[parsed.argdecls])
  if parsed.ret.isSome:
    expanded[parsed.ret.get] = fsymbol(expanded[parsed.ret.get].span, scope.expandSymbol(expanded[parsed.ret.get].symbol))
  for g in generics:
    g.symbol.instance = none(Symbol)
    
  let genericstypes = generics.mapIt(it.symbol)
  let specopt = expanded.scope.get.getSpecFunc(procname($expanded[parsed.name], argtypes, genericstypes))
  if specopt.isSome:
    let fsym = fsymbol(expanded.span, specopt.get.sym)
    return fsym
  
  let exscope = expanded.scope.get.extendFScope()
  exscope.rootPass(expanded)
  
  return expanded[parsed.name]

# proc expandMacrofn*(scope: FScope, fexpr: var FExpr, argtypes: seq[Symbol]): FExpr =
#   result = expandDefn(scope, fexpr, @[], argtypes)
#   let mp = MacroProc(importname: codegenMangling(result.symbol, result.symbol.fexpr.defn.generics.mapIt(it.symbol), result.symbol.fexpr.defn.args.mapIt(it[1].symbol)) & "_macro")
#   result.symbol.macroproc = mp
#   result.symbol.kind = symbolMacro
  
#   scope.ctx.macroprocs.add(mp)
#   scope.ctx.reloadMacroLibrary(scope.top)
