
import fexpr_core
import passutils, ccodegen, compileutils
import passmacro

import options
import strutils, sequtils
import tables

proc expandDeftype*(scope: Scope, fexpr: var FExpr, argtypes: seq[Symbol]): FExpr

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
  elif sym.kind == symbolFuncType:
    result = scope.symbol(istring("Fn"), symbolFuncType, sym.fexpr)
    result.argtypes = iarray[Symbol](sym.argtypes.len)
    for i, t in sym.argtypes:
      result.argtypes[i] = scope.expandSymbol(t)
    result.rettype = scope.expandSymbol(sym.rettype)
  elif sym.kind == symbolGenerics:
    if sym.instance.isNone:
      sym.fexpr.error("cannot instantiate $#." % $sym)
    result = sym.instance.get
  elif sym.kind == symbolTypeGenerics:
    var extypes = newSeq[Symbol]()
    for t in sym.types:
      extypes.add(scope.expandSymbol(t))
    result = scope.expandDeftype(sym.obj.fexpr, extypes).symbol
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
      g.symbol = g.symbol.instance.get
proc expandArgtypes*(scope: Scope, argdecls: FExpr) =
  for argdecl in argdecls:
    argdecl[1] = fsymbol(argdecl[1].span, scope.expandSymbol(argdecl[1].symbol))

proc expandTemplate*(scope: Scope, fexpr: FExpr, generics: FExpr, genericstypes: seq[Symbol]): FExpr =
  result = fexpr.copy
  for i, g in generics:
    replaceIdent(result, g, fsymbol(g.span, genericstypes[i]))
      
proc expandDeftype*(scope: Scope, fexpr: var FExpr, argtypes: seq[Symbol]): FExpr =
  let manglingname = istring(codegenMangling(fexpr.fnName.symbol, argtypes, @[], internal = true))
  let specopt = fexpr.metadata.scope.top.getDecl($manglingname)
  if specopt.isSome:
    let fsym = fsymbol(fexpr.span, specopt.get)
    return fsym

  var expanded = fexpr.metadata.scope.expandTemplate(fexpr, fexpr.fnGenerics, argtypes)
  let tsym = expanded.metadata.scope.symbol(expanded.fnName.symbol.name, symbolTypeGenerics, expanded)
  tsym.types = iarray(argtypes)
  let fsym = fsymbol(expanded.span, tsym)
  expanded[1] = fsym

  discard expanded.metadata.scope.top.addDecl(manglingname, tsym)

  let exscope = expanded.metadata.scope.extendScope()
  let scopeopt = scope.imports.find("flori_current_scope")
  if scopeopt.isSome:
    exscope.importScope(istring("flori_current_scope"), scopeopt.get)
  else:
    exscope.importScope(istring("flori_current_scope"), scope.top)
  exscope.rootPass(expanded)
  gCtx.globaltoplevels.add(expanded)
  
  return fsym
    
proc expandDefn*(scope: Scope, fexpr: var FExpr, genericstypes: seq[Symbol], argtypes: seq[Symbol]): FExpr =
  let generics = fexpr.fnGenerics.copy
  for i, gtype in genericstypes:
    if not applyInstance(generics[i].symbol, gtype):
      generics[i].error("generics not match: $#, $#" % [$generics[i].symbol.instance.get, $gtype])
  for i, argtype in argtypes:
    let args = fexpr.fnArguments
    if not applyInstance(args[i][1].symbol, argtype):
      args[i][1].error("argtype not match: $#, $#" % [$args[i][1].symbol.instance.get, $argtype])
  generics.expandGenerics()
  
  var expanded = scope.expandTemplate(fexpr, fexpr.fnGenerics, generics.mapIt(it.symbol))
  scope.expandArgtypes(expanded.fnArguments)
  expanded.fnReturn.symbol = scope.expandSymbol(expanded.fnReturn.symbol)
  for g in fexpr.fnGenerics:
    g.symbol.instance = none(Symbol)
    
  let genericstypes = generics.mapIt(it.symbol)
  let specopt = expanded.metadata.scope.getSpecFunc(procname($expanded.fnName, argtypes, genericstypes))
  if specopt.isSome:
    let fsym = fsymbol(expanded.span, specopt.get.sym)
    return fsym
  
  let exscope = expanded.metadata.scope.extendScope()
  let scopeopt = scope.imports.find("flori_current_scope")
  if scopeopt.isSome:
    exscope.importScope(istring("flori_current_scope"), scopeopt.get)
  else:
    exscope.importScope(istring("flori_current_scope"), scope.top)
  exscope.rootPass(expanded)
  # exscope.importscopes.del(name("flori_current_scope"))
  expanded.assert(expanded.fnName.kind == fexprSymbol)

  gCtx.globaltoplevels.add(expanded)
  
  return expanded.fnName

proc expandMacrofn*(scope: Scope, fexpr: var FExpr, argtypes: seq[Symbol]): FExpr =
  result = expandDefn(scope, fexpr, @[], argtypes)
  let mp = MacroProc(importname: istring(codegenMangling(result.symbol, result.symbol.fexpr.fnGenerics.mapIt(it.symbol), result.symbol.fexpr.fnArguments.mapIt(it[1].symbol)) & "_macro"))
  result.symbol.macroproc = mp
  result.symbol.kind = symbolMacro
  
  gCtx.macroprocs.add(mp)
  gCtx.reloadMacroLibrary(scope.top)
