
import fexpr_core
import newpassmacro, expandutils, inlinepass

import options
import strutils, sequtils
import tables

proc releaseInstance*(scope: Scope, fexpr: var FExpr) =
  for g in fexpr[0].symbol.fexpr.defn.generics:
    g.symbol.instance = none(Symbol)
  for arg in fexpr[0].symbol.fexpr.defn.args:
    arg[1].symbol.instance = none(Symbol)
      
proc applyInstancePass*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  if fexpr.isNormalFuncCall and fexpr[0].symbol.fexpr.hasDefn:
    scope.releaseInstance(fexpr)
    for i, arg in fexpr[1]:
      if not fexpr[0].symbol.fexpr.defn.args[i][1].symbol.applyInstance(arg.typ):
        arg.error("argtype not match: $#, $#" % [$fexpr[0].symbol.fexpr.defn.args[i][1].symbol.instance.get, $arg.typ])
  elif fexpr.isGenericsFuncCall:
    scope.releaseInstance(fexpr)
    for i, ginst in fexpr[1]:
      if not fexpr[0].symbol.fexpr.defn.generics[i].symbol.applyInstance(ginst.symbol):
        ginst.error("generics type not match: $#, $#" % [$fexpr[0].symbol.fexpr.defn.generics[i].symbol.instance.get, $ginst.symbol])
    for i, arg in fexpr[2]:
      if not fexpr[0].symbol.fexpr.defn.args[i][1].symbol.applyInstance(arg.typ):
        arg.error("argtype not match: $#, $#" % [$fexpr[0].symbol.fexpr.defn.args[i][1].symbol.instance.get, $arg.typ])
  elif fexpr.isInfixFuncCall:
    scope.releaseInstance(fexpr)
    if not fexpr[0].symbol.fexpr.defn.args[0][1].symbol.applyInstance(fexpr[1].typ):
      fexpr[1].error("argtype not match: $#, $#" % [$fexpr[0].symbol.fexpr.defn.args[0][1].symbol.instance.get, $fexpr[1].typ])
    if not fexpr[0].symbol.fexpr.defn.args[1][1].symbol.applyInstance(fexpr[2].typ):
      fexpr[2].error("argtype not match: $#, $#" % [$fexpr[0].symbol.fexpr.defn.args[1][1].symbol.instance.get, $fexpr[2].typ])

  return true

proc expandInlinePass*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  if fexpr.isFuncCall:
    if not fexpr[0].symbol.fexpr.hasDefn:
      return true
    scope.expandBy(fexpr.span):
      if fexpr[0].symbol.fexpr.hasInternalPragma and fexpr[0].symbol.fexpr.internalPragma.inline:
        let inlinescope = fexpr[0].symbol.fexpr.internalScope
        scope.importScope(name("inline_scope_" & $inlinescope.name), inlinescope)
        for name, s in inlinescope.importscopes:
          scope.importScope(name, s)
        scope.expandInlineFunc(fexpr)
        scope.rootPass(fexpr)
  return true

proc expandDefnPass*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  if fexpr.isNormalFuncCall and fexpr[0].symbol.fexpr.hasDefn:
    scope.expandBy(fexpr.span):
      let fnsym = fexpr[0]
      let argtypes = fexpr[1].mapIt(it.typ)
      if argtypes.isSpecTypes and fnsym.symbol.fexpr.defn.isGenerics:
        let exsym = expandDefn(rootPass, scope, fnsym.symbol.fexpr, argtypes)
        fexpr[0] = exsym
        fexpr.typ = exsym.symbol.fexpr.defn.ret.symbol
      
    return true
  elif fexpr.isGenericsFuncCall:
    scope.expandBy(fexpr.span):
      let fnsym = fexpr[0]
      let genericstypes = fexpr[1]
      let argtypes = fexpr[2].mapIt(it.typ)
      if argtypes.isSpecTypes and fnsym.symbol.fexpr.defn.isGenerics:
        let newfexpr = fnsym.symbol.fexpr.copy
        for i, gtype in genericstypes:
          gtype.assert(gtype.kind == fexprSymbol)
          newfexpr.defn.generics[i].symbol.instance = some(gtype.symbol)
        let exsym = expandDefn(rootPass, scope, newfexpr, argtypes)
        fexpr[0] = exsym
        fexpr.typ = exsym.symbol.fexpr.defn.ret.symbol

    return true
  elif fexpr.isInfixFuncCall:
    scope.expandBy(fexpr.span):
      let fnsym = fexpr[0]
      let argtypes = @[fexpr[1].typ, fexpr[2].typ]
      if argtypes.isSpecTypes and fnsym.symbol.fexpr.defn.isGenerics:
        let exsym = expandDefn(rootPass, scope, fnsym.symbol.fexpr, argtypes)
        fexpr[0] = exsym
        fexpr.typ = exsym.symbol.fexpr.defn.ret.symbol
      
    return true
  else:
    return true

proc expandDeftypePass*(scope: Scope, fexpr: var FExpr): bool =
  if fexpr.hasInternalMark and fexpr.internalMark == internalVar and fexpr[2].symbol.kind == symbolTypeGenerics:
    scope.expandBy(fexpr.span):
      let exsym = expandDeftype(scope, fexpr[2].symbol.fexpr, fexpr[2].symbol.types)
      fexpr[2] = exsym
    return true
  elif fexpr.hasInternalMark and fexpr.internalMark == internalDeftype and not fexpr.deftype.isGenerics:
    scope.expandBy(fexpr.span):
      for field in fexpr.deftype.body:
        if field[1].symbol.kind == symbolTypeGenerics:
          let exsym = expandDeftype(scope, field[1].symbol.fexpr, field[1].symbol.types)
          field[1] = exsym
    return true

  thruInternal(fexpr)
      
  if fexpr.hasTyp and fexpr.typ.kind == symbolTypeGenerics:
    scope.expandBy(fexpr.span):
      let exsym = expandDeftype(scope, fexpr.typ.fexpr, fexpr.typ.types)
      fexpr.typ = exsym.symbol
    return true
      
  if fexpr.isFuncCall:
    if not fexpr[0].symbol.fexpr.hasDefn:
      return true
    scope.expandBy(fexpr.span):
      let defn = fexpr[0].symbol.fexpr.defn
      for arg in defn.args.mitems:
        if arg[1].symbol.kind == symbolTypeGenerics:
          arg[1] = expandDeftype(scope, arg[1].symbol.fexpr, arg[1].symbol.types)
      if defn.ret.symbol.kind == symbolTypeGenerics:
        defn.ret = expandDeftype(scope, defn.ret.symbol.fexpr, defn.ret.symbol.types)
    return true

  return true
