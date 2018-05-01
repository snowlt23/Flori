
import fexpr_core
import passmacro, expand_templates, inlinepass

import options
import strutils, sequtils
import tables

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
        let exsym = expandDefn(rootPass, scope, fnsym.symbol.fexpr, @[], argtypes)
        fexpr[0] = exsym
        fexpr.typ = exsym.symbol.fexpr.defn.ret.symbol
      
    return true
  elif fexpr.isGenericsFuncCall:
    scope.expandBy(fexpr.span):
      let fnsym = fexpr[0]
      let argtypes = fexpr[2].mapIt(it.typ)
      if argtypes.isSpecTypes and fnsym.symbol.fexpr.defn.isGenerics:
        let exsym = expandDefn(rootPass, scope, fnsym.symbol.fexpr, fexpr[1].mapIt(it.symbol), argtypes)
        fexpr[0] = exsym
        fexpr.typ = exsym.symbol.fexpr.defn.ret.symbol

    return true
  elif fexpr.isInfixFuncCall:
    scope.expandBy(fexpr.span):
      let fnsym = fexpr[0]
      let argtypes = @[fexpr[1].typ, fexpr[2].typ]
      if argtypes.isSpecTypes and fnsym.symbol.fexpr.defn.isGenerics:
        let exsym = expandDefn(rootPass, scope, fnsym.symbol.fexpr, @[], argtypes)
        fexpr[0] = exsym
        fexpr.typ = exsym.symbol.fexpr.defn.ret.symbol
      
    return true
  else:
    return true
