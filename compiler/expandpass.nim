
import fexpr_core
import passmacro, expand_templates, inlinepass

import options
import strutils, sequtils
import tables

proc expandDefnPass*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  
  if fexpr.isNormalFuncCall and fexpr[0].kind == fexprSymbol and fexpr[0].symbol.fexpr.hasDefn:
    scope.expandBy(fexpr.span):
      let fnsym = fexpr[0]
      let argtypes = fexpr[1].mapIt(it.metadata.typ)
      if argtypes.isSpecTypes and fnsym.symbol.fexpr.isGenerics:
        let exsym = expandDefn(scope, fnsym.symbol.obj.fexpr, @[], argtypes)
        fexpr[0] = exsym
        fexpr.metadata.typ = exsym.symbol.fexpr.fnReturn.symbol
      
    return true
  elif fexpr.isGenericsFuncCall:
    scope.expandBy(fexpr.span):
      let fnsym = fexpr[0]
      let argtypes = fexpr[2].mapIt(it.metadata.typ)
      if argtypes.isSpecTypes and fnsym.symbol.fexpr.isGenerics:
        let exsym = expandDefn(scope, fnsym.symbol.obj.fexpr, fexpr[1].mapIt(it.symbol), argtypes)
        fexpr[0] = exsym
        fexpr.metadata.typ = exsym.symbol.fexpr.fnReturn.symbol

    return true
  elif fexpr.isInfixFuncCall:
    scope.expandBy(fexpr.span):
      let fnsym = fexpr[0]
      let argtypes = @[fexpr[1].metadata.typ, fexpr[2].metadata.typ]
      if argtypes.isSpecTypes and fnsym.symbol.fexpr.isGenerics:
        let exsym = expandDefn(scope, fnsym.symbol.obj.fexpr, @[], argtypes)
        fexpr[0] = exsym
        fexpr.metadata.typ = exsym.symbol.fexpr.fnReturn.symbol
      
    return true
  else:
    return true
