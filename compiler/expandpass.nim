
import fcore
import passmacro, expand_templates

import options
import strutils, sequtils
import tables

# proc expandInlinePass*(scope: Scope, fexpr: var FExpr): bool =
#   thruInternal(fexpr)
#   if fexpr.isFuncCall:
#     fexpr[0].assert(fexpr[0].kind == fexprSymbol)
#     if not fexpr[0].symbol.fexpr.hasDefn:
#       return true
#     scope.expandBy(fexpr.span):
#       if fexpr[0].symbol.fexpr.hasInternalPragma and fexpr[0].symbol.fexpr.internalPragma.inline:
#         let inlinescope = fexpr[0].symbol.fexpr.internalScope
#         scope.importScope(name("inline_scope_" & $inlinescope.name), inlinescope)
#         for name, s in inlinescope.importscopes:
#           scope.importScope(name, s)
#         scope.expandInlineFunc(fexpr)
#         scope.rootPass(fexpr)
#   return true

proc expandDefnPass*(scope: FScope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  
  if fexpr.isNormalFuncCall:
    expandBy(fexpr.span):
      let fnsym = fexpr[0]
      let argtypes = fexpr[1].mapIt(it.typ.get)
      let parsed = parseDefn(fnsym.symbol.fexpr)
      if argtypes.isSpecTypes and fnsym.symbol.fexpr.isGenerics(parsed):
        let exsym = expandDefn(scope, fnsym.symbol.fexpr, @[], argtypes)
        fexpr[0] = exsym
        let parsed = parseDefn(exsym.symbol.fexpr)
        if parsed.ret.isSome:
          fexpr.typ = some(exsym.symbol.fexpr[parsed.ret.get].symbol)
        else:
          resolveByVoid(fexpr)
      
    return true
  elif fexpr.isGenericsFuncCall:
    expandBy(fexpr.span):
      let fnsym = fexpr[0]
      let argtypes = fexpr[2].mapIt(it.typ.get)
      let parsed = parseDefn(fnsym.symbol.fexpr)
      if argtypes.isSpecTypes and fnsym.symbol.fexpr.isGenerics(parsed):
        let exsym = expandDefn(scope, fnsym.symbol.fexpr, fexpr[1].mapIt(it.symbol), argtypes)
        fexpr[0] = exsym
        let parsed = parseDefn(exsym.symbol.fexpr)
        if parsed.ret.isSome:
          fexpr.typ = some(exsym.symbol.fexpr[parsed.ret.get].symbol)
        else:
          resolveByVoid(fexpr)

    return true
  elif fexpr.isInfixFuncCall:
    expandBy(fexpr.span):
      let fnsym = fexpr[0]
      let argtypes = @[fexpr[1].typ.get, fexpr[2].typ.get]
      let parsed = parseDefn(fnsym.symbol.fexpr)
      if argtypes.isSpecTypes and fnsym.symbol.fexpr.isGenerics(parsed):
        let exsym = expandDefn(scope, fnsym.symbol.fexpr, @[], argtypes)
        fexpr[0] = exsym
        let parsed = parseDefn(exsym.symbol.fexpr)
        if parsed.ret.isSome:
          fexpr.typ = some(exsym.symbol.fexpr[parsed.ret.get].symbol)
        else:
          resolveByVoid(fexpr)
      
    return true
  else:
    return true
