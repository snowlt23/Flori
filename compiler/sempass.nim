
import fcore
import passmacro

import options
import strutils, sequtils
import tables

proc internalPass*(scope: FScope, fexpr: var FExpr): bool =
  # echo fexpr
  case fexpr.kind
  of fexprCalls:
    let internalopt = scope.getFunc(procname($fexpr.call, @[]))
    if internalopt.isSome and internalopt.get.internalproc.isSome:
      (internalopt.get.internalproc.get)(scope, fexpr)
      return false
    else:
      return true
  else:
    return true

proc symbolResolve*(scope: FScope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  case fexpr.kind
  of fexprIdent:
    let opt = scope.getDecl($fexpr)
    if opt.isSome:
      fexpr = fsymbol(fexpr.span, opt.get)
    else:
      fexpr.error("undeclared $# ident." % $fexpr)
    return true
  else:
    return true

proc typeInfer*(scope: FScope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  case fexpr.kind
  of fexprIdent:
    fexpr.error("unresolved $# ident by symbolResolve pass." % $fexpr)
  of fexprSymbol:
    if fexpr.symbol.fexpr.typ.isSome:
      fexpr.typ = some(linksym(fexpr.symbol.fexpr.typ.get))
    elif fexpr.symbol.kind in {symbolType, symbolTypeGenerics}:
      fexpr.typ = some(linksym(fexpr.symbol))
    else:
      fexpr.error("$# hasn't type." % $fexpr)
    return true
  of fexprIntLit:
    fexpr.typ = some(intlittypeSymbol)
    return true
  of fexprFloatLit:
    fexpr.typ = some(floatlittypeSymbol)
    return true
  of fexprStrLit:
    fexpr.typ = some(strlittypeSymbol)
    return true
  else:
    return true

proc callResolve*(scope: FScope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  if fexpr.kind in fexprCalls:
    if fexpr.call.kind == fexprIdent:
      let words = scope.getWords($fexpr.call)
      if words.len == 0:
        fexpr.error("undeclared $# function" % $fexpr.call)
      let calltyp = if words.len == 1:
                      words[0].getReturnType()
                    else:
                      unionsym(words.mapIt(it.getReturnType()))
      fexpr.typ = some(calltyp)

  return true

proc finalPass*(scope: FScope, fexpr: var FExpr): bool =
  return true

definePass processSemPass, rootPass, (FScope, var FExpr):
  internalPass
  # expandMacro
  symbolResolve
  typeInfer
  callResolve
  # expandDefnPass
  finalPass
