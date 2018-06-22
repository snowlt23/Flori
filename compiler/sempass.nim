
import fcore
import passmacro, typepass, expandpass

import options
import strutils, sequtils
import tables

proc internalPass*(scope: FScope, fexpr: var FExpr): bool =
  # echo fexpr
  case fexpr.kind
  of fexprSeq:
    if fexpr.len == 0:
      return false

    let fnident = $fexpr[0]
    let internalopt = scope.getFunc(procname(fnident, @[]))
    if internalopt.isSome and internalopt.get.internalproc.isSome:
      (internalopt.get.internalproc.get)(scope, fexpr)
      return false
    # elif internalopt.isSome and internalopt.get.isSyntax:
    #   scope.expandBy(fexpr.span):
    #     var expanded = internalopt.get.pd.macroproc.call(fexpr)
    #     scope.rootPass(expanded)
    #     fexpr = expanded
    #   return false
    else:
      return true
  else:
    return true

proc toplevelPass*(scope: FScope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  case fexpr.kind
  of fexprArray, fexprList:
    for son in fexpr.mitems:
      scope.rootPass(son)
    return true
  of fexprBlock:
    for son in fexpr.mitems:
      scope.rootPass(son)
    return true
  of fexprSeq:
    for i in 1..<fexpr.len:
      scope.rootPass(fexpr[i])
    return true
  else:
    return true

proc symbolResolve*(scope: FScope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  case fexpr.kind
  of fexprIdent:
    let opt = scope.getDecl(name(fexpr))
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
      fexpr.typ = fexpr.symbol.fexpr.typ
    elif fexpr.symbol.kind in {symbolType, symbolTypeGenerics}:
      fexpr.typ = some(fexpr.symbol)
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
  of fexprList:
    if fexpr.len != 0:
      fexpr[0].assert(fexpr[0].typ.isSome)
      fexpr.typ = fexpr[0].typ
    else:
      resolveByVoid(fexpr)
    return true
  of fexprBlock:
    if fexpr.len != 0:
      fexpr[^1].assert(fexpr[^1].typ.isSome)
      fexpr.typ = fexpr[^1].typ
    else:
      resolveByVoid(fexpr)
    return true
  else:
    return true

proc overloadResolve*(scope: FScope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  if fexpr.isNormalFuncCall:
    checkArgsHastype(fexpr[1])
    let fnident = $fexpr[0]
    let argtypes = fexpr[1].mapIt(it.typ.get)
    let opt = scope.getFunc(procname(fnident, argtypes))
    if opt.isSome:
      fexpr[0] = fsymbol(fexpr[0].span, opt.get.sym)
      fexpr.typ = some(opt.get.returntype)
    else:
      fexpr.error("undeclared $#($#) function." % [fnident, argtypes.mapIt($it).join(", ")])
    return true
  elif fexpr.isGenericsFuncCall:
    checkArgsHastype(fexpr[2])
    let fnident = $fexpr[0]
    let generics = fexpr[1]
    let argtypes = fexpr[2].mapIt(it.typ.get)
    for g in generics.mitems:
      if g.kind == fexprSymbol and g.symbol.isSpecSymbol:
        continue
      g = fsymbol(g.span, scope.semType(g, 0))
    let opt = scope.getFunc(procname(fnident, argtypes))
    if opt.isSome:
      fexpr[0] = fsymbol(fexpr[0].span, opt.get.sym)
      fexpr.typ = some(opt.get.returntype)
    else:
      fexpr.error("undeclared $#[$#]($#) function." % [fnident, generics.mapit($it).join(", "), argtypes.mapIt($it).join(", ")])
    return true
  elif fexpr.isInfixFuncCall:
    let fnident = $fexpr[0]
    let args = @[fexpr[1], fexpr[2]]
    checkArgsHastype(args)
    let argtypes = @[fexpr[1].typ.get, fexpr[2].typ.get]
    let opt = scope.getFunc(procname(fnident, argtypes))
    if opt.isSome:
      fexpr[0] = fsymbol(fexpr[0].span, opt.get.sym)
      fexpr.typ = some(opt.get.returntype)
    else:
      fexpr.error("undeclared $#($#) function." % [fnident, argtypes.mapIt($it).join(", ")])
    return true
  else:
    return true

# proc varfnResolve*(scope: Scope, fexpr: var FExpr): bool =
#   thruInternal(fexpr)

#   if fexpr.kind == fexprSeq and fexpr.len == 2 and fexpr[0].kind != fexprSymbol and fexpr[1].kind == fexprList:
#     if fexpr[0].kind == fexprIdent:
#       let opt = scope.getDecl(name(fexpr[0]))
#       if opt.isNone:
#         echo fexpr[1].mapIt(it.typ)
#         fexpr.error("undeclared $#($#) function." % [$fexpr[0], fexpr[1].mapIt($it.typ).join(", ")])
#     scope.rootPass(fexpr[0])
#     if fexpr[0].typ.kind != symbolFuncType and not (fexpr[0].typ.kind == symbolVar and fexpr[0].typ.wrapped.kind == symbolFuncType):
#       echo fexpr[0].typ.kind
#       fexpr[0].error("$# is not callable." % $fexpr[0])
#     if fexpr[0].typ.kind == symbolVar:
#       fexpr[0].typ = fexpr[0].typ.wrapped
#     fexpr.typ = fexpr[0].typ.rettype

#   return true

proc finalPass*(scope: FScope, fexpr: var FExpr): bool =
  return true

definePass processSemPass, rootPass, (FScope, var FExpr):
  internalPass
  # expandMacro
  toplevelPass
  symbolResolve
  typeInfer
  overloadResolve
  # varfnResolve
  # expandInlinePass
  expandDefnPass
  finalPass
