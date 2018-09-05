
import fexpr_core
import passmacro, typepass, macropass, expandpass

import options
import strutils, sequtils
import tables

proc internalPass*(scope: Scope, fexpr: var FExpr): bool =
  # echo fexpr
  if fexpr.metadata.isEvaluated:
    return
  fexpr.metadata.scope = scope
  
  case fexpr.kind
  of fexprSeq:
    if fexpr.len == 0:
      return false
    
    let fnident = fexpr[0]
    if fnident.kind notin fexprAllNames:
      return true
    let internalopt = scope.getFunc(procname($fnident, @[]))
    if internalopt.isSome and internalopt.get.pd.internalproc.isSome:
      if scope.getDecl("Void").isSome:
        scope.resolveByVoid(fexpr)
      fexpr.metadata.isEvaluated = true
      (internalopt.get.pd.internalproc.get)(scope, fexpr)
      return false
    elif internalopt.isSome and internalopt.get.pd.isSyntax:
      scope.expandBy(fexpr.span):
        var expanded = (internalopt.get.pd.macroproc.call)(fexpr)
        scope.rootPass(expanded)
        fexpr = expanded
      return false
    else:
      return true
  else:
    return true

proc toplevelPass*(scope: Scope, fexpr: var FExpr): bool =
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
    
proc symbolResolve*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  case fexpr.kind
  of fexprIdent:
    let opt = scope.getDecl($fexpr)
    if opt.isSome:
      fexpr = fsymbol(fexpr.span, opt.get)
      fexpr.metadata.scope = opt.get.fexpr.metadata.scope
      return true
    else:
      let fnopt = scope.getFnDecl($fexpr)
      if fnopt.isNone:
        fexpr.error("undeclared $# ident." % $fexpr)
      fexpr = fsymbol(fexpr.span, fnopt.get)
      return true
  else:
    return true

proc typeInfer*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  case fexpr.kind
  of fexprIdent:
    fexpr.error("unresolved $# ident by symbolResolve pass." % $fexpr)
  of fexprSymbol:
    if fexpr.symbol.kind == symbolFunc:
      let sym = scope.symbol(istring("Fn"), symbolFuncType, fexpr)
      var argtypes = newSeq[Symbol]()
      for arg in fexpr.symbol.fexpr.fnArguments:
        argtypes.add(arg[1].symbol)
      sym.argtypes = iarray(argtypes)
      sym.rettype = fexpr.symbol.fexpr.fnReturn.symbol
      fexpr.metadata.typ = sym
    elif fexpr.symbol.fexpr.hasTyp:
      fexpr.metadata.typ = fexpr.symbol.fexpr.metadata.typ
      # if fexpr.typ.instance.isSome:
      #   fexpr.typ = fexpr.typ.instance.get
    elif fexpr.symbol.instance.isSome and fexpr.symbol.instance.get.kind == symbolIntLit:
      fexpr.symbol = fexpr.symbol.instance.get
      let opt = scope.getDecl("IntLit")
      if opt.isNone:
        fexpr.error("undeclared IntLit type, please import prelude.")
      fexpr.metadata.typ = opt.get
    elif fexpr.symbol.kind == symbolIntLit:
      let opt = scope.getDecl("IntLit")
      if opt.isNone:
        fexpr.error("undeclared IntLit type, please import prelude.")
      fexpr.metadata.typ = opt.get
    return true
  of fexprIntLit:
    let opt = scope.getDecl("IntLit")
    if opt.isNone:
      fexpr.error("undeclared IntLit type, please import prelude.")
    fexpr.metadata.typ = opt.get
    return true
  of fexprFloatLit:
    let opt = scope.getDecl("FloatLit")
    if opt.isNone:
      fexpr.error("undeclared FloatLit type, please import prelude.")
    fexpr.metadata.typ = opt.get
    return true
  of fexprStrLit:
    let opt = scope.getDecl("StrLit")
    if opt.isNone:
      fexpr.error("undeclared StrLit type, please import prelude.")
    fexpr.metadata.typ = opt.get
    return true
  of fexprList:
    if fexpr.len != 0:
      fexpr[0].assert(fexpr[0].hasTyp)
      fexpr.metadata.typ = fexpr[0].metadata.typ
    else:
      scope.resolveByVoid(fexpr)
    return true
  of fexprArray:
    if fexpr.len != 0:
      fexpr[0].assert(fexpr[0].hasTyp)
      fexpr.metadata.typ = fexpr[0].metadata.typ
    else:
      scope.resolveByVoid(fexpr)
    return true
  of fexprBlock:
    if fexpr.len != 0:
      fexpr[^1].assert(fexpr[^1].hasTyp)
      fexpr.metadata.typ = fexpr[^1].metadata.typ
    else:
      scope.resolveByVoid(fexpr)
    return true
  else:
    return true
  
proc overloadResolve*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  if fexpr.isNormalFuncCall:
    let fnident = fexpr[0]
    if fnident.kind notin fexprAllNames:
      return true
    checkArgsHastype(fexpr[1])
    let argtypes = fexpr[1].mapIt(it.metadata.typ)
    let opt = scope.getFunc(procname(name(fnident), argtypes))
    if opt.isSome:
      fexpr[0] = fsymbol(fexpr[0].span, opt.get.pd.sym)
      fexpr.metadata.typ = opt.get.pd.returntype
      fexpr[1] = genConvertedCall(fexpr[1], opt.get.matches)
      scope.rootPass(fexpr[1])
    else:
      fexpr.error("undeclared $#($#) function" % [$fnident, argtypes.mapIt($it).join(", ")])

    return true
  elif fexpr.isGenericsFuncCall:
    let fnident = fexpr[0]
    let generics = fexpr[1]
    checkArgsHastype(fexpr[2])
    let argtypes = fexpr[2].mapIt(it.metadata.typ)
    for g in generics.mitems:
      if g.kind == fexprSymbol and g.symbol.isSpecSymbol:
        continue
      g = fsymbol(g.span, scope.semType(g))
    let opt = scope.getFunc(procname(name(fnident), argtypes))
    if opt.isSome:
      fexpr[0] = fsymbol(fexpr[0].span, opt.get.pd.sym)
      fexpr.metadata.typ = opt.get.pd.returntype
      fexpr[2] = genConvertedCall(fexpr[2], opt.get.matches)
      scope.rootPass(fexpr[2])
    else:
      fexpr.error("undeclared $#$#($#) function" % [$fnident, $generics, argtypes.mapIt($it).join(", ")])

    return true
  elif fexpr.isInfixFuncCall:
    let fnident = fexpr[0]
    let args = flist(fexpr.span, @[fexpr[1], fexpr[2]])
    checkArgsHastype(args)
    let argtypes = @[fexpr[1].metadata.typ, fexpr[2].metadata.typ]
    let opt = scope.getFunc(procname(name(fnident), argtypes))
    if opt.isSome:
      fexpr[0] = fsymbol(fexpr[0].span, opt.get.pd.sym)
      fexpr.metadata.typ = opt.get.pd.returntype
      let converted = genConvertedCall(args, opt.get.matches)
      fexpr[1] = converted[0]
      fexpr[2] = converted[1]
      scope.rootPass(fexpr[1])
      scope.rootPass(fexpr[2])
    else:
      fexpr.error("undeclared $# $# $# function" % [$args[0].metadata.typ, $fnident, $args[1].metadata.typ])
      
    return true
  else:
    return true

proc varfnResolve*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)

  if fexpr.kind == fexprSeq and fexpr.len == 2 and fexpr[0].kind != fexprSymbol and fexpr[1].kind == fexprList and fexpr[0].hasTyp:
    if fexpr[0].kind == fexprIdent:
      let opt = scope.getDecl($fexpr[0])
      if opt.isNone:
        fexpr.error("undeclared $#($#) function." % [$fexpr[0], fexpr[1].mapIt($it.metadata.typ).join(", ")])
    scope.rootPass(fexpr[0])
    if fexpr[0].metadata.typ.kind != symbolFuncType and not (fexpr[0].metadata.typ.kind == symbolVar and fexpr[0].metadata.typ.wrapped.kind == symbolFuncType):
      fexpr[0].error("$# is not callable." % $fexpr[0])
    if fexpr[0].metadata.typ.kind == symbolVar:
      fexpr[0].metadata.typ = fexpr[0].metadata.typ.wrapped
    fexpr.metadata.typ = fexpr[0].metadata.typ.rettype

  return true

proc finalPass*(scope: Scope, fexpr: var FExpr): bool =
  when not defined(release):
    if not fexpr.hasTyp:
      fexpr.error("$# undecided expression type." % $fexpr)
  fexpr.metadata.isEvaluated = true
  return true
    
definePass processSemPass, rootPass, (Scope, var FExpr):
  internalPass
  expandMacro
  toplevelPass
  symbolResolve
  typeInfer
  overloadResolve
  varfnResolve
  expandDefnPass
  finalPass
