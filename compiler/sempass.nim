
import fexpr_core
import newpassmacro, typepass, macropass, expandpass, effectpass, scopeout, converterpass

import options
import strutils, sequtils
import tables

proc internalPass*(scope: Scope, fexpr: var FExpr): bool =
  if fexpr.isEvaluated:
    return
  fexpr.internalScope = scope
  
  case fexpr.kind
  of fexprSeq:
    if fexpr.len == 0:
      return false
    
    let fnident = fexpr[0]
    let internalopt = scope.getFunc(procname(name(fnident), @[]))
    if internalopt.isSome and internalopt.get.isInternal:
      if scope.getDecl(name("Void")).isSome:
        scope.resolveByVoid(fexpr)
      fexpr.isEvaluated = true
      internalopt.get.internalproc(rootPass, scope, fexpr)
      return true
    elif internalopt.isSome and internalopt.get.isSyntax:
      scope.expandBy(fexpr.span):
        var expanded = internalopt.get.macroproc.call(fexpr)
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
    let opt = scope.getDecl(name(fexpr))
    if opt.isSome:
      fexpr = fsymbol(fexpr.span, opt.get)
      if opt.get.fexpr.hasInternalScope:
        fexpr.internalScope = opt.get.fexpr.internalScope
      return true
    else:
      let fnopt = scope.getFnDecl(name(fexpr))
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
      let sym = scope.symbol(name("Fn"), symbolFuncType, fexpr)
      sym.argtypes = @[]
      for arg in fexpr.symbol.fexpr.defn.args:
        sym.argtypes.add(arg[1].symbol)
      sym.rettype = fexpr.symbol.fexpr.defn.ret.symbol
      fexpr.typ = sym
    elif fexpr.symbol.fexpr.hasTyp:
      fexpr.typ = fexpr.symbol.fexpr.typ
      if fexpr.typ.instance.isSome:
        fexpr.typ = fexpr.typ.instance.get
    elif fexpr.symbol.instance.isSome and fexpr.symbol.instance.get.kind == symbolIntLit:
      fexpr.symbol = fexpr.symbol.instance.get
      let opt = scope.getDecl(name("IntLit"))
      if opt.isNone:
        fexpr.error("undeclared IntLit type, please import prelude.")
      fexpr.typ = opt.get
    return true
  of fexprIntLit:
    let opt = scope.getDecl(name("IntLit"))
    if opt.isNone:
      fexpr.error("undeclared IntLit type, please import prelude.")
    fexpr.typ = opt.get
    return true
  of fexprFloatLit:
    let opt = scope.getDecl(name("FloatLit"))
    if opt.isNone:
      fexpr.error("undeclared FloatLit type, please import prelude.")
    fexpr.typ = opt.get
    return true
  of fexprStrLit:
    let opt = scope.getDecl(name("StrLit"))
    if opt.isNone:
      fexpr.error("undeclared StrLit type, please import prelude.")
    fexpr.typ = opt.get
    return true
  of fexprList:
    if fexpr.len != 0:
      fexpr[0].assert(fexpr[0].hasTyp)
      fexpr.typ = fexpr[0].typ
    else:
      scope.resolveByVoid(fexpr)
    return true
  of fexprBlock:
    if fexpr.len != 0:
      fexpr[^1].assert(fexpr[^1].hasTyp)
      fexpr.typ = fexpr[^1].typ
    else:
      scope.resolveByVoid(fexpr)
    return true
  else:
    return true
  
proc overloadResolve*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  if fexpr.isNormalFuncCall:
    let fnident = fexpr[0]
    checkArgsHastype(fexpr[1])
    let argtypes = fexpr[1].mapIt(it.typ)
    let opt = scope.getFunc(procname(name(fnident), argtypes))
    if opt.isSome:
      fexpr[0] = fsymbol(fexpr[0].span, opt.get.sym)
      fexpr.typ = opt.get.returntype

    return true
  elif fexpr.isGenericsFuncCall:
    let fnident = fexpr[0]
    let generics = fexpr[1]
    checkArgsHastype(fexpr[2])
    let argtypes = fexpr[2].mapIt(it.typ)
    for g in generics.mitems:
      if g.kind == fexprSymbol and g.symbol.isSpecSymbol:
        continue
      g = fsymbol(g.span, scope.semType(g))
    let opt = scope.getFunc(procname(name(fnident), argtypes))
    if opt.isSome:
      fexpr[0] = fsymbol(fexpr[0].span, opt.get.sym)
      fexpr.typ = opt.get.returntype

    return true
  elif fexpr.isInfixFuncCall:
    let fnident = fexpr[0]
    checkArgsHastype(fseq(fexpr.span, @[fexpr[1], fexpr[2]]))
    let argtypes = @[fexpr[1].typ, fexpr[2].typ]
    let opt = scope.getFunc(procname(name(fnident), argtypes))
    if opt.isSome:
      fexpr[0] = fsymbol(fexpr[0].span, opt.get.sym)
      fexpr.typ = opt.get.returntype
      
    return true
  else:
    return true

proc varfnResolve*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)

  if fexpr.kind == fexprSeq and fexpr.len == 2 and fexpr[0].kind != fexprSymbol and fexpr[1].kind == fexprList:
    if fexpr[0].kind == fexprIdent:
      let opt = scope.getDecl(name(fexpr[0]))
      if opt.isNone:
        fexpr.error("undeclared $#($#) function." % [$fexpr[0], fexpr[1].mapIt($it.typ).join(", ")])
    scope.rootPass(fexpr[0])
    if fexpr[0].typ.kind != symbolFuncType:
      fexpr[0].error("$# is not callable." % $fexpr[0])
    fexpr.typ = fexpr[0].typ.rettype

  return true

#
# ownership pass
#

proc moveEffectPass*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  echo "unimplemented: moveEffectPass"
  return true

proc explicitDestruct*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  echo "unimplemented: explicitDestruct"
  return true

proc finalPass*(scope: Scope, fexpr: var FExpr): bool =
  fexpr.isEvaluated = true
  return true
    
definePass processSemPass, rootPass, (Scope, var FExpr):
  internalPass
  expandMacro
  toplevelPass
  symbolResolve
  typeInfer
  overloadResolve
  converterPass
  varfnResolve
  applyInstancePass
  expandInlinePass
  applyInstancePass
  expandDefnPass
  expandDeftypePass
  inferFnEffectPass
  # earlySetDestruct
  # expandDestructorPass
  # moveEffectPass
  # explicitDestruct
  finalPass
