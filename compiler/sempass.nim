
import fexpr_core, marking
import newpassmacro, typepass, macropass, expandutils, expandpass, effectpass, scopeout, converterpass

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

proc expandMacro*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  if fexpr.isNormalFuncCall:
    scope.expandBy(fexpr.span):
      let args = fexpr[1]
      let pd = matchMacro(rootPass, scope, scope, fexpr[0], args, false)
      if pd.isSome:
        if not pd.get.sym.fexpr.defn.generics.isSpecTypes:
          fexpr[0] = expandMacrofn(rootPass, scope, pd.get.sym.fexpr, scope.getMacroArgs(pd.get, args))
          var expanded = fexpr[0].symbol.macroproc.call(args)
          scope.rootPass(expanded)
          fexpr = expanded
        else:
          var expanded = pd.get.macroproc.call(args)
          scope.rootPass(expanded)
          fexpr = expanded
        return false
      
  if fexpr.kind == fexprSeq:
    scope.expandBy(fexpr.span):
      let args = fexpr[1..^1]
      let pd = matchMacro(rootPass, scope, scope, fexpr[0], args, false)
      if pd.isSome:
        if not pd.get.sym.fexpr.defn.generics.isSpecTypes:
          fexpr[0] = expandMacrofn(rootPass, scope, pd.get.sym.fexpr, scope.getMacroArgs(pd.get, args))
          var expanded = fexpr[0].symbol.macroproc.call(args)
          scope.rootPass(expanded)
          fexpr = expanded
        else:
          var expanded = pd.get.macroproc.call(args)
          scope.rootPass(expanded)
          fexpr = expanded
        return false
    
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
      g = fsymbol(g.span, scope.semTypeExpr(g))
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

proc converterResolve*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  
  if fexpr.isNormalFuncCall and fexpr[0].kind != fexprSymbol:
    let fnident = fexpr[0]
    checkArgsHastype(fexpr[1])
    let opt = scope.findMatchFn(fnident, fexpr[1], fexpr[1].mapIt(0))
    if opt.isSome:
      fexpr = fexpr.span.quoteFExpr("`embed `embed", [fnident, opt.get])
      scope.rootPass(fexpr)
      return false
    else:
      return true
  elif fexpr.isGenericsFuncCall and fexpr[0].kind != fexprSymbol:
    let fnident = fexpr[0]
    checkArgsHastype(fexpr[2])
    let opt = scope.findMatchFn(fnident, fexpr[2], fexpr[2].mapIt(0))
    if opt.isNone:
      fexpr.error("undeclared $#($#) function." % [$fnident, fexpr[2].mapIt($it.typ).join(", ")])
    fexpr = fexpr.span.quoteFExpr("`embed `embed `embed", [fnident, fexpr[1], opt.get])
    scope.rootPass(fexpr)
    return false
  elif fexpr.isInfixFuncCall and fexpr[0].kind != fexprSymbol:
    let fnident = fexpr[0]
    let args = flist(fexpr.span, @[fexpr[1], fexpr[2]])
    checkArgsHastype(args)
    let opt = scope.findMatchFn(fnident, args, args.mapIt(0))
    if opt.isNone:
      fexpr.error("undeclared $#($#, $#) function." % [$fnident, $fexpr[1].typ, $fexpr[2].typ])
    fexpr = fexpr.span.quoteFExpr("`embed `embed `embed", [fnident, opt.get[0], opt.get[1]])
    scope.rootPass(fexpr)
    return false
  else:
    return true

proc varfnResolve*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)

  if fexpr.kind == fexprSeq and fexpr.len == 2 and fexpr[0].kind != fexprSymbol and fexpr[1].kind == fexprList:
    scope.rootPass(fexpr[0])
    if fexpr[0].typ.kind != symbolFuncType:
      fexpr[0].error("value is not callable.")
    fexpr.typ = fexpr[0].typ.rettype

  return true

#
# ownership pass
#
  
proc markingInfer*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  if fexpr.kind == fexprSymbol and fexpr.symbol.fexpr.hasMarking:
    fexpr.marking = fexpr.symbol.fexpr.marking
  elif fexpr.kind == fexprBlock:
    if fexpr.len != 0 and fexpr[^1].hasMarking:
      fexpr.marking = fexpr[^1].marking
  return true

proc moveEffectPass*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  if fexpr.isFuncCall:
    if not fexpr[0].symbol.fexpr.hasDefn:
      return true
    let args = if fexpr.isNormalFuncCall:
                 fexpr[1]
               elif fexpr.isGenericsFuncCall:
                 fexpr[2]
               else:
                 fseq(fexpr.span, @[fexpr[1], fexpr[2]])
    for i, argdef in fexpr[0].symbol.fexpr.defn.args:
      if argdef[1].symbol.kind == symbolMove:
        if args[i].hasMarking:
          if not args[i].marking.owned:
            args[i].error("$# can't move, it's borrow value.")
          args[i].marking.owned = false

  return true

proc explicitDestruct*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  if fexpr.isNormalFuncCall and $fexpr[0] == "destruct" and fexpr[1].len == 1:
    if fexpr[1][0].hasMarking:
      returnFrom(fexpr[1][0].marking)
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
  converterResolve
  varfnResolve
  applyInstancePass
  expandInlinePass
  applyInstancePass
  expandDefnPass
  expandDeftypePass
  markingInfer
  inferFnEffectPass
  applyEffectPass
  earlySetDestruct
  moveEffectPass
  expandDestructorPass
  explicitDestruct
  finalPass
