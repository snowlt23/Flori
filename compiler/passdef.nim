
import parser, types, fexpr, scope, metadata, ctrc, effect
import passmacro, expandpass, passutils

import options
import strutils, sequtils

definePass SemPass

proc internalPass*(scope: Scope, fexpr: var FExpr) {.pass: SemPass.} =
  if fexpr.hasEvaluated:
    return
  
  case fexpr.kind
  of fexprSeq:
    if fexpr.len == 0:
      scope.nextPass(fexpr)
      return
    
    let fnident = fexpr[0]
    let internalopt = scope.getFunc(procname(name(fnident), @[]))
    if internalopt.isSome and internalopt.get.isInternal:
      fexpr.evaluated = true
      internalopt.get.internalproc(rootPassProc, scope, fexpr)
    elif internalopt.isSome and internalopt.get.isMacro:
      var expanded = internalopt.get.macroproc.call(fexpr)
      scope.rootPass(expanded)
      fexpr = expanded
    else:
      scope.nextPass(fexpr)
  else:
    scope.nextPass(fexpr)

proc toplevelPass*(scope: Scope, fexpr: var FExpr) {.pass: SemPass.} =
  # echo fexpr
  case fexpr.kind
  of fexprArray, fexprList, fexprBlock:
    for son in fexpr.mitems:
      scope.rootPass(son)
    scope.nextPass(fexpr)
  of fexprSeq:
    for i in 1..<fexpr.len:
      scope.rootPass(fexpr[i])
    scope.nextPass(fexpr)
  else:
    scope.nextPass(fexpr)
    
proc symbolResolve*(scope: Scope, fexpr: var FExpr) {.pass: SemPass.} =
  case fexpr.kind
  of fexprIdent:
    let opt = scope.getDecl(name(fexpr))
    if opt.isNone:
      fexpr.error("undeclared $# ident." % $fexpr)
    fexpr = fsymbol(fexpr.span, opt.get)
    scope.nextPass(fexpr)
  else:
    scope.nextPass(fexpr)

proc typeInfer*(scope: Scope, fexpr: var FExpr) {.pass: SemPass.} =
  case fexpr.kind
  of fexprIdent:
    fexpr.error("unresolved $# ident by symbolResolve pass." % $fexpr)
  of fexprSymbol:
    if fexpr.symbol.types.isSpecTypes and fexpr.symbol.fexpr.hasTyp:
      fexpr.typ = fexpr.symbol.fexpr.typ
      if fexpr.typ.instance.isSome:
        fexpr.typ = fexpr.typ.instance.get
    scope.nextPass(fexpr)
  of fexprIntLit:
    let opt = scope.getDecl(name("Int"))
    if opt.isNone:
      fexpr.error("undeclared Int type, please import prelude.")
    fexpr.typ = opt.get
    scope.nextPass(fexpr)
  of fexprStrLit:
    let opt = scope.getDecl(name("CString"))
    if opt.isNone:
      fexpr.error("undeclared CString type, please import prelude.")
    fexpr.typ = opt.get
    scope.nextPass(fexpr)
  of fexprList:
    if fexpr.len != 0:
      fexpr.typ = fexpr[0].typ
    scope.nextPass(fexpr)
  of fexprBlock:
    if fexpr.len != 0:
      fexpr.typ = fexpr[^1].typ
    scope.nextPass(fexpr)
  else:
    scope.nextPass(fexpr)

proc overloadResolve*(scope: Scope, fexpr: var FExpr) {.pass: SemPass.} =
  case fexpr.kind
  of fexprSeq:
    if fexpr.len == 2 and fexpr[1].kind == fexprList: # call
      let fnident = fexpr[0]
      let argtypes = fexpr[1].mapIt(it.typ)
      let opt = scope.getFunc(procname(name(fnident), argtypes))
      if opt.isNone:
        fexpr.error("undeclared $#($#) function." % [$fnident, argtypes.mapIt($it).join(", ")])
      fexpr[0] = fsymbol(fexpr[0].span, opt.get.sym)
      fexpr.typ = opt.get.returntype
      if fexpr[0].symbol.fexpr.hasEffect:
        applyEffect(fexpr[1], fexpr[0].symbol.fexpr.effect) # effect
    elif fexpr.len == 3 and fexpr[1].kind == fexprArray and fexpr[2].kind == fexprList: # generics call
      let fnident = fexpr[0]
      let generics = fexpr[1]
      let argtypes = fexpr[2].mapIt(it.typ)
      for g in generics.mitems:
        g = fsymbol(g.span, scope.semTypeExpr(g))
      let opt = scope.getFunc(procname(name(fnident), argtypes))
      if opt.isNone:
        fexpr.error("undeclared $#[$#]($#) function." % [$fnident, generics.mapIt($it).join(", "), argtypes.mapIt($it).join(", ")])
      fexpr[0] = fsymbol(fexpr[0].span, opt.get.sym)
      fexpr.typ = opt.get.returntype
      if fexpr[0].symbol.fexpr.hasEffect:
        applyEffect(fexpr[2], fexpr[0].symbol.fexpr.effect) # effect
    elif fexpr.len == 3 and fexpr[0].kind == fexprInfix: # infix call
      let fnident = fexpr[0]
      let argtypes = @[fexpr[1].typ, fexpr[2].typ]
      let opt = scope.getFunc(procname(name(fnident), argtypes))
      if opt.isNone:
        fexpr.error("undeclared `$#($#) infix function." % [$fnident, argtypes.mapIt($it).join(", ")])
      fexpr[0] = fsymbol(fexpr[0].span, opt.get.sym)
      fexpr.typ = opt.get.returntype
      if fexpr[0].symbol.fexpr.hasEffect:
        applyEffect(fseq(fexpr.span, @[fexpr[1], fexpr[2]]), fexpr[0].symbol.fexpr.effect) # effect
      
    scope.nextPass(fexpr)
  else:
    scope.nextPass(fexpr)

proc expandTemplates*(scope: Scope, fexpr: var FExpr) {.pass: SemPass.} =
  case fexpr.kind
  of fexprSeq:
    if fexpr.len == 2 and fexpr[1].kind == fexprList: # call
      let fnsym = fexpr[0]
      let argtypes = fexpr[1].mapIt(it.typ)
      if argtypes.isSpecTypes and fnsym.symbol.fexpr.defn.generics.len != 0:
        let exsym = expandDefn(rootPassProc, scope, fnsym.symbol.fexpr, argtypes)
        fexpr[0] = exsym
        fexpr.typ = exsym.symbol.fexpr.defn.ret.symbol
    elif fexpr.len == 3 and fexpr[1].kind == fexprArray and fexpr[2].kind == fexprList: # generics call
      let fnsym = fexpr[0]
      let genericstypes = fexpr[1].mapIt(it)
      let argtypes = fexpr[2].mapIt(it.typ)
      if argtypes.isSpecTypes and fnsym.symbol.fexpr.defn.generics.len != 0:
        let defngenerics = fnsym.symbol.fexpr.defn.generics
        for i, gtype in genericstypes:
          defngenerics[i].assert(defngenerics[i].kind == fexprSymbol)
          gtype.assert(gtype.kind == fexprSymbol)
          defngenerics[i].symbol.instance = some(gtype.symbol)
        let exsym = expandDefn(rootPassProc, scope, fnsym.symbol.fexpr, argtypes)
        fexpr[0] = exsym
        fexpr.typ = exsym.symbol.fexpr.defn.ret.symbol
    elif fexpr.len == 3 and fexpr[0].symbol.kind == symbolInfix:
      let fnsym = fexpr[0]
      let argtypes = @[fexpr[1].typ, fexpr[2].typ]
      if argtypes.isSpecTypes and fnsym.symbol.fexpr.defn.generics.len != 0:
        let exsym = expandDefn(rootPassProc, scope, fnsym.symbol.fexpr, argtypes)
        fexpr[0] = exsym
        fexpr.typ = exsym.symbol.fexpr.defn.ret.symbol
      
    scope.nextPass(fexpr)
  else:
    scope.nextPass(fexpr)

proc finalPass*(scope: Scope, fexpr: var FExpr) {.pass: SemPass.} =
  fexpr.evaluated = true
    
instPass SemPass, processSemPass
