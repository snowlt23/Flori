
import parser, types, fexpr, scope, metadata, ctrc, effect
import passmacro, expandpass, passutils, typepass

import options
import strutils, sequtils
import tables

definePass SemPass

proc internalPass*(scope: Scope, fexpr: var FExpr) {.pass: SemPass.} =
  if fexpr.hasEvaluated:
    return
  fexpr.internalScope = scope
  
  case fexpr.kind
  of fexprSeq:
    if fexpr.len == 0:
      scope.nextPass(fexpr)
      return
    
    let fnident = fexpr[0]
    let internalopt = scope.getFunc(procname(name(fnident), @[]))
    if internalopt.isSome and internalopt.get.isInternal:
      if scope.getDecl(name("Void")).isSome:
        scope.resolveByVoid(fexpr)
      fexpr.evaluated = true
      internalopt.get.internalproc(rootPassProc, scope, fexpr)
    elif internalopt.isSome and internalopt.get.isSyntax:
      var expanded = internalopt.get.macroproc.call(fexpr)
      scope.rootPass(expanded)
      fexpr = expanded
    else:
      scope.nextPass(fexpr)
  else:
    scope.nextPass(fexpr)

proc isMatchMacro*(rootPass: PassProcType, scope: Scope, args: FExpr, pd: ProcDecl): bool =
  if args.len != pd.argtypes.len: return false
  for i in 0..<args.len:
    if $pd.argtypes[i].name == "FExpr":
      continue
    elif $pd.argtypes[i].name == "FSeq":
      if args[i].kind != fexprSeq:
        return false
    elif $pd.argtypes[i].name == "FArray":
      if args[i].kind != fexprArray:
        return false
    elif $pd.argtypes[i].name == "FList":
      if args[i].kind != fexprList:
        return false
    elif $pd.argtypes[i].name == "FBlock":
      if args[i].kind != fexprBlock:
        return false
    elif $pd.argtypes[i].name == "FIdent":
      if args[i].kind != fexprIdent:
        return false
    elif $pd.argtypes[i].name == "FSymbol":
      if args[i].kind != fexprSymbol:
        return false
    elif $pd.argtypes[i].name == "FIntLit":
      if args[i].kind != fexprIntLit:
        return false
    elif $pd.argtypes[i].name == "FStrLit":
      if args[i].kind != fexprStrLit:
        return false
    elif $pd.argtypes[i].name == "TExpr":
      scope.rootPass(args[i])
      if not args[i].typ.match(pd.argtypes[i].types[0]):
        return false
    else:
      args[i].error("macro argument type should be FExpr or TExpr[T]")
  return true
proc matchMacro*(rootPass: PassProcType, scope: Scope, curscope: Scope, n: FExpr, args: FExpr, issyntax: bool, importscope = true): Option[ProcDecl] =
  if not scope.procdecls.hasKey(name(n)):
    if importscope:
      for scopename, s in scope.importscopes:
        let match = matchMacro(rootPass, s, curscope, n, args, issyntax, importscope = false)
        if match.isSome:
          return match
      return none(ProcDecl)
    else:
      return none(ProcDecl)

  for pd in scope.procdecls[name(n)].decls:
    if (not issyntax) and pd.isMacro and isMatchMacro(rootPass, curscope, args, pd):
      return some(pd)
    elif issyntax and pd.fexpr.internalPragma.isSyntax and isMatchMacro(rootPass, curscope, args, pd):
      return some(pd)

  if importscope:
    for scopename, s in scope.importscopes:
      let match = matchMacro(rootPass, s, curscope, n, args, issyntax, importscope = false)
      if match.isSome:
        return match
    return none(ProcDecl)
  else:
    return none(ProcDecl)

proc isFExprName*(name: Name): bool =
  case $name
  of "FSeq", "FArray", "FList", "FBlock", "FStrLit", "FIntLit", "FIdent", "FSymbol":
    true
  else:
    false
    
proc getMacroArgs*(scope: Scope, pd: ProcDecl, args: FExpr): seq[Symbol] =
  result = @[]
  for i in 0..<args.len:
    if pd.argtypes[i].name.isFExprName:
      let opt = scope.getDecl(pd.argtypes[i].name)
      if opt.isNone:
        args[i].error("undeclared $# type." % $pd.argtypes[i].name)
      result.add(opt.get)
    elif $pd.argtypes[i].name == "TExpr":
      let opt = scope.getDecl(name("TExpr"))
      if opt.isNone:
        args[i].error("undeclared FExpr type.")
      let sym = symcopy(opt.get)
      sym.types.add(args[i].typ)
      result.add(sym)
    
proc expandMacro*(scope: Scope, fexpr: var FExpr) {.pass: SemPass.} =
  if fexpr.isNormalFuncCall:
    let args = fexpr[1]
    let pd = matchMacro(rootPassProc, scope, scope, fexpr[0], args, false)
    if pd.isSome:
      if not pd.get.sym.fexpr.defn.generics.isSpecTypes:
        fexpr[0] = expandMacrofn(rootPassProc, scope, pd.get.sym.fexpr, scope.getMacroArgs(pd.get, args))
        var expanded = fexpr[0].symbol.macroproc.call(args)
        scope.rootPass(expanded)
        fexpr = expanded
      else:
        var expanded = pd.get.macroproc.call(args)
        scope.rootPass(expanded)
        fexpr = expanded
      return
      
  if fexpr.kind == fexprSeq:
    let args = fexpr[1..^1]
    let pd = matchMacro(rootPassProc, scope, scope, fexpr[0], args, false)
    if pd.isSome:
      if not pd.get.sym.fexpr.defn.generics.isSpecTypes:
        fexpr[0] = expandMacrofn(rootPassProc, scope, pd.get.sym.fexpr, scope.getMacroArgs(pd.get, args))
        var expanded = fexpr[0].symbol.macroproc.call(args)
        scope.rootPass(expanded)
        fexpr = expanded
      else:
        var expanded = pd.get.macroproc.call(args)
        scope.rootPass(expanded)
        fexpr = expanded
      return
    
  scope.nextPass(fexpr)

proc toplevelPass*(scope: Scope, fexpr: var FExpr) {.pass: SemPass.} =
  case fexpr.kind
  of fexprArray, fexprList:
    for son in fexpr.mitems:
      scope.rootPass(son)
    scope.nextPass(fexpr)
  of fexprBlock:
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
    if fexpr.symbol.fexpr.hasTyp:
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
  of fexprFloatLit:
    let opt = scope.getDecl(name("Float"))
    if opt.isNone:
      fexpr.error("undeclared Float type, please import prelude.")
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
      fexpr[0].assert(fexpr[0].hasTyp)
      fexpr.typ = fexpr[0].typ
    else:
      scope.resolveByVoid(fexpr)
    scope.nextPass(fexpr)
  of fexprBlock:
    if fexpr.len != 0:
      fexpr[^1].assert(fexpr[^1].hasTyp)
      fexpr.typ = fexpr[^1].typ
    else:
      scope.resolveByVoid(fexpr)
    scope.nextPass(fexpr)
  else:
    scope.nextPass(fexpr)

proc ctrcInfer*(scope: Scope, fexpr: var FExpr) {.pass: SemPass.} =
  if fexpr.kind == fexprSymbol and fexpr.symbol.fexpr.hasCTRC:
    fexpr.ctrc = fexpr.symbol.fexpr.ctrc
  scope.nextPass(fexpr)
    
proc checkArgsHastype*(args: FExpr) =
  for arg in args:
    if not arg.hasTyp:
      arg.error("$# hasn't type." % $arg)
    
proc overloadResolve*(scope: Scope, fexpr: var FExpr) {.pass: SemPass.} =
  if fexpr.isNormalFuncCall:
    let fnident = fexpr[0]
    checkArgsHastype(fexpr[1])
    let argtypes = fexpr[1].mapIt(it.typ)
    let opt = scope.getFunc(procname(name(fnident), argtypes))
    if opt.isNone:
      fexpr.error("undeclared $#($#) function." % [$fnident, argtypes.mapIt($it).join(", ")])
    fexpr[0] = fsymbol(fexpr[0].span, opt.get.sym)
    fexpr.typ = opt.get.returntype

    scope.nextPass(fexpr)
  elif fexpr.isGenericsFuncCall:
    let fnident = fexpr[0]
    let generics = fexpr[1]
    checkArgsHastype(fexpr[2])
    let argtypes = fexpr[2].mapIt(it.typ)
    for g in generics.mitems:
      g = fsymbol(g.span, scope.semTypeExpr(g))
    let opt = scope.getFunc(procname(name(fnident), argtypes))
    if opt.isNone:
      fexpr.error("undeclared $#[$#]($#) function." % [$fnident, generics.mapIt($it).join(", "), argtypes.mapIt($it).join(", ")])
    fexpr[0] = fsymbol(fexpr[0].span, opt.get.sym)
    fexpr.typ = opt.get.returntype

    scope.nextPass(fexpr)
  elif fexpr.isInfixFuncCall:
    let fnident = fexpr[0]
    checkArgsHastype(fseq(fexpr.span, @[fexpr[1], fexpr[2]]))
    let argtypes = @[fexpr[1].typ, fexpr[2].typ]
    let opt = scope.getFunc(procname(name(fnident), argtypes))
    if opt.isNone:
      fexpr.error("undeclared `$#($#) infix function." % [$fnident, argtypes.mapIt($it).join(", ")])
    fexpr[0] = fsymbol(fexpr[0].span, opt.get.sym)
    fexpr.typ = opt.get.returntype
      
    scope.nextPass(fexpr)
  else:
    scope.nextPass(fexpr)

proc expandTemplates*(scope: Scope, fexpr: var FExpr) {.pass: SemPass.} =
  if fexpr.isNormalFuncCall:
    let fnsym = fexpr[0]
    let argtypes = fexpr[1].mapIt(it.typ)
    if argtypes.isSpecTypes and fnsym.symbol.fexpr.defn.generics.len != 0:
      let exsym = expandDefn(rootPassProc, scope, fnsym.symbol.fexpr, argtypes)
      fexpr[0] = exsym
      fexpr.typ = exsym.symbol.fexpr.defn.ret.symbol
      
    scope.nextPass(fexpr)
  elif fexpr.isGenericsFuncCall:
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

    scope.nextPass(fexpr)
  elif fexpr.isInfixFuncCall:
    let fnsym = fexpr[0]
    let argtypes = @[fexpr[1].typ, fexpr[2].typ]
    if argtypes.isSpecTypes and fnsym.symbol.fexpr.defn.generics.len != 0:
      let exsym = expandDefn(rootPassProc, scope, fnsym.symbol.fexpr, argtypes)
      fexpr[0] = exsym
      fexpr.typ = exsym.symbol.fexpr.defn.ret.symbol
      
    scope.nextPass(fexpr)
  else:
    scope.nextPass(fexpr)

proc canApplyEffect*(fexpr: FExpr): bool =
  fexpr[0].kind == fexprSymbol and fexpr[0].symbol.fexpr.hasEffect

proc effectPass*(scope: Scope, fexpr: var FExpr) {.pass: SemPass.} =
  if fexpr.isNormalFuncCall or fexpr.isGenericsFuncCall:
    if fexpr.canApplyEffect:
      expandEffectedCall(rootPassProc, scope, fexpr)
    scope.nextPass(fexpr)
  else:
    scope.nextPass(fexpr)

proc destroyCheckPass*(scope: Scope, fexpr: var FExpr) {.pass: SemPass.} =
  if fexpr.isFuncCall:
    let args = if fexpr.isNormalFuncCall:
                 fexpr[1]
               elif fexpr.isGenericsFuncCall:
                 fexpr[2]
               else:
                 fseq(fexpr.span, @[fexpr[1], fexpr[2]])
    for arg in args:
      if arg.kind == fexprSymbol and arg.symbol.fexpr.ctrc.exdestroyed:
        arg.error("$# has been explicit destroyed." % $arg)
    scope.nextPass(fexpr)
  else:
    scope.nextPass(fexpr)

proc explicitDestructPass*(scope: Scope, fexpr: var FExpr) {.pass: SemPass.} =
  if fexpr.isNormalFuncCall:
    if $fexpr[0].symbol.name == "destruct":
      if fexpr[1][0].kind == fexprSymbol:
        fexpr[1][0].symbol.fexpr.ctrc.exdestroyed = true
    scope.nextPass(fexpr)
  else:
    scope.nextPass(fexpr)  

proc finalPass*(scope: Scope, fexpr: var FExpr) {.pass: SemPass.} =
  fexpr.evaluated = true
    
instPass SemPass, processSemPass
