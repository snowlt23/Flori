
import compiler.parser, compiler.types, compiler.fexpr, compiler.scope, compiler.metadata
import compiler.passmacro

import options
import strutils, sequtils

definePass SemPass

proc internalPass*(scope: Scope, fexpr: var FExpr) {.pass: SemPass.} =
  case fexpr.kind
  of fexprSeq:
    let fnident = fexpr[0]
    let internalopt = scope.getFunc(procname(name(fnident), @[]))
    if internalopt.isSome and internalopt.get.isInternal:
      internalopt.get.internalproc(rootPassProc, scope, fexpr)
    else:
      scope.nextPass(fexpr)
  else:
    scope.nextPass(fexpr)

proc toplevelPass*(scope: Scope, fexpr: var FExpr) {.pass: SemPass.} =
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
    fexpr.typ = fexpr.symbol.fexpr.typ
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
  else:
    scope.nextPass(fexpr)

# TODO:
proc expandTemplates*(scope: Scope, fexpr: var FExpr) {.pass: SemPass.} =
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
    elif fexpr.len == 3 and fexpr[0].kind == fexprInfix: # infix call
      let fnident = fexpr[0]
      let argtypes = @[fexpr[1].typ, fexpr[2].typ]
      let opt = scope.getFunc(procname(name(fnident), argtypes))
      if opt.isNone:
        fexpr.error("undeclared `$#($#) function." % [$fnident, argtypes.mapIt($it).join(", ")])
      fexpr[0] = fsymbol(fexpr[0].span, opt.get.sym)
      fexpr.typ = opt.get.returntype
      
    scope.nextPass(fexpr)
  else:
    scope.nextPass(fexpr)
    
instPass SemPass, processSemPass
