
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
  of fexprSeq:
    for i in 1..<fexpr.len:
      scope.rootPass(fexpr[i])
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
    # TODO: type infer for symbol
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
    
instPass SemPass, processSemPass
