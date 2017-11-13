
import fexpr
import scope
import tables
import options
import strutils, sequtils

import types
export types.SemanticContext

proc addInternalEval*(scope: Scope, n: string, p: proc (ctx: SemanticContext, scope: Scope, fexpr: FExpr)) =
  let status = scope.addFunc(ProcDecl(isInternal: true, internalproc: p, name: name(n), argtypes: @[], sym: scope.symbol(n, symbolInternal)))
  if not status:
    fseq(internalSpan).error("redefinition $# function." % n)

proc evalFn*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  discard
proc evalStruct*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  discard

proc initInternalEval*(scope: Scope) =
  scope.addInternalEval("fn", evalFn)
  scope.addInternalEval("struct", evalFn)

proc initInternalScope*(ctx: SemanticContext) =
  let scope = newScope(name("internal"))
  scope.initInternalEval()
  ctx.modules[name("internal")] = scope

proc newSemanticContext*(): SemanticContext =
  new result
  result.modules = initTable[Name, Scope]()
  result.initInternalScope()

proc internalScope*(ctx: SemanticContext): Scope =
  ctx.modules[name("internal")]

proc name*(fexpr: FExpr): Name =
  if fexpr.kind == fexprIdent:
    return name(fexpr.ident)
  else:
    fexpr.error("$# is not name." % $fexpr)

proc evalFExpr*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  case fexpr.kind
  of fexprIdent:
    let opt = scope.getDecl(name(fexpr))
    if opt.isNone:
      fexpr.error("undeclared $# ident." % fexpr.ident)
    fexpr.typ = opt
  of fexprSymbol:
    discard
  of fexprIntLit:
    let opt = scope.getDecl(name("Int32"))
    if opt.isNone:
      fexpr.error("undeclared Int32 type.")
    fexpr.typ = opt
  of fexprStrLit:
    let opt = scope.getDecl(name("CString"))
    if opt.isNone:
      fexpr.error("undeclared CString type.")
    fexpr.typ = opt
  of fexprSeq:
    let fn = fexpr[0]
    let internalopt = scope.getProc(procname(name(fn), @[]))
    if internalopt.isSome:
      internalopt.get.internalproc(ctx, scope, fexpr)
    else:
      for arg in fexpr[1..^1]:
        ctx.evalFExpr(scope, arg)
      let argtypes = fexpr.sons[1..^1].mapIt(it.typ.get)  
      let opt = scope.getProc(procname(name(fn), argtypes))
      if opt.isNone:
        fexpr.error("undeclared $#($#) function." % [$fn, argtypes.mapIt($it).join(", ")])
      fexpr.typ = some(opt.get.returntype)
  of fexprArray..fexprBlock:
    for son in fexpr:
      ctx.evalFExpr(scope, fexpr)
    if fexpr.kind == fexprList and fexpr.len == 1:
      fexpr.typ = fexpr[0].typ
  of fexprCall:
    for arg in fexpr[1..^1]:
      ctx.evalFExpr(scope, arg)
    let argtypes = fexpr.sons[1..^1].mapIt(it.typ.get)
    let fn = fexpr[0]
    let opt = scope.getProc(procname(name(fn), argtypes))
    if opt.isNone:
      fexpr.error("undeclared $#($#) function." % [$fn, argtypes.mapIt($it).join(", ")])
    fexpr.typ = some(opt.get.returntype)
  else:
    fexpr.error("$# is unsupported expression in eval." % $fexpr.kind)

proc evalModule*(ctx: SemanticContext, name: Name, fexprs: seq[FExpr]) =
  let scope = newScope(name)
  scope.importScope(ctx.internalScope)
  for f in fexprs:
    ctx.evalFExpr(scope, f)
  ctx.modules[name] = scope
