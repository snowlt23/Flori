
import fexpr
import scope
import tables
import options
import strutils, sequtils

import types
export types.SemanticContext

type
  FnExpr* = object
    name*: FExpr
    generics*: Option[seq[(FExpr, FExpr)]]
    args*: seq[(FExpr, FExpr)]
    ret*: FExpr
    pragma*: Option[FExpr]
    body*: Option[FExpr]
  StructExpr* = object
    name*: FExpr
    generics*: Option[seq[(FExpr, FExpr)]]
    pragma*: Option[FExpr]
    body*: Option[FExpr]
    
proc name*(fexpr: FExpr): Name =
  case fexpr.kind
  of fexprIdent:
    return name(fexpr.ident)
  of fexprPrefix:
    return name(fexpr.prefix)
  of fexprInfix:
    return name(fexpr.infix)
  of fexprQuote:
    return name(fexpr.quoted)
  else:
    fexpr.error("$# is not name." % $fexpr)

proc addInternalEval*(scope: Scope, n: string, p: proc (ctx: SemanticContext, scope: Scope, fexpr: FExpr)) =
  let status = scope.addFunc(ProcDecl(isInternal: true, internalproc: p, name: name(n), argtypes: @[], sym: scope.symbol(n, symbolInternal))) # FIXME: returntype
  if not status:
    fseq(internalSpan).error("redefinition $# function." % n)

proc parseFn*(fexpr: FExpr): FnExpr =
  var pos = 1

  result.name = fexpr[pos]
  pos.inc

  if fexpr[pos].kind == fexprArray:
    result.generics = some(fexpr[pos].sons.mapIt((it[0], it[1])))
    pos.inc
  else:
    result.generics = none(seq[(FExpr, FExpr)])

  if fexpr[pos].kind == fexprList:
    result.args = fexpr[pos].sons.mapIt((it[0], it[1]))
    pos.inc
  else:
    fexpr[pos].error("function arguments should be FList.")

  if fexpr[pos].kind == fexprIdent:
    result.ret = fexpr[pos]
    pos.inc
  else:
    result.ret = fident(fexpr.span, "Void")

  if fexpr[pos].kind == fexprPrefix and fexpr[pos].prefix == "$":
    pos.inc
    result.pragma = some(fexpr[pos])
    pos.inc
  else:
    result.pragma = none(FExpr)

  if fexpr.len > pos:
    if fexpr[pos].kind != fexprBlock:
      fexpr[pos].error("function body should be FBlock.")
    result.body = some(fexpr[pos])
    pos.inc
  else:
    result.body = none(FExpr)

proc parseStruct*(fexpr: FExpr): StructExpr =
  var pos = 1

  result.name = fexpr[pos]
  pos.inc

  if fexpr[pos].kind == fexprArray:
    result.generics = some(fexpr[pos].sons.mapIt((it[0], it[1])))
    pos.inc
  else:
    result.generics = none(seq[(FExpr, FExpr)])

  if fexpr[pos].kind == fexprPrefix and fexpr[pos].prefix == "$":
    pos.inc
    result.pragma = some(fexpr[pos])
    pos.inc
  else:
    result.pragma = none(FExpr)
  
  if fexpr.len > pos and fexpr[pos].kind == fexprBlock:
    if fexpr[pos].kind != fexprBlock:
      fexpr[pos].error("struct body should be FBlock.")
    result.body = some(fexpr[pos])
    pos.inc
  else:
    result.body = none(FExpr)

proc evalFn*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  let parsed = parseFn(fexpr)
  let fname = parsed.name
  var argtypes = newSeq[Symbol]()
  let rettype = scope.getDecl(name(parsed.ret))
  if rettype.isNone:
    parsed.ret.error("undeclared $# type." % $parsed.ret)
  for arg in parsed.args:
    let (n, t) = arg
    let opt = scope.getDecl(name(t))
    if opt.isNone:
      t.error("undeclared $# type." % $n)
    n.typ = opt
    argtypes.add(opt.get)
  let status = scope.addFunc(ProcDecl(isInternal: false, name: name(fname), argtypes: argtypes, returntype: rettype.get, sym: scope.symbol($fname, symbolFunc)))
  if not status:
    fexpr.error("redefinition $# function." % $fname)

proc evalStruct*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  let parsed = parseStruct(fexpr)
  let fname = parsed.name
  let sym = scope.symbol($fname, symbolType)
  let status = scope.addDecl(name(fname), sym)
  if not status:
    fexpr.error("redefinition $# type." % $fname)

# TODO: evalIf
proc evalIf*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  discard

proc initInternalEval*(scope: Scope) =
  scope.addInternalEval("fn", evalFn)
  scope.addInternalEval("struct", evalStruct)
  scope.addInternalEval("if", evalIf)

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
    let internalopt = scope.getFunc(procname(name(fn), @[]))
    if internalopt.isSome:
      internalopt.get.internalproc(ctx, scope, fexpr)
    else:
      fexpr.error("undeclared $# macro. (unsupported macro in currently)" % $fn)
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
    let opt = scope.getFunc(procname(name(fn), argtypes))
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
