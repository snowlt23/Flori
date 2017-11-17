
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
  IfExpr* = object
    tbody*: (FExpr, FExpr)
    ebody*: seq[(FExpr, FExpr)]
    fbody*: Option[FExpr]
    
proc evalFExpr*(ctx: SemanticContext, scope: Scope, fexpr: FExpr)

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

proc parseIf*(fexpr: FExpr): IfExpr =
  var pos = 1
  if fexpr[pos].kind != fexprList:
    fexpr[pos].error("if condition should be FList.")
  let tcond = fexpr[pos]
  pos.inc

  if fexpr[pos].kind != fexprBlock:
    fexpr[pos].error("if body should be FBlock.")
  result.tbody = (tcond, fexpr[pos])
  pos.inc

  result.ebody = @[]
  while fexpr.len > pos:
    if $fexpr[pos] == "elif":
      pos.inc
      if fexpr[pos].kind != fexprList:
        fexpr[pos].error("elif condition should be FList.")
      let econd = fexpr[pos]
      pos.inc
      result.ebody.add((econd, fexpr[pos]))
      pos.inc
    elif $fexpr[pos] == "else":
      pos.inc
      result.fbody = some(fexpr[pos])
      pos.inc
    else:
      fexpr[pos].error("if expression expect `else or `elif ident.") 

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

proc isEqualType*(types: seq[Symbol]): bool =
  let first = types[0]
  for t in types[1..^1]:
    if first != t:
      return false
  return true

proc evalIf*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  let parsed = parseIf(fexpr)
  let (tcond, tbody) = parsed.tbody
  ctx.evalFExpr(scope, tcond)
  ctx.evalFExpr(scope, tbody)
  if parsed.fbody.isSome:
    ctx.evalFExpr(scope, parsed.fbody.get)
  var types = @[tbody.typ.get, parsed.fbody.get.typ.get]
  for e in parsed.ebody:
    let (econd, ebody) = e
    ctx.evalFExpr(scope, econd)
    ctx.evalFExpr(scope, ebody)
    types.add(ebody.typ.get)
  if parsed.fbody.isSome and isEqualType(types):
    fexpr.typ = some(types[0])
  else:
    let opt = scope.getDecl(name("Void"))
    if opt.isNone:
      fexpr.error("undeclared Void type.")
    fexpr.typ = opt

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
      ctx.evalFExpr(scope, son)
    if fexpr.len >= 1:
      fexpr.typ = fexpr[^1].typ
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
