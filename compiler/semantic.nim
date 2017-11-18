
import fexpr, parser
import scope

import tables
import options
import strutils, sequtils
import os

import types
export types.SemanticContext

import metadata

type
  InternalMarkKind* = enum
    internalFn
    internalStruct
    internalIf
  FnExpr* = object
    name*: FExpr
    generics*: Option[seq[(FExpr, FExpr)]]
    args*: seq[(FExpr, FExpr)]
    ret*: FExpr
    pragma*: FExpr
    body*: Option[FExpr]
  StructExpr* = object
    name*: FExpr
    generics*: Option[seq[(FExpr, FExpr)]]
    pragma*: FExpr
    body*: Option[FExpr]
  IfExpr* = object
    tbody*: (FExpr, FExpr)
    ebody*: seq[(FExpr, FExpr)]
    fbody*: Option[FExpr]
  InternalPragma = object
    importc*: bool
    importname*: Option[string]
    header*: Option[string]
    infix*: bool
    nodecl*: bool

defMetadata(internalMark, InternalMarkKind)
defMetadata(internalFnExpr, FnExpr)
defMetadata(internalStructExpr, StructExpr)
defMetadata(internalIfExpr, IfExpr)
defMetadata(internalPragma, InternalPragma)

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

proc addInternalEval*(scope: Scope, n: Name, p: proc (ctx: SemanticContext, scope: Scope, fexpr: FExpr)) =
  let status = scope.addFunc(ProcDecl(isInternal: true, internalproc: p, name: n, argtypes: @[], sym: scope.symbol(n, symbolInternal, fident(internalSpan, "internal")))) # FIXME: returntype
  if not status:
    fseq(internalSpan).error("redefinition $# function." % $n)

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
    result.pragma = fexpr[pos]
    pos.inc
  else:
    result.pragma = farray(fexpr.span)

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
    result.pragma = fexpr[pos]
    pos.inc
  else:
    result.pragma = farray(fexpr.span)
  
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

proc addInternalPragma*(fexpr: FExpr, pragma: FExpr) =
  var ipragma = InternalPragma()
  for e in pragma:
    if e.len >= 2:
      if $e[0] == "importc":
        ipragma.importc = true
        ipragma.importname = some(e[1].strval)
      elif $e[0] == "header":
        ipragma.header = some(e[1].strval)
      else:
        e[0].error("unknown pragma.")
    else:
      if $e == "importc":
        ipragma.importc = true
        ipragma.importname = none(string)
      elif $e == "nodecl":
        ipragma.nodecl = true
      elif $e == "infix":
        ipragma.infix = true
      else:
        e.error("unknown pragma.")
  fexpr.internalPragma = ipragma

proc evalFn*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  var parsed = parseFn(fexpr)
  let fname = parsed.name
  var argtypes = newSeq[Symbol]()
  let rettype = scope.getDecl(name(parsed.ret))
  if rettype.isNone:
    parsed.ret.error("undeclared $# type." % $parsed.ret)
  parsed.ret = fsymbol(parsed.ret.span, rettype.get)

  let fnscope = scope.extendScope()

  for arg in parsed.args.mitems:
    let (n, t) = arg
    let opt = scope.getDecl(name(t))
    if opt.isNone:
      t.error("undeclared $# type." % $n)
    arg[1] = fsymbol(arg[0].span, opt.get)
    n.typ = opt
    argtypes.add(opt.get)
    let status = fnscope.addDecl(name(n), opt.get)
    if not status:
      n.error("redefinition $# variable." % $n)

  let sym = scope.symbol(name(fname), symbolFunc, fexpr)
  let status = scope.addFunc(ProcDecl(isInternal: false, name: name(fname), argtypes: argtypes, returntype: rettype.get, sym: sym))
  if not status:
    fexpr.error("redefinition $# function." % $fname)

  if parsed.body.isSome:
    for e in parsed.body.get:
      ctx.evalFExpr(fnscope, e)

  # symbol resolve
  let fsym = fsymbol(fexpr[0].span, sym)
  fexpr[1] = fsym
  parsed.name = fsym
  # internal metadata for postprocess phase
  fexpr.addInternalPragma(parsed.pragma)
  fexpr.internalMark = internalFn
  fexpr.internalFnExpr = parsed

proc evalStruct*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  var parsed = parseStruct(fexpr)
  let sname = parsed.name
  let sym = scope.symbol(name(sname), symbolType, fexpr)
  let status = scope.addDecl(name(sname), sym)
  if not status:
    fexpr.error("redefinition $# type." % $sname)
  # symbol resolve
  let fsym = fsymbol(fexpr[0].span, sym)
  fexpr[1] = fsym
  parsed.name = fsym
  # internal metadata for postprocess phase
  fexpr.addInternalPragma(parsed.pragma)
  fexpr.internalMark = internalStruct
  fexpr.internalStructExpr = parsed

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
  # internal metadata for postprocess phase
  fexpr.internalMark = internalIf
  fexpr.internalIfExpr = parsed

proc initInternalEval*(scope: Scope) =
  scope.addInternalEval(name("fn"), evalFn)
  scope.addInternalEval(name("struct"), evalStruct)
  scope.addInternalEval(name("if"), evalIf)

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
    # symbol resolve
    fexpr[0] = fsymbol(fexpr[0].span, opt.get.sym)
  else:
    fexpr.error("$# is unsupported expression in eval." % $fexpr.kind)

proc evalModule*(ctx: SemanticContext, name: Name, fexprs: seq[FExpr]) =
  let scope = newScope(name)
  scope.importScope(ctx.internalScope)
  for f in fexprs:
    ctx.evalFExpr(scope, f)
    scope.toplevels.add(f)
  ctx.modules[name] = scope

proc evalFile*(ctx: SemanticContext, filepath: string) =
  let (dir, file, _) = filepath.splitFile()
  let n = name((dir / file).replace("/", "_").replace("\\", "_"))
  let fexprs = parseToplevel(filepath, readFile(filepath))
  ctx.evalModule(n, fexprs)
