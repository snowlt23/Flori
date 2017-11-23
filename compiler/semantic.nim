
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
    internalDefn
    internalDeftype
    internalIf
  DefnExpr* = object
    name*: FExpr
    generics*: Option[FExpr]
    args*: FExpr
    ret*: FExpr
    pragma*: FExpr
    body*: FExpr
  DeftypeExpr* = object
    name*: FExpr
    generics*: Option[FExpr]
    pragma*: FExpr
    body*: FExpr
  IfExpr* = object
    cond*: FExpr
    tbody*: FExpr
    fbody*: FExpr
  InternalPragma = object
    importc*: Option[string]
    header*: Option[string]
    infixc*: bool

defMetadata(internalMark, InternalMarkKind)
defMetadata(internalDefnExpr, DefnExpr)
defMetadata(internalDeftypeExpr, DeftypeExpr)
defMetadata(internalIfExpr, IfExpr)
defMetadata(internalPragma, InternalPragma)

proc evalFExpr*(ctx: SemanticContext, scope: Scope, fexpr: FExpr)

proc name*(fexpr: FExpr): Name =
  case fexpr.kind
  of fexprIdent:
    return name($fexpr)
  else:
    fexpr.error("$# is not name." % $fexpr)
proc typename*(fexpr: FExpr): Name =
  if fexpr.kind == fexprList and $fexpr[0] == "type":
    return name($fexpr[1])
  else:
    fexpr.error("$# is not typename." % $fexpr)

proc addInternalEval*(scope: Scope, n: Name, p: proc (ctx: SemanticContext, scope: Scope, fexpr: FExpr)) =
  let status = scope.addFunc(ProcDecl(isInternal: true, internalproc: p, name: n, argtypes: @[], sym: scope.symbol(n, symbolInternal, fident(internalSpan, "internal")))) # FIXME: returntype
  if not status:
    fnil(internalSpan).error("redefinition $# function." % $n)

proc voidtypeExpr*(span: Span): FExpr =
  flist(span, fident(span, "type"), fident(span, "void"))  

proc isTypeExpr*(fexpr: FExpr): bool =
  fexpr.kind == fexprList and $fexpr[0] == "type"
proc isPragmaExpr*(fexpr: FExpr): bool =
  fexpr.kind == fexprList and $fexpr[0] == "pragma"

proc parseDefn*(fexpr: FExpr): DefnExpr =
  var pos = 1

  result.name = fexpr[pos]
  pos.inc

  if fexpr[pos].kind == fexprMap:
    result.generics = some(fexpr[pos])
    pos.inc
  else:
    result.generics = none(FExpr)

  if fexpr[pos].kind == fexprArray:
    result.args = fexpr[pos]
    pos.inc
  else:
    fexpr[pos].error("function arguments should be ^farray.")

  if fexpr[pos].isTypeExpr:
    result.ret = fexpr[pos]
    pos.inc
  else:
    result.ret = voidtypeExpr(fexpr.span)

  if fexpr[pos].isPragmaExpr:
    result.pragma = fexpr[pos][1]
    pos.inc
  else:
    result.pragma = fmap(fexpr.span)

  if fexpr.len > pos:
    result.body = fexpr[pos..^1]
  else:
    result.body = fnil(fexpr.span)

proc parseDeftype*(fexpr: FExpr): DeftypeExpr =
  var pos = 1

  result.name = fexpr[pos]
  pos.inc

  if fexpr[pos].kind == fexprMap:
    result.generics = some(fexpr[pos])
    pos.inc
  else:
    result.generics = none(FExpr)

  if fexpr[pos].isPragmaExpr:
    result.pragma = fexpr[pos][1]
    pos.inc
  else:
    result.pragma = fmap(fexpr.span)
  
  if fexpr.len > pos:
    result.body = fexpr[pos..^1]
  else:
    result.body = fnil(fexpr.span)

proc parseIf*(fexpr: FExpr): IfExpr =
  if fexpr.len != 4:
    fexpr.error("if expression require 3 arguments.")
  result.cond = fexpr[1]
  result.tbody = fexpr[2]
  result.fbody = fexpr[3]

proc addInternalPragma*(fexpr: FExpr, pragma: FExpr) =
  var ipragma = InternalPragma()
  for key, value in pragma.map:
    if $key == "importc":
      if value.kind != fexprStrLit:
        value.error("importc pragma requires fstrlit.")
      ipragma.importc = some(value.strval)
    elif $key == "header":
      if $value == "nodeclc":
        ipragma.header = none(string)
      else:
        if value.kind != fexprStrLit:
          value.error("importc pragma requires fstrlit.")
        ipragma.header = some(value.strval)
    elif $key == "pattern":
      if $value == "infixc":
        ipragma.infixc = true
      else:
        value.error("unknown pragma.")
    else:
      value.error("unknown pragma.")
  fexpr.internalPragma = ipragma

proc evalDefn*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  var parsed = parseDefn(fexpr)
  let fname = parsed.name
  var argtypes = newSeq[Symbol]()
  let rettype = scope.getDecl(typename(parsed.ret))
  if rettype.isNone:
    parsed.ret.error("undeclared $# type." % $parsed.ret)
  parsed.ret = fsymbol(parsed.ret.span, rettype.get)

  let fnscope = scope.extendScope()

  for i in countup(0, parsed.args.len-1, 2):
    let t = parsed.args[i]
    let n = parsed.args[i+1]
    let opt = scope.getDecl(typename(t))
    if opt.isNone:
      t.error("undeclared $# type." % $n)
    parsed.args[i] = fsymbol(parsed.args[i].span, opt.get)
    n.typ = opt
    argtypes.add(opt.get)
    let status = fnscope.addDecl(name(n), opt.get)
    if not status:
      n.error("redefinition $# variable." % $n)

  let sym = scope.symbol(name(fname), symbolFunc, fexpr)
  let status = scope.addFunc(ProcDecl(isInternal: false, name: name(fname), argtypes: argtypes, returntype: rettype.get, sym: sym))
  if not status:
    fexpr.error("redefinition $# function." % $fname)

  if parsed.body.kind != fexprNil:
    for e in parsed.body:
      ctx.evalFExpr(fnscope, e)

  # symbol resolve
  let fsym = fsymbol(fexpr[0].span, sym)
  fexpr[1] = fsym
  parsed.name = fsym
  # internal metadata for postprocess phase
  fexpr.addInternalPragma(parsed.pragma)
  fexpr.internalMark = internalDefn
  fexpr.internalDefnExpr = parsed

proc evalDeftype*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  var parsed = parseDeftype(fexpr)
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
  fexpr.internalMark = internalDeftype
  fexpr.internalDeftypeExpr = parsed

proc isEqualType*(types: seq[Symbol]): bool =
  let first = types[0]
  for t in types[1..^1]:
    if first != t:
      return false
  return true

proc evalIf*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  let parsed = parseIf(fexpr)
  ctx.evalFExpr(scope, parsed.cond)
  ctx.evalFExpr(scope, parsed.tbody)
  ctx.evalFExpr(scope, parsed.fbody)
  
  if parsed.tbody.typ.get == parsed.fbody.typ.get:
    fexpr.typ = parsed.tbody.typ
  else:
    let opt = scope.getDecl(name("void"))
    if opt.isNone:
      fexpr.error("undeclared ^void type.")
    fexpr.typ = opt
  # internal metadata for postprocess phase
  fexpr.internalMark = internalIf
  fexpr.internalIfExpr = parsed

proc initInternalEval*(scope: Scope) =
  scope.addInternalEval(name("defn"), evalDefn)
  scope.addInternalEval(name("deftype"), evalDeftype)
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
    let opt = scope.getDecl(name("int"))
    if opt.isNone:
      fexpr.error("undeclared ^int type.")
    fexpr.typ = opt
  of fexprStrLit:
    let opt = scope.getDecl(name("cstring"))
    if opt.isNone:
      fexpr.error("undeclared ^cstring type.")
    fexpr.typ = opt
  of fexprList:
    let fn = fexpr[0]
    let internalopt = scope.getFunc(procname(name(fn), @[]))
    if internalopt.isSome:
      internalopt.get.internalproc(ctx, scope, fexpr)
    else:
      let fn = fexpr[0]
      for arg in fexpr[1..^1]:
        ctx.evalFExpr(scope, arg)
      let argtypes = fexpr[1..^1].mapIt(it.typ.get)

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
