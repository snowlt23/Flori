
import types, fexpr, parser
import scope, semantic
import metadata

import options
import strutils
import tables

type
  InternalMarkKind* = enum
    internalDefn
    internalDeftype
    internalIf
    internalWhile
    internalDef
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
  WhileExpr* = object
    cond*: FExpr
    body*: FExpr
  DefExpr* = object
    name*: FExpr
    value*: FExpr
    isToplevel*: bool
  InternalPragma = object
    importc*: Option[string]
    header*: Option[string]
    infixc*: bool

defMetadata(internalToplevel, bool)
defMetadata(internalMark, InternalMarkKind)
defMetadata(internalDefnExpr, DefnExpr)
defMetadata(internalDeftypeExpr, DeftypeExpr)
defMetadata(internalIfExpr, IfExpr)
defMetadata(internalWhileExpr, WhileExpr)
defMetadata(internalDefExpr, DefExpr)
defMetadata(internalPragma, InternalPragma)

#
# Parser
#

proc parseDefn*(fexpr: FExpr): DefnExpr =
  var pos = 1

  result.name = fexpr[pos]
  pos.inc

  if fexpr[pos].isGenericsExpr:
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

  if fexpr.len > pos and fexpr[pos].isPragmaExpr:
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

  if fexpr[pos].isGenericsExpr:
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

proc parseWhile*(fexpr: FExpr): WhileExpr =
  if fexpr.len < 2:
    fexpr.error("while statement require arguments greater than 2")
  result.cond = fexpr[1]
  result.body = fexpr[2..^1]

proc parseDef*(fexpr: FExpr): DefExpr =
  if fexpr.len != 3:
    fexpr.error("if expression require 2 arguments.")
  result.name = fexpr[1]
  result.value = fexpr[2]

#
# Evaluater
#

proc isGenerics*(defn: DefnExpr): bool = defn.generics.isSome
proc isGenerics*(deftype: DeftypeExpr): bool = deftype.generics.isSome

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

  let fnscope = scope.extendScope()
  let rettype = ctx.evalType(fnscope, parsed.ret)
  var argtypes = newSeq[Symbol]() # for procdecl

  for i in countup(0, parsed.args.len-1, 2):
    let n = parsed.args[i+1]
    let sym = ctx.evalType(fnscope, parsed.args[i])
    n.typ = some(sym)
    argtypes.add(sym)
    let status = fnscope.addDecl(name(n), sym)
    if not status:
      n.error("redefinition $# variable." % $n)

  let sym = scope.symbol(name(parsed.name), symbolFunc, fexpr)
  let status = scope.addFunc(ProcDecl(isInternal: false, name: name(parsed.name), argtypes: argtypes, returntype: rettype, sym: sym))
  if not status:
    fexpr.error("redefinition $# function." % $parsed.name)

  if parsed.body.kind != fexprNil:
    for e in parsed.body:
      ctx.evalFExpr(fnscope, e)

  # symbol resolve
  let fsym = fsymbol(fexpr[0].span, sym)
  fexpr[1] = fsym
  parsed.name = fsym
  parsed.ret = fsymbol(parsed.ret.span, rettype)
  # internal metadata for postprocess phase
  fexpr.addInternalPragma(parsed.pragma)
  fexpr.internalMark = internalDefn
  fexpr.internalDefnExpr = parsed

proc evalDeftype*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  var parsed = parseDeftype(fexpr)
  let sname = parsed.name
  let sym = scope.symbol(name(sname), if parsed.generics.isSome: symbolTypeGenerics else: symbolType, fexpr)
  let status = scope.addDecl(name(sname), sym)
  if not status:
    fexpr.error("redefinition $# type." % $sname)
  fexpr.typ = some(sym)
  
  # symbol resolve
  let fsym = fsymbol(fexpr[0].span, sym)
  fexpr[1] = fsym
  parsed.name = fsym
  # internal metadata for postprocess phase
  fexpr.addInternalPragma(parsed.pragma)
  fexpr.internalMark = internalDeftype
  fexpr.internalDeftypeExpr = parsed

proc evalIf*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  let parsed = parseIf(fexpr)
  ctx.evalFExpr(scope, parsed.cond)
  ctx.evalFExpr(scope, parsed.tbody)
  ctx.evalFExpr(scope, parsed.fbody)

  if parsed.cond.typ.get.name != name("bool"):
    parsed.cond.error("if expression cond should be ^bool")
  
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

proc evalWhile*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  let parsed = parseWhile(fexpr)

  ctx.evalFExpr(scope, parsed.cond)
  if parsed.cond.typ.get.name != name("bool"):
    parsed.cond.error("while statement cond should be ^bool")

  for b in parsed.body:
    ctx.evalFExpr(scope, b)

  scope.resolveByVoid(fexpr)

  # internal metadata for postprocess phase
  fexpr.internalMark = internalWhile
  fexpr.internalWhileExpr = parsed

proc evalDef*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  var parsed = parseDef(fexpr)
  if parsed.name.kind != fexprIdent:
    parsed.name.error("def name should be fident.")

  let sym = scope.symbol(name(parsed.name), symbolVar, parsed.value)
  let status = scope.addDecl(name(parsed.name), sym)
  if not status:
    fexpr.error("redefinition $# variable." % $parsed.name)

  ctx.evalFExpr(scope, parsed.value)
  if parsed.value.typ.get.isVoidType:
    parsed.value.error("value is void.")
  scope.resolveByVoid(fexpr)

  # symbol resolve
  let fsym = fsymbol(fexpr[0].span, sym)
  fexpr[1] = fsym
  parsed.name = fsym
  # internal metadata for postprocess phase
  fexpr.internalMark = internalDef
  fexpr.internalDefExpr = parsed

proc initInternalEval*(scope: Scope) =
  scope.addInternalEval(name("defn"), evalDefn)
  scope.addInternalEval(name("deftype"), evalDeftype)
  scope.addInternalEval(name("if"), evalIf)
  scope.addInternalEval(name("while"), evalWhile)
  scope.addInternalEval(name("def"), evalDef)

proc initInternalScope*(ctx: SemanticContext) =
  let scope = newScope(name("internal"))
  scope.initInternalEval()
  ctx.modules[name("internal")] = scope

proc newSemanticContext*(): SemanticContext =
  new result
  result.modules = initTable[Name, Scope]()
  result.initInternalScope()
