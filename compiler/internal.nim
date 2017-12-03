
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
    elifbranch*: seq[tuple[cond: FExpr, body: FExpr]]
    elsebranch*: FExpr
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

  if fexpr[pos].kind == fexprArray:
    result.generics = some(fexpr[pos])
    pos.inc
  else:
    result.generics = none(FExpr)

  if fexpr[pos].kind == fexprList:
    result.args = fexpr[pos]
    pos.inc
  else:
    fexpr[pos].error("function arguments should be FList.")

  if fexpr[pos].isTypeExpr:
    result.ret = fexpr[pos]
    pos.inc
  else:
    result.ret = voidtypeExpr(fexpr.span)

  if fexpr.len > pos and fexpr[pos].isPragmaPrefix:
    pos.inc
    if fexpr[pos].kind != fexprArray:
      fexpr[pos].error("pragma should be FArray.")
    result.pragma = fexpr[pos]
    pos.inc
  else:
    result.pragma = farray(fexpr.span)

  if fexpr.len > pos:
    if fexpr[pos].kind != fexprBlock:
      fexpr[pos].error("function body should be FBlock.")
    result.body = fexpr[pos]
  else:
    result.body = fblock(fexpr.span)

proc parseDeftype*(fexpr: FExpr): DeftypeExpr =
  var pos = 1

  result.name = fexpr[pos]
  pos.inc

  if fexpr[pos].kind == fexprArray:
    result.generics = some(fexpr[pos])
    pos.inc
  else:
    result.generics = none(FExpr)

  if fexpr[pos].isPragmaPrefix:
    pos.inc
    if fexpr[pos].kind != fexprArray:
      fexpr[pos].error("pragma should be FArray.")
    result.pragma = fexpr[pos]
    pos.inc
  else:
    result.pragma = farray(fexpr.span)
  
  if fexpr.len > pos:
    if fexpr[pos].kind != fexprBlock:
      fexpr[pos].error("function body should be FBlock.")
    result.body = fexpr[pos]
  else:
    result.body = fblock(fexpr.span)

proc parseIf*(fexpr: FExpr): IfExpr =
  if fexpr.len < 3:
    fexpr.error("if expression require greater than 3 arguments.")
  result.elifbranch = @[]
  
  var pos = 1

  if fexpr[pos].kind != fexprList:
    fexpr[pos].error("if cond should be FList.")
  if fexpr[pos+1].kind != fexprBlock:
    fexpr[pos+1].error("if body should be FBlock.")
  result.elifbranch.add((fexpr[pos], fexpr[pos+1]))
  pos += 2

  while fexpr.len > pos:
    if $fexpr[pos] == "elif":
      pos.inc
      let cond = fexpr[pos]
      if cond.kind != fexprList:
        fexpr[pos].error("elif cond should be FList.")
      pos.inc
      let body = fexpr[pos]
      if body.kind != fexprBlock:
        fexpr[pos].error("elif body should be FBlock.")
      pos.inc
      result.elifbranch.add((fexpr[pos], fexpr[pos+1]))
    else:
      pos.inc
      let body = fexpr[pos]
      if body.kind != fexprBlock:
        fexpr[pos].error("else body should be FBlock.")
      pos.inc
      result.elsebranch = body
      break

proc parseWhile*(fexpr: FExpr): WhileExpr =
  if fexpr.len < 3:
    fexpr.error("while statement require arguments greater than 2")
  result.cond = fexpr[1]
  result.body = fexpr[2]
  if result.cond.kind != fexprList:
    result.cond.error("while cond should be FBlock.")
  if result.body.kind != fexprBlock:
    result.body.error("while body should be FBlock.")

proc parseDef*(fexpr: FExpr): DefExpr =
  if fexpr.len != 3:
    fexpr.error("def expression require 2 arguments.")
  result.name = fexpr[1]
  result.value = fexpr[2]

#
# Evaluater
#

proc isGenerics*(defn: DefnExpr): bool = defn.generics.isSome
proc isGenerics*(deftype: DeftypeExpr): bool = deftype.generics.isSome

proc addInternalPragma*(fexpr: FExpr, pragma: FExpr) =
  var ipragma = InternalPragma()
  if pragma.kind != fexprArray:
    pragma.error("$# isn't internal pragma." % $pragma)

  for key in pragma:
    if key.kind != fexprSeq:
      key.error("pragma should be FSeq.")
    if key.len != 2:
      key.error("pragma.len != 2")
    
    case $key[0]
    of "importc":
      if key[1].kind != fexprStrLit:
        key[1].error("importc value should be FStrLit.")
      ipragma.importc = some(key[1].strval)
    of "header":
      if $key[1] == "nodeclc":
        ipragma.header = none(string)
      else:
        if key[1].kind != fexprStrLit:
          key[1].error("header value should be FStrLit.")
        ipragma.header = some(key[1].strval)
    of "pattern":
      if $key[1] == "infixc":
        ipragma.infixc = true
      else:
        key[1].error("pattern is support infixc only. (in currently)")
    else:
      key[0].error("$# is unknown pragma." % $key[0])

  fexpr.internalPragma = ipragma

proc evalDefn*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  var parsed = parseDefn(fexpr)

  let fnscope = scope.extendScope()
  let rettype = ctx.evalType(fnscope, parsed.ret)
  var argtypes = newSeq[Symbol]() # for procdecl

  for arg in parsed.args:
    let n = arg[0]
    let sym = ctx.evalType(fnscope, arg[1])
    n.typ = some(sym)
    argtypes.add(sym)
    let status = fnscope.addDecl(name(n), sym)
    if not status:
      n.error("redefinition $# variable." % $n)

  let sym = scope.symbol(name(parsed.name), symbolFunc, fexpr)
  let status = scope.addFunc(ProcDecl(isInternal: false, name: name(parsed.name), argtypes: argtypes, returntype: rettype, sym: sym))
  if not status:
    fexpr.error("redefinition $# function." % $parsed.name)

  ctx.evalFExpr(fnscope, parsed.body)

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

proc isEqualTypes*(types: seq[Symbol]): bool =
  var first = types[0]
  for t in types[1..^1]:
    if first != t:
      return false
  return true

proc evalIf*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  let parsed = parseIf(fexpr)
  var types = newSeq[Symbol]()

  for branch in parsed.elifbranch:
    ctx.evalFExpr(scope, branch.cond)
    if not branch.cond.typ.get.isBoolType:
      branch.cond.error("if expression cond should be Bool")
    ctx.evalFExpr(scope, branch.body)
    types.add(branch.body.typ.get)
  ctx.evalFExpr(scope, parsed.elsebranch)
  types.add(parsed.elsebranch.typ.get)

  if isEqualTypes(types):
    fexpr.typ = some(types[0])
  else:
    let opt = scope.getDecl(name("Void"))
    if opt.isNone:
      fexpr.error("undeclared Void type.")
    fexpr.typ = opt
  # internal metadata for postprocess phase
  fexpr.internalMark = internalIf
  fexpr.internalIfExpr = parsed

proc evalWhile*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  let parsed = parseWhile(fexpr)

  ctx.evalFExpr(scope, parsed.cond)
  if not parsed.cond.typ.get.isBoolType:
    parsed.cond.error("while statement cond should be Bool")

  ctx.evalFExpr(scope, parsed.body)

  scope.resolveByVoid(fexpr)

  # internal metadata for postprocess phase
  fexpr.internalMark = internalWhile
  fexpr.internalWhileExpr = parsed

proc evalDef*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  var parsed = parseDef(fexpr)
  if parsed.name.kind != fexprIdent:
    parsed.name.error("define name should be FIdent.")

  let sym = scope.symbol(name(parsed.name), symbolVar, parsed.value)
  let status = scope.addDecl(name(parsed.name), sym)
  if not status:
    fexpr.error("redefinition $# variable." % $parsed.name)

  ctx.evalFExpr(scope, parsed.value)
  if parsed.value.typ.get.isVoidType:
    parsed.value.error("value is Void.")
  scope.resolveByVoid(fexpr)

  # symbol resolve
  let fsym = fsymbol(fexpr[0].span, sym)
  fexpr[1] = fsym
  parsed.name = fsym
  # internal metadata for postprocess phase
  fexpr.internalMark = internalDef
  fexpr.internalDefExpr = parsed

proc initInternalEval*(scope: Scope) =
  scope.addInternalEval(name("fn"), evalDefn)
  scope.addInternalEval(name("type"), evalDeftype)
  scope.addInternalEval(name("if"), evalIf)
  scope.addInternalEval(name("while"), evalWhile)
  scope.addInternalEval(name(":="), evalDef)

proc initInternalScope*(ctx: SemanticContext) =
  let scope = newScope(name("internal"))
  scope.initInternalEval()
  ctx.modules[name("internal")] = scope

proc newSemanticContext*(): SemanticContext =
  new result
  result.modules = initTable[Name, Scope]()
  result.initInternalScope()
