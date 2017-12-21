
import types, fexpr, parser
import scope, semantic
import metadata

import options
import strutils, sequtils
import tables
import deques

type
  IfExpr* = object
    elifbranch*: seq[tuple[cond: FExpr, body: FExpr]]
    elsebranch*: FExpr
  WhileExpr* = object
    cond*: FExpr
    body*: FExpr
  DefExpr* = object
    name*: FExpr
    value*: FExpr
  FieldAccessExpr* = object
    value*: FExpr
    fieldname*: FExpr
  ImportExpr* = object
    modname*: Name
    importname*: Name

defMetadata(internalIfExpr, IfExpr)
defMetadata(internalWhileExpr, WhileExpr)
defMetadata(internalDefExpr, DefExpr)
defMetadata(internalFieldAccessExpr, FieldAccessExpr)
defMetadata(internalImportExpr, ImportExpr)
defMetadata(parent, FExpr)

#
# Parser
#

proc parseDefn*(fexpr: FExpr): DefnExpr =
  var pos = 1

  let ftyp = fexpr.parseTypeExpr(pos)
  result.name = ftyp.typ
  result.generics = ftyp.generics

  if fexpr[pos].kind == fexprList:
    result.args = fexpr[pos]
    pos.inc
  else:
    fexpr[pos].error("function arguments should be FList.")
  
  if fexpr.isParametricTypeExpr(pos) or fexpr[pos].kind == fexprIdent:
    let rtyp = fexpr.parseTypeExpr(pos)
    result.ret = rtyp.typ
    result.retgenerics = rtyp.generics
  else:
    result.ret = voidtypeExpr(fexpr.span)
    result.retgenerics = none(FExpr)

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

  let ttyp = fexpr.parseTypeExpr(pos)
  result.name = ttyp.typ
  result.generics = ttyp.generics

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

  if fexpr[pos].kind != fexprList or fexpr[pos].len != 1:
    fexpr[pos].error("if cond should be single FList.")
  if fexpr[pos+1].kind != fexprBlock:
    fexpr[pos+1].error("if body should be FBlock.")
  result.elifbranch.add((fexpr[pos][0], fexpr[pos+1]))
  pos += 2

  while fexpr.len > pos:
    if $fexpr[pos] == "elif":
      pos.inc
      let cond = fexpr[pos]
      if cond.kind != fexprList or cond.len != 1:
        fexpr[pos].error("elif cond should be single FList.")
      pos.inc
      let body = fexpr[pos]
      if body.kind != fexprBlock:
        fexpr[pos].error("elif body should be FBlock.")
      pos.inc
      result.elifbranch.add((fexpr[pos][0], fexpr[pos+1]))
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

proc isGenerics*(defn: DefnExpr): bool = defn.generics.isSome
proc isGenerics*(deftype: DeftypeExpr): bool = deftype.generics.isSome

#
# Pragma
#

proc evalImportc*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  if fexpr.kind == fexprIdent:
    let name = $fexpr.parent[0]
    fexpr.parent.internalPragma.importc = some(name)
  else:
    if fexpr[1].kind != fexprStrLit:
      fexpr[1].error("importc argument should be FStrLit")
    let name = fexpr[1].strval
    fexpr.parent.internalPragma.importc = some(name)

proc evalHeader*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  if fexpr.len == 2:
    if $fexpr[1] == "nodeclc":
      fexpr.parent.internalPragma.header = none(string)
    elif fexpr[1].kind == fexprStrLit:
      let name = fexpr[1].strval
      fexpr.parent.internalPragma.header = some(name)
    else:
      fexpr[1].error("header argument should be FStrLit")
  else:
    fexpr.error("usage: `header \"headername.h\"`")

proc evalPattern*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  if fexpr.len == 2:
    if $fexpr[1] == "infixc":
      fexpr.parent.internalPragma.infixc = true
    elif fexpr[1].kind == fexprStrLit:
      fexpr.parent.internalPragma.pattern = some(fexpr[1].strval)
    else:
      fexpr[1].error("unsupported in pattern pragma")
  else:
    fexpr.error("usage: `pattern \"#1($1)\"` or `pattern infixc`")

proc evalPragma*(ctx: SemanticContext, scope: Scope, fexpr: FExpr, pragma: FExpr) =
  if pragma.kind != fexprArray:
    pragma.error("$# isn't internal pragma." % $pragma)
  fexpr.internalPragma = InternalPragma()

  for key in pragma:
    key.parent = fexpr
    let pragmaname = if key.kind in fexprContainer:
                       name(key[0])
                     else:
                       name(key)
    let internalopt = scope.getFunc(procname(pragmaname, @[]))
    if internalopt.isSome:
      internalopt.get.internalproc(ctx, scope, key)
    else:
      key[0].error("undeclared $# pragma." % $pragmaname)

#
# Evaluater
#

proc evalDefn*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  var parsed = parseDefn(fexpr)

  let fnscope = scope.extendScope()
  # add generics variable to scope
  var generics = newSeq[Symbol]()
  if parsed.generics.isSome:
    for g in parsed.generics.get.mitems:
      let sym = fnscope.symbol(name(g), symbolGenerics, g)
      let status = fnscope.addDecl(name(g), sym)
      g.typ = some(sym) # FIXME:
      if not status:
        g.error("redefinition $# generics." % $g)
      generics.add(sym)

  let rettype = ctx.evalType(fnscope, parsed.ret, parsed.retgenerics)
  parsed.ret.replaceByTypesym(rettype)
  var argtypes = newSeq[Symbol]() # for procdecl

  for arg in parsed.args:
    var pos = 1
    let argtyp = arg.parseTypeExpr(pos)
    let sym = ctx.evalType(fnscope, argtyp.typ, argtyp.generics)
    arg[1].replaceByTypesym(sym)
    argtypes.add(sym)
    let status = fnscope.addDecl(name(arg[0]), sym)
    if not status:
      arg[0].error("redefinition $# variable." % $arg[0])
  
  let symkind = if parsed.name.kind == fexprQuote: symbolInfix else: symbolFunc
  let sym = scope.symbol(name(parsed.name), symkind, fexpr)
  let pd = ProcDecl(isInternal: false, name: name(parsed.name), argtypes: argtypes, generics: generics, returntype: rettype, sym: sym)
  let status = scope.addFunc(pd)
  if not status:
    fexpr.error("redefinition $# function." % $parsed.name)
  discard fnscope.addFunc(pd)
    
  parsed.body.internalScope = fnscope
  if parsed.generics.isNone:
    ctx.evalFExpr(fnscope, parsed.body)

  # symbol resolve
  let fsym = fsymbol(fexpr[0].span, sym)
  fexpr[1] = fsym
  parsed.name = fsym
  # internal metadata for postprocess phase
  ctx.evalPragma(scope, fexpr, parsed.pragma)
  fexpr.internalMark = internalDefn
  fexpr.defn = parsed

proc getFieldType*(fexpr: FExpr, fieldname: string): Option[Symbol] =
  if not fexpr.hasDeftype:
    fexpr.error("$# isn't structure type." % $fexpr)
  for field in fexpr.deftype.body:
    if $field[0] == fieldname:
      return some(field[1].symbol)
  return none(Symbol)

proc evalDeftype*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  var parsed = parseDeftype(fexpr)

  let typescope = scope.extendScope()
  # add generics variable to scope
  if parsed.generics.isSome:
    for g in parsed.generics.get:
      let status = typescope.addDecl(name(g), typescope.symbol(name(g), symbolGenerics, g))
      if not status:
        g.error("redefinition $# generics." % $g)

  let sname = parsed.name
  let sym = scope.symbol(name(sname), if parsed.generics.isSome: symbolTypeGenerics else: symbolType, fexpr)
  let status = scope.addDecl(name(sname), sym)
  if not status:
    fexpr.error("redefinition $# type." % $sname)
  fexpr.typ = some(sym)

  for field in parsed.body:
    var pos = 1
    let fieldtyp = field.parseTypeExpr(pos)
    let s = ctx.evalType(typescope, fieldtyp.typ, fieldtyp.generics)
    field[1].replaceByTypesym(s)
    sym.types.add(s)
  
  # symbol resolve
  let fsym = fsymbol(fexpr[0].span, sym)
  fexpr[1] = fsym
  parsed.name = fsym
  # internal metadata for postprocess phase
  ctx.evalPragma(scope, fexpr, parsed.pragma)
  fexpr.internalMark = internalDeftype
  fexpr.deftype = parsed

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

proc evalFieldAccess*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  let value = fexpr[1]
  let fieldname = fexpr[2]
  if fieldname.kind != fexprIdent:
    fieldname.error("fieldname should be FIdent.")
  ctx.evalFExpr(scope, value)
  let fieldopt = value.getType.fexpr.getFieldType($fieldname)
  if fieldopt.isNone:
    fieldname.error("value hasn't $# field." % $fieldname)
  fexpr.typ = fieldopt

  # internal metadata for postprocess phase
  fexpr.internalMark = internalFieldAccess
  fexpr.internalFieldAccessExpr = FieldAccessExpr(value: value, fieldname: fieldname)

proc evalInit*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  if fexpr.len != 3:
    fexpr.error("init require 2 arguments.")
  if fexpr[1].kind != fexprList:
    fexpr.error("init type should be FList.")
  if fexpr[1].len != 1:
    fexpr.error("init type should be single argument.")
  if fexpr[2].kind != fexprBlock:
    fexpr.error("init body should be FBlock.")
  var pos = 0
  let inittyp = fexpr[1][0].parseTypeExpr(pos)
  let sym = ctx.evalType(scope, inittyp.typ, inittyp.generics)
  fexpr[1][0].replaceByTypesym(sym)
  ctx.evalFExpr(scope, fexpr[2])

  let types = fexpr[2].mapIt(it.typ.get)
  let t = ctx.instantiateDeftype(scope, sym.fexpr, types)

  # symbol resolve
  fexpr.typ = some(t.symbol)
  # internal metadata for postprocess phase
  fexpr.initexpr = InitExpr(
    typ: t,
    body: fexpr[2],
  )
  fexpr.internalMark = internalInit

proc evalImport*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  let importname = name(fexpr[1])
  let filepath = replace($importname, ".", "/") & ".flori"
  let modname = ctx.evalFile(filepath)
  scope.importScope(importname, ctx.modules[modname])
  # internal metadata for postprocess phase
  fexpr.internalMark = internalImport
  fexpr.internalImportExpr = ImportExpr(
    importname: importname,
    modname: modname
  )

proc initInternalEval*(scope: Scope) =
  scope.addInternalEval(name("fn"), evalDefn)
  scope.addInternalEval(name("type"), evalDeftype)
  scope.addInternalEval(name("if"), evalIf)
  scope.addInternalEval(name("while"), evalWhile)
  scope.addInternalEval(name(":="), evalDef)
  scope.addInternalEval(name("."), evalFieldAccess)
  scope.addInternalEval(name("init"), evalInit)
  scope.addInternalEval(name("import"), evalImport)

  scope.addInternalEval(name("importc"), evalImportc)
  scope.addInternalEval(name("header"), evalHeader)
  scope.addInternalEval(name("pattern"), evalPattern)

proc initInternalScope*(ctx: SemanticContext) =
  let scope = newScope(name("internal"))
  scope.initInternalEval()
  ctx.modules[name("internal")] = scope

proc newSemanticContext*(): SemanticContext =
  new result
  result.expandspans = initDeque[Span]()
  result.modules = initOrderedTable[Name, Scope]()
  result.initInternalScope()
