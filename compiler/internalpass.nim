
import parser, types, fexpr, scope, metadata

import options
import strutils, sequtils
import tables

proc replaceByTypesym*(fexpr: var FExpr, sym: Symbol) =
  fexpr = fsymbol(fexpr.span, sym)

proc voidtypeExpr*(span: Span): FExpr =
  return fident(span, name("Void"))

#
# Parser
#

proc parseTypeExpr*(fexpr: FExpr, pos: var int): tuple[typ: FExpr, generics: FExpr] =
  if fexpr.kind in {fexprIdent, fexprSymbol, fexprQuote}:
    result = (fexpr, farray(fexpr[pos].span))
  elif fexpr.isParametricTypeExpr(pos):
    result = (fexpr[pos], fexpr[pos+1])
    pos += 2
  elif fexpr[pos].kind in {fexprIdent, fexprQuote}:
    result = (fexpr[pos], farray(fexpr[pos].span))
    pos += 1
  else:
    fexpr[pos].error("$# isn't type expression." % $fexpr[pos])

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
    result.retgenerics = farray(fexpr.span)

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
  result.elsebranch = fblock(fexpr.span)
  
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

#
# Pragma
#

proc semImportc*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  if fexpr.kind == fexprIdent:
    let name = $fexpr.parent[0]
    fexpr.parent.internalPragma.importc = some(name)
  else:
    if fexpr[1].kind != fexprStrLit:
      fexpr[1].error("importc argument should be FStrLit")
    let name = fexpr[1].strval
    fexpr.parent.internalPragma.importc = some(name)

proc semHeader*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
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

proc semPattern*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  if fexpr.len == 2:
    if $fexpr[1] == "infixc":
      fexpr.parent.internalPragma.infixc = true
    elif fexpr[1].kind == fexprStrLit:
      fexpr.parent.internalPragma.pattern = some(fexpr[1].strval)
    else:
      fexpr[1].error("unsupported in pattern pragma")
  else:
    fexpr.error("usage: `pattern \"#1($1)\"` or `pattern infixc`")

proc semPragma*(rootPass: PassProcType, scope: Scope, fexpr: FExpr, pragma: FExpr) =
  if pragma.kind != fexprArray:
    pragma.error("$# isn't internal pragma." % $pragma)
  fexpr.internalPragma = InternalPragma()

  for key in pragma.mitems:
    key.parent = fexpr
    let pragmaname = if key.kind in fexprContainer:
                       name(key[0])
                     else:
                       name(key)
    let internalopt = scope.getFunc(procname(pragmaname, @[]))
    if internalopt.isSome:
      internalopt.get.internalproc(rootPass, scope, key)
    else:
      key[0].error("undeclared $# pragma. (internal only support in currently)" % $pragmaname)

#
# Evaluater
#

proc semType*(scope: Scope, typ: FExpr, generics: FExpr): Symbol =
  let opt = scope.getDecl(name(typ))
  if opt.isNone:
    typ.error("undeclared $# type." % $typ[1])
    
  var sym = opt.get.scope.symbol(opt.get.name, opt.get.kind, opt.get.fexpr)
  for arg in generics.mitems:
    var pos = 0
    let argtyp = arg.parseTypeExpr(pos)
    sym.types.add(scope.semType(argtyp.typ, argtyp.generics))
  return sym

proc declGenerics*(scope: Scope, fexpr: FExpr): seq[Symbol] =
  result = @[]
  for g in fexpr.mitems:
    let sym = scope.symbol(name(g), symbolGenerics, g)
    let status = scope.addDecl(name(g), sym)
    if not status:
      g.error("redefinition $# generics." % $g)
    g = fsymbol(g.span, sym)
    result.add(sym)

proc declArgtypes*(scope: Scope, fexpr: FExpr, isGenerics: bool): seq[Symbol] =
  result = @[]
  for arg in fexpr:
    var pos = 1
    let argtyp = arg.parseTypeExpr(pos)
    let sym = scope.semType(argtyp.typ, argtyp.generics)
    arg[1].replaceByTypesym(sym)
    result.add(sym)
    if not isGenerics:
      let status = scope.addDecl(name(arg[0]), sym)
      if not status:
        arg[0].error("redefinition $# variable." % $arg[0])

proc semFunc*(rootPass: PassProcType, scope: Scope, fexpr: FExpr, parsed: var DefnExpr): (Scope, seq[Symbol], seq[Symbol], Symbol, Symbol) =
  let fnscope = scope.extendScope()
  let generics = if parsed.isGenerics:
                   fnscope.declGenerics(parsed.generics)
                 else:
                   @[]
  let argtypes = fnscope.declArgtypes(parsed.args, parsed.isGenerics)
  let rettype = fnscope.semType(parsed.ret, parsed.retgenerics)
  parsed.ret.replaceByTypesym(rettype)
  
  let symkind = if parsed.name.kind == fexprQuote: symbolInfix else: symbolFunc
  let sym = scope.symbol(name(parsed.name), symkind, fexpr)
  let pd = ProcDecl(isInternal: false, name: name(parsed.name), argtypes: argtypes, generics: generics, returntype: rettype, sym: sym)
  discard fnscope.addFunc(pd)
  
  if parsed.generics.isSpecTypes:
    fnscope.rootPass(parsed.body)
  semPragma(rootPass, scope, fexpr, parsed.pragma)

  # symbol resolve
  let fsym = fsymbol(fexpr[0].span, sym)
  fexpr[1] = fsym
  parsed.name = fsym
  
  return (fnscope, generics, argtypes, rettype, sym)

proc semDefn*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  var parsed = parseDefn(fexpr)
  let (fnscope, generics, argtypes, rettype, sym) = semFunc(rootPass, scope, fexpr, parsed)

  let pd = ProcDecl(isInternal: false, name: name(parsed.name), argtypes: argtypes, generics: generics, returntype: rettype, sym: sym)
  let status = scope.addFunc(pd)
  if not status:
    fexpr.error("redefinition $# function." % $parsed.name)

  # internal metadata for postprocess phase
  fexpr.internalMark = internalDefn
  fexpr.defn = parsed

proc semDeftype*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  var parsed = parseDeftype(fexpr)

  let typescope = scope.extendScope()

  let typename = name(parsed.name)
  let sym = scope.symbol(typename, symbolType, fexpr)
  let status = scope.addDecl(typename, sym)
  if not status:
    fexpr.error("redefinition $# type." % $typename)

  for field in parsed.body:
    var pos = 1
    let fieldtyp = field.parseTypeExpr(pos)
    let s = typescope.semType(fieldtyp.typ, fieldtyp.generics)
    field[1].replaceByTypesym(s)
    sym.types.add(s)
  
  # symbol resolve
  let fsym = fsymbol(fexpr[0].span, sym)
  fexpr[1] = fsym
  parsed.name = fsym
  # internal metadata for postprocess phase
  semPragma(rootPass, scope, fexpr, parsed.pragma)
  fexpr.internalMark = internalDeftype
  fexpr.deftype = parsed

proc addInternalEval*(scope: Scope, n: Name, p: proc (rootPass: PassProcType, scope: Scope, fexpr: var FExpr)) =
  let status = scope.addFunc(ProcDecl(
    isInternal: true,
    internalproc: p,
    name: n,
    argtypes: @[],
    generics: @[],
    sym: scope.symbol(n, symbolInternal, fident(internalSpan, name("internal")))
  ))
  if not status:
    fseq(internalSpan).error("redefinition $# function." % $n)
  
proc initInternalEval*(scope: Scope) =
  scope.addInternalEval(name("fn"), semDefn)
  scope.addInternalEval(name("type"), semDeftype)

  scope.addInternalEval(name("importc"), semImportc)
  scope.addInternalEval(name("header"), semHeader)
  scope.addInternalEval(name("pattern"), semPattern)

proc initInternalScope*(ctx: SemanticContext) =
  let scope = ctx.newScope(name("internal"))
  scope.initInternalEval()
  ctx.modules[name("internal")] = scope

proc internalScope*(ctx: SemanticContext): Scope =
  ctx.modules[name("internal")]

proc newSemanticContext*(): SemanticContext =
  new result
  result.modules = initOrderedTable[Name, Scope]()
  result.initInternalScope()

proc semModule*(ctx: SemanticContext, rootPass: PassProcType, name: Name, fexprs: var seq[FExpr]) =
  let scope = ctx.newScope(name)
  scope.importScope(name("internal"), ctx.internalScope)
  for f in fexprs.mitems:
    scope.rootPass(f)
    scope.toplevels.add(f)
  ctx.modules[name] = scope
