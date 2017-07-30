
import sast
import semantic

import strutils, sequtils
import options

proc evalCImport*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let (argtypes, rettype, cffidef) = getTypeAnnotation(scope, sexpr)
  let funcname = cffidef.rest.first
  let nameattr = cffidef.getAttr("name")
  let patternattr = cffidef.getAttr("pattern")
  let headerattr = cffidef.getAttr("header")
  let nodeclattr = cffidef.hasAttr("nodecl")
  let infixattr = cffidef.hasAttr("infix")
  let primpattern = patternattr.isSome
  let primname = if patternattr.isSome:
                   patternattr.get.strval
                 elif nameattr.isSome:
                   nameattr.get.strval
                 else:
                   $funcname
  let primtype = if infixattr:
                   primitiveInfix
                 else:
                   primitiveCall
  # let sym = newSymbol(sexpr, scope, $funcname, argtypesyms.getSemanticTypeArgs)
  if nodeclattr:
    let sym = scope.defPrimitiveFunc(sexpr.span, primpattern, $funcname, argtypes, rettype, primtype, primname)
    return newSemanticExpr(sexpr.span, semanticSymbol, rettype, symbol: sym)
  elif headerattr.isSome:
    scope.module.ccodegeninfo.addHeader(headerattr.get.strval)
    let sym = scope.defPrimitiveFunc(sexpr.span, primpattern, $funcname, argtypes, rettype, primtype, primname)
    return newSemanticExpr(sexpr.span, semanticSymbol, rettype, symbol: sym)
  else:
    let cffi = Cffi(
      name: $funcname,
      primname: primname,
      argtypes: argtypes,
      rettype: rettype,
    )
    scope.module.addCFFI(cffi)
    # FIXME: c-import cffi
    return newSemanticExpr(sexpr.span, semanticSymbol, rettype, symbol: notTypeSym)

proc evalCType*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let typeexpr = sexpr.rest.first
  let typename = if typeexpr.kind == sexprIdent:
                   $typeexpr
                 else:
                   $typeexpr.first
  let primattr = sexpr.getAttr("name")
  let primname = if primattr.isSome:
                   primattr.get.strval
                 else:
                   typename
  let headerattr = sexpr.getAttr("header")
  let nodeclattr = sexpr.hasAttr("nodecl")
  var generics = newSeq[string]()
  if typeexpr.kind == sexprList:
    for gene in typeexpr.rest:
      generics.add($gene)
  var sym: Symbol
  if nodeclattr:
    sym = scope.defPrimitiveType(sexpr.span, generics, typename, primname)
  elif headerattr.isSome:
    scope.module.ccodegeninfo.addHeader(headerattr.get.strval)
    sym = scope.defPrimitiveType(sexpr.span, generics, typename, primname)
  else:
    scope.module.addDecl("extern $#;" % primname)
    sym = scope.defPrimitiveType(sexpr.span, generics, typename, primname)
  return newSemanticExpr(sexpr.span, semanticSymbol, sym, symbol: sym)

proc evalCValue*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let (argtypes, _, vardef) = parseTypeAnnotation(sexpr)
  let varname = $vardef.rest.first
  let primattr = vardef.getAttr("name")
  let primname = if primattr.isSome:
                   primattr.get.strval
                 else:
                   varname
  scope.defPrimitiveValue(sexpr.span, $argtypes[0], varname, primname)
  return newSemanticExpr(sexpr.span, semanticSymbol, notTypeSym, symbol: notTypeSym)

proc evalCEmit*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let format = sexpr.rest.first.strval
  var args = newSeq[SemanticExpr]()
  for se in sexpr.rest.rest:
    args.add(scope.evalSExpr(se))
  return newSemanticExpr(sexpr.span, semanticCExpr, notTypeSym, cexpr: CExpr(format: format, args: args))

proc evalFunctionBody*(scope: var Scope, sexpr: SExpr): seq[SemanticExpr] =
  result = @[]
  for e in sexpr:
    result.add(scope.evalSExpr(e))
proc evalFunction*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let (argtypes, rettype, funcdef) = scope.getTypeAnnotation(sexpr)
  let funcname = funcdef.rest.first
  var argnames = newSeq[string]()
  for arg in funcdef.rest.rest.first:
    argnames.add($arg)

  var scope = scope
  scope.addArgSymbols(argtypes, funcdef)

  let f = Function(
    isGenerics: false,
    name: $funcname,
    argnames: argnames,
    fntype: FuncType(
      returntype: rettype,
      argtypes: argtypes,
    ),
    body: scope.evalFunctionBody(funcdef.rest.rest.rest),
  )
  let semexpr = newSemanticExpr(sexpr.span, semanticFunction, rettype, function: f)
  let sym = newSymbol(scope, $funcname, semexpr)
  let semid = newSemanticIdent(scope, sexpr.span, $funcname, argtypes.getSemanticTypeArgs)
  scope.module.addSymbol(semid, sym)
  return newSemanticExpr(sexpr.span, semanticSymbol, rettype, symbol: sym)

proc evalTypeAnnot*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let (_, _, funcdef) = parseTypeAnnotation(sexpr)
  if $funcdef.first == "defn":
    return evalFunction(scope, sexpr)
  elif $funcdef.first == "c-import":
    return evalCImport(scope, sexpr)
  elif $funcdef.first == "c-value":
    return evalCValue(scope, sexpr)
  else:
    sexpr.span.raiseError("couldn't apply type annotation: $#" % $sexpr)
proc evalGenericsAnnot*(scope: var Scope, sexpr: Sexpr): SEmanticExpr =
  var scope = scope
  sexpr.rest.each(e):
    if e.first.kind != sexprList and e.first.len < 2:
      sexpr.span.raiseError("generics parameter should be list")
    let protocolsemid = scope.newSemanticIdent(e.first.rest.first)
    let protocolsym = scope.getSymbol(protocolsemid)
    let semexpr = newSemanticExpr(
      sexpr.span,
      semanticGenerics,
      notTypeSym,
      generics: Generics(
        attr: $e.first,
        protocol: protocolsym,
        spec: none(Symbol)
      )
    )
    let sym = newSymbol(scope, $e.first.first, semexpr)
    let semid = newSemanticIdent(scope, e.first.first)
    if not scope.hasSemId(semid):
      scope.addSymbol(semid, sym)

    for fn in protocolsym.semexpr.protocol.funcs:
      let semexpr = newSemanticExpr(sexpr.span, semanticProtocolFunc, notTypeSym, protocolfntype: fn.fntype)
      let sym = newSymbol(scope, fn.name, semexpr)
      let semid = newSemanticIdent(scope, sexpr.span, fn.name, fn.fntype.argtypes.getSemanticTypeArgs)
      scope.addSymbol(semid, sym)

    if e.rest.rest.kind == sexprNil:
      break

  let typeannot = sexpr.last
  var retsym = scope.evalSExpr(typeannot)
  if retsym.kind == semanticSymbol and not (retsym.symbol == notTypeSym):
    if retsym.symbol.semexpr.kind == semanticFunction:
      retsym.symbol.semexpr.function.isGenerics = true
    elif retsym.symbol.semexpr.kind == semanticProtocol:
      retsym.symbol.semexpr.protocol.isGenerics = true
    elif retsym.symbol.semexpr.kind == semanticStruct:
      retsym.symbol.semexpr.struct.isGenerics = true
    elif retsym.symbol.semexpr.kind == semanticPrimitiveFunc:
      retsym.symbol.semexpr.primfunc.isGenerics = true
    elif retsym.symbol.semexpr.kind == semanticPrimitiveType:
      retsym.symbol.semexpr.primTypeIsGenerics = true
  return newSemanticExpr(sexpr.span, semanticSymbol, notTypeSym, symbol: notTypeSym)

proc evalProtocol*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let protocolname = $sexpr.rest.first
  var funcs = newSeq[tuple[name: string, fntype: FuncType]]()
  if sexpr.rest.rest.kind != sexprNil:
    for fn in sexpr.rest.rest:
      let name = fn.first
      let (argtypes, rettype, _) = getTypeAnnotation(scope, fn.rest.first)
      funcs.add(($name, FuncType(returntype: rettype, argtypes: argtypes)))
  let protocol = Protocol(funcs: funcs)
  let semexpr = newSemanticExpr(sexpr.span, semanticProtocol, notTypeSym, protocol: protocol)
  let sym = newSymbol(scope, protocolname, semexpr)
  let semid = newSemanticIdent(sym)
  scope.module.semanticidents.addSymbol(semid, sym)
  return newSemanticExpr(sexpr.span, semanticSymbol, notTypeSym, symbol: sym)

proc evalStruct*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let structname = $sexpr.rest.first
  var fields = newSeq[tuple[name: string, typesym: Symbol]]()
  for field in sexpr.rest.rest:
    let fieldname = $field.first
    let typesym = scope.getSymbol(scope.newSemanticIdent(field.rest.first))
    fields.add((fieldname, typesym))
  let struct = Struct(
    isGenerics: false,
    name: structname,
    fields: fields,
  )
  let semexpr = newSemanticExpr(sexpr.span, semanticStruct, notTypeSym, struct: struct)
  let sym = newSymbol(scope, structname, semexpr)
  scope.module.semanticidents.addSymbol(newSemanticIdent(sym), sym)
  return newSemanticExpr(sexpr.span, semanticSymbol, notTypeSym, symbol: sym)

proc evalVariable*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let name = $sexpr.rest.first
  let value = scope.evalSExpr(sexpr.rest.rest.first)
  let typesym = getType(value)
  let semexpr = newSemanticExpr(
    sexpr.span,
    semanticVariable,
    typesym,
    variable: Variable(
      name: name,
      value: value
    )
  )
  let sym = newSymbol(scope, name, semexpr)
  let semid = newSemanticIdent(sym)
  scope.addSymbol(semid, sym)
  return semexpr

proc evalIfExpr*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let cond = scope.evalSExpr(sexpr.rest.first)
  let tbody = scope.evalSExpr(sexpr.rest.rest.first)
  let fbody = scope.evalSExpr(sexpr.rest.rest.rest.first)
  let condtypesym = getType(cond)
  if not (condtypesym == scope.getSymbol(scope.newSemanticIdent(cond.span, "Bool", @[]))):
    sexpr.span.raiseError("cond expression is not Bool type")
  let ttypesym = getType(tbody)
  let ftypesym = getType(fbody)
  let typesym = if ttypesym == ftypesym:
                  ttypesym
                else:
                  notTypeSym
  let semexpr = newSemanticExpr(
    sexpr.span,
    semanticIfExpr,
    typesym,
    ifexpr: IfExpr(cond: cond, tbody: tbody, fbody: fbody),
  )
  return semexpr

proc predefine*(scope: var Scope) =
  scope.defPrimitiveEval(internalSpan, "c-import", evalCImport)
  scope.defPrimitiveEval(internalSpan, "c-type", evalCType)
  scope.defPrimitiveEval(internalSpan, "c-emit", evalCEmit)
  scope.defPrimitiveEval(internalSpan, ":", evalTypeAnnot)
  scope.defPrimitiveEval(internalSpan, "^", evalGenericsAnnot)
  scope.defPrimitiveEval(internalSpan, "defprotocol", evalProtocol)
  scope.defPrimitiveEval(internalSpan, "defstruct", evalStruct)
  scope.defPrimitiveEval(internalSpan, "var", evalVariable)
  scope.defPrimitiveEval(internalSpan, "if", evalIfExpr)
