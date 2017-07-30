
import sast
import semantic

import strutils, sequtils
import options

proc evalCImport*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let (argtypesyms, rettypesym, cffidef) = getTypeAnnotation(scope, sexpr)
  let funcname = cffidef.rest.first
  let nameattr = cffidef.getAttr("name")
  let headerattr = cffidef.getAttr("header")
  let nodeclattr = cffidef.hasAttr("nodecl")
  let infixattr = cffidef.hasAttr("infix")
  let primname = if nameattr.isSome:
                   $nameattr.get.strval
                 else:
                   $funcname
  let primtype = if infixattr:
                   primitiveInfix
                 else:
                   primitiveCall
  let sym = newSymbol(sexpr, scope, $funcname, argtypesyms.mapIt(getSymbolArg(it)))
  if nodeclattr:
    scope.module.semanticexprs.addSymbol(
      sym,
      newSemanticExpr(
        sexpr.span,
        semanticPrimitiveFunc,
        primFuncKind: primtype,
        primFuncName: primname,
        primFuncRetType: rettypesym,
      )
    )
  elif headerattr.isSome:
    scope.module.ccodegeninfo.addHeader(headerattr.get.strval)
    scope.module.semanticexprs.addSymbol(
      sym,
      newSemanticExpr(
        sexpr.span,
        semanticPrimitiveFunc,
        primFuncKind: primtype,
        primFuncName: primname,
        primFuncRetType: rettypesym,
      )
    )
  else:
    let cffi = Cffi(
      name: $funcname,
      primname: primname,
      argtypes: argtypesyms,
      rettype: rettypesym,
    )
    scope.module.addCFFI(cffi)
  return notTypeSemExpr

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
  if nodeclattr:
    scope.defPrimitiveType(sexpr.span, generics, typename, primname)
  elif headerattr.isSome:
    scope.module.ccodegeninfo.addHeader(headerattr.get.strval)
    scope.defPrimitiveType(sexpr.span, generics, typename, primname)
  else:
    scope.module.addDecl("extern $#;" % primname)
    scope.defPrimitiveType(sexpr.span, generics, typename, primname)
  return notTypeSemExpr

proc evalCValue*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let (argtypes, _, vardef) = parseTypeAnnotation(sexpr)
  let varname = $vardef.rest.first
  let primattr = vardef.getAttr("name")
  let primname = if primattr.isSome:
                   primattr.get.strval
                 else:
                   varname
  scope.defPrimitiveValue(sexpr.span, $argtypes[0], varname, primname)
  return notTypeSemExpr

proc evalCEmit*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let format = sexpr.rest.first.strval
  var args = newSeq[SemanticExpr]()
  for se in sexpr.rest.rest:
    args.add(scope.evalSExpr(se))
  return newSemanticExpr(sexpr.span, semanticCExpr, cexpr: CExpr(format: format, args: args))

proc evalFunctionBody*(scope: var Scope, sexpr: SExpr): seq[SemanticExpr] =
  result = @[]
  for e in sexpr:
    result.add(scope.evalSExpr(e))
proc evalFunction*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let (argtypes, rettype, funcdef) = parseTypeAnnotation(sexpr)
  let funcname = funcdef.rest.first
  let argtypesyms = argtypes.mapIt(scope.getSymbol(it, $it))
  var argnames = newSeq[string]()
  for arg in funcdef.rest.rest.first:
    argnames.add($arg)

  var scope = scope
  scope.addArgSymbols(argtypesyms, funcdef)
  let sym = newSymbol(sexpr, scope, $funcname, argtypesyms.mapIt(getSymbolArg(it)))
  let f = Function(
    isGenerics: false,
    name: $funcname,
    argnames: argnames,
    fntype: FuncType(
      returntype: scope.getSymbol(rettype, $rettype),
      argtypes: argtypesyms,
    ),
    body: scope.evalFunctionBody(funcdef.rest.rest.rest),
  )
  scope.module.semanticexprs.addSymbol(sym, newSemanticExpr(sexpr.span, semanticFunction, function: f))
  return newSemanticExpr(sexpr.span, semanticSymbol, symbol: sym)

proc evalTypeAnnot*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let (argtypes, rettype, funcdef) = parseTypeAnnotation(sexpr)
  if $funcdef.first == "defn":
    return evalFunction(scope, sexpr)
  elif $funcdef.first == "c-import":
    discard evalCImport(scope, sexpr)
  elif $funcdef.first == "c-value":
    discard evalCValue(scope, sexpr)
  else:
    sexpr.raiseError("couldn't apply type annotation: $#" % $sexpr)
  return notTypeSemExpr
proc evalGenericsAnnot*(scope: var Scope, sexpr: Sexpr): SEmanticExpr =
  var scope = scope
  var curexpr = sexpr.rest
  while true:
    if curexpr.first.kind != sexprList:
      sexpr.raiseError("generics parameter should be list")
    let attrsym = newSymbol(curexpr, scope, $curexpr.first.first)
    let protocolsym = scope.getSymbol(curexpr.rest.first, $curexpr.first.rest.first)
    let semexpr = newSemanticExpr(
      sexpr.span,
      semanticGenerics,
      generics: Generics(
        attr: $curexpr.first,
        protocol: protocolsym,
        spec: none(Symbol)
      )
    )
    scope.addSymbol(attrsym, semexpr)

    let protocolsemexpr = getSemanticExpr(protocolsym)
    for fn in protocolsemexpr.protocol.funcs:
      scope.addSymbol(
        newSymbol(sexpr, scope, fn.name, fn.fntype.argtypes.mapIt(getSymbolArg(it))),
        newSemanticExpr(sexpr.span, semanticProtocolFunc, protocolfntype: fn.fntype)
      )
    if curexpr.rest.rest.kind == sexprNil:
      break
    curexpr = curexpr.rest
  let typeannot = sexpr.last
  var retsym = scope.evalSExpr(typeannot)
  if retsym.kind == semanticSymbol and not (retsym.symbol == notTypeSym):
    var retsemexpr = getSemanticExpr(retsym.symbol)
    if retsemexpr.kind == semanticFunction:
      retsemexpr.function.isGenerics = true
    elif retsemexpr.kind == semanticProtocol:
      retsemexpr.protocol.isGenerics = true
    elif retsemexpr.kind == semanticStruct:
      retsemexpr.struct.isGenerics = true
  return notTypeSemExpr

proc evalProtocol*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let protocolname = $sexpr.rest.first
  var funcs = newSeq[tuple[name: string, fntype: FuncType]]()
  if sexpr.rest.rest.kind != sexprNil:
    for fn in sexpr.rest.rest:
      let name = fn.first
      let (argtypes, rettype, _) = getTypeAnnotation(scope, fn.rest.first)
      funcs.add(($name, FuncType(returntype: rettype, argtypes: argtypes)))
  let protocol = Protocol(funcs: funcs)
  let sym = newSymbol(sexpr, scope, protocolname)
  scope.module.semanticexprs.addSymbol(sym, newSemanticExpr(sexpr.span, semanticProtocol, protocol: protocol))
  return notTypeSemExpr

proc evalStruct*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let structname = $sexpr.rest.first
  var fields = newSeq[tuple[name: string, typesym: Symbol]]()
  for field in sexpr.rest.rest:
    let fieldname = $field.first
    let typesym = scope.getSymbol(field, $field.rest.first)
    fields.add((fieldname, typesym))
  let sym = newSymbol(sexpr, scope, structname)
  let struct = Struct(
    isGenerics: false,
    name: structname,
    fields: fields,
  )
  scope.module.semanticexprs.addSymbol(sym, newSemanticExpr(sexpr.span, semanticStruct, struct: struct))
  return newSemanticExpr(sexpr.span, semanticSymbol, symbol: sym)

proc evalVariable*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let name = $sexpr.rest.first
  let value = scope.evalSExpr(sexpr.rest.rest.first)
  let typesym = getType(value)
  scope.addSymbol(newSymbol(sexpr, scope, name), value)
  result = newSemanticExpr(
    sexpr.span,
    semanticVariable,
    variable: Variable(
      name: name,
      value: value
    )
  )
  result.typesym = typesym

proc evalIfExpr*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let cond = scope.evalSExpr(sexpr.rest.first)
  let tbody = scope.evalSExpr(sexpr.rest.rest.first)
  let fbody = scope.evalSExpr(sexpr.rest.rest.rest.first)
  let condtypesym = getType(cond)
  if not (condtypesym == scope.getSymbol(newSNil(sexpr.span), "Bool")):
    sexpr.raiseError("cond expression is not Bool type")
  let ttypesym = getType(tbody)
  let ftypesym = getType(fbody)
  let typesym = if ttypesym == ftypesym:
                  ttypesym
                else:
                  notTypeSym
  result = newSemanticExpr(
    sexpr.span,
    semanticIfExpr,
    ifexpr: IfExpr(cond: cond, tbody: tbody, fbody: fbody),
  )
  result.typesym = typesym

proc predefine*(scope: var Scope) =
  scope.defPrimitiveEval(unknownSpan, "c-import", evalCImport)
  scope.defPrimitiveEval(unknownSpan, "c-type", evalCType)
  scope.defPrimitiveEval(unknownSpan, "c-emit", evalCEmit)
  scope.defPrimitiveEval(unknownSpan, ":", evalTypeAnnot)
  scope.defPrimitiveEval(unknownSpan, "^", evalGenericsAnnot)
  scope.defPrimitiveEval(unknownSpan, "defprotocol", evalProtocol)
  scope.defPrimitiveEval(unknownSpan, "defstruct", evalStruct)
  scope.defPrimitiveEval(unknownSpan, "var", evalVariable)
  scope.defPrimitiveEval(unknownSpan, "if", evalIfExpr)
