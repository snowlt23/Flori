
import sast
import semantic

import strutils, sequtils
import options

proc cimportMacroExpand*(scope: var Scope, sexpr: SExpr): SExpr =
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
  let sym = newSymbol(sexpr, scope.module, $funcname, argtypesyms.mapIt(scope.getSymbolArg(it)))
  if nodeclattr:
    scope.module.semanticexprs.addSymbol(
      sym,
      newSemanticExpr(
        sexpr,
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
        sexpr,
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
  return ast(sexpr.span, newSNil())
proc ctypeMacroExpand*(scope: var Scope, sexpr: SExpr): SExpr =
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
    scope.defPrimitiveType(generics, typename, primname)
  elif headerattr.isSome:
    scope.module.ccodegeninfo.addHeader(headerattr.get.strval)
    scope.defPrimitiveType(generics, typename, primname)
  else:
    scope.module.addDecl("extern $#;" % primname)
    scope.defPrimitiveType(generics, typename, primname)
  return ast(sexpr.span, newSNil())
proc cvalueMacroExpand*(scope: var Scope, sexpr: SExpr): SExpr =
  let (argtypes, _, vardef) = parseTypeAnnotation(sexpr)
  let varname = $vardef.rest.first
  let primattr = vardef.getAttr("name")
  let primname = if primattr.isSome:
                   primattr.get.strval
                 else:
                   varname
  scope.defPrimitiveValue($argtypes[0], varname, primname)

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
  let sym = newSymbol(sexpr, scope.module, $funcname, argtypesyms.mapIt(scope.getSymbolArg(it)))
  let f = Function(
    name: $funcname,
    argnames: argnames,
    argtypes: argtypesyms,
    rettype: scope.getSymbol(rettype, $rettype),
    body: scope.evalFunctionBody(funcdef.rest.rest.rest),
  )
  scope.module.semanticexprs.addSymbol(sym, newSemanticExpr(sexpr, semanticFunction, function: f))
  return newSemanticExpr(sexpr, semanticSymbol, symbol: sym)

proc evalTypeAnnot*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let (argtypes, rettype, funcdef) = parseTypeAnnotation(sexpr)
  if $funcdef.first == "defn":
    discard evalFunction(scope, sexpr)
  elif $funcdef.first == "c-import":
    discard cimportMacroExpand(scope, sexpr)
  elif $funcdef.first == "c-value":
    discard cvalueMacroExpand(scope, sexpr)
  else:
    sexpr.raiseError("couldn't apply type annotation: $#" % $sexpr)
  return notTypeSemExpr
proc evalGenericsAnnot*(scope: var Scope, sexpr: Sexpr): SEmanticExpr =
  var scope = scope
  var curexpr = sexpr.rest
  while true:
    let semexpr = newSemanticExpr(sexpr, semanticGenerics, generics: Generics(attr: $curexpr.first)) # TODO: protocol
    scope.addSymbol(newSymbol(curexpr, scope.module, $curexpr.first), semexpr)
    if curexpr.rest.kind == sexprNil:
      break
    curexpr = curexpr.rest.rest
  let typeannot = sexpr.last
  discard scope.evalSExpr(typeannot)
  # TODO:
  return notTypeSemExpr

proc evalStruct*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let structname = $sexpr.rest.first
  var fields = newSeq[tuple[name: string, typesym: Symbol]]()
  for field in sexpr.rest.rest:
    let fieldname = $field.first
    let typesym = scope.getSymbol(field, $field.rest.first)
    fields.add((fieldname, typesym))
  let sym = newSymbol(sexpr, scope.module, structname)
  let struct = Struct(
    name: structname,
    fields: fields,
  )
  scope.module.semanticexprs.addSymbol(sym, newSemanticExpr(sexpr, semanticStruct, struct: struct))
  return notTypeSemExpr

proc evalVariable*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let name = $sexpr.rest.first
  let value = scope.evalSExpr(sexpr.rest.rest.first)
  let typesym = scope.getType(value)
  scope.addSymbol(newSymbol(sexpr, scope.module, name), value)
  result = newSemanticExpr(
    sexpr,
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
  let condtypesym = scope.getType(cond)
  if not (condtypesym == scope.getSymbol(newSNil(), "Bool")):
    sexpr.raiseError("cond expression is not Bool type")
  let ttypesym = scope.getType(tbody)
  let ftypesym = scope.getType(fbody)
  let typesym = if ttypesym == ftypesym:
                  ttypesym
                else:
                  notTypeSym
  result = newSemanticExpr(
    sexpr,
    semanticIfExpr,
    ifexpr: IfExpr(cond: cond, tbody: tbody, fbody: fbody),
  )
  result.typesym = typesym

proc predefine*(scope: var Scope) =
  scope.defPrimitiveMacro("c-import", cimportMacroExpand)
  scope.defPrimitiveMacro("c-type", ctypeMacroExpand)
  scope.defPrimitiveEval(":", evalTypeAnnot)
  scope.defPrimitiveEval("^", evalGenericsAnnot)
  scope.defPrimitiveEval("defstruct", evalStruct)
  scope.defPrimitiveEval("var", evalVariable)
  scope.defPrimitiveEval("if", evalIfExpr)
