
import sast
import semantic

import strutils, sequtils
import options

proc evalCFunc*(scope: var Scope, sexpr: SExpr): SemanticExpr =
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

proc genDestructor*(scope: var Scope, span: Span, garbage: SemanticExpr, res: var seq[SemanticExpr]) =
  if garbage.typesym.semexpr.kind == semanticStruct:
    for field in garbage.typesym.semexpr.struct.fields:
      let fieldgarbage = newSemanticExpr(
        span,
        semanticFieldAccess,
        field.typesym,
        fieldaccess: FieldAccess(
          valuesym: garbage,
          fieldname: field.name
        )
      )
      scope.genDestructor(span, fieldgarbage, res)
    let trysym = scope.tryType(span, "destructor", @[garbage.typesym])
    if trysym.isSome:
      res.add(scope.evalFuncCall(span, trysym.get, @[garbage],  @[garbage.typesym]))
  elif garbage.kind == semanticSymbol:
    let trysym = scope.tryType(span, "destructor", @[garbage.typesym])
    if trysym.isSome:
      res.add(scope.evalFuncCall(span, trysym.get, @[garbage],  @[garbage.typesym]))
  else:
    let trysym = scope.tryType(span, "destructor", @[garbage.typesym])
    if trysym.isSome:
      res.add(scope.evalFuncCall(span, trysym.get, @[garbage],  @[garbage.typesym]))

proc evalBody*(parentscope: var Scope, scope: var Scope, rettype: Symbol, sexpr: SExpr): seq[SemanticExpr] =
  result = @[]
  if sexpr.len > 1:
    sexpr.each(e):
      result.add(scope.evalSExpr(e.first))
      if e.rest.rest.kind == sexprNil:
        break
  let lastsemexpr = scope.evalSExpr(sexpr.last)
  if scope.isReturnType(lastsemexpr.typesym, rettype):
    lastsemexpr.refinc
  else:
    result.add(lastsemexpr)
  
  let (survived, garbages) = scope.getScopeValues()
  for survive in survived:
    parentscope.addScopeValue(survive)
  for garbage in garbages:
    scope.genDestructor(sexpr.span, newSemanticExpr(garbage), result)

  if scope.isReturnType(lastsemexpr.typesym, rettype):
    result.add(lastsemexpr)

proc evalFunction*(parentscope: var Scope, sexpr: SExpr): SemanticExpr =
  let (argtypes, rettype, funcdef) = parentscope.getTypeAnnotation(sexpr)
  let funcname = funcdef.rest.first
  var argnames = newSeq[string]()
  for arg in funcdef.rest.rest.first:
    argnames.add($arg)

  var nullscope = newScope(parentscope.module) # FIXME: ctref transfer to parent to black hole
  var scope = parentscope
  scope.addArgSymbols(argtypes, funcdef)

  let f = Function(
    isGenerics: false,
    argnames: argnames,
    fntype: FuncType(
      returntype: rettype,
      argtypes: argtypes,
    ),
    body: evalBody(nullscope, scope, rettype, funcdef.rest.rest.rest),
  )
  let semexpr = newSemanticExpr(sexpr.span, semanticFunction, rettype, function: f)
  let sym = newSymbol(scope, $funcname, semexpr)
  f.sym = sym
  let semid = newSemanticIdent(scope, sexpr.span, $funcname, argtypes.getSemanticTypeArgs)
  scope.module.addSymbol(semid, sym)
  return newSemanticExpr(sexpr.span, semanticSymbol, rettype, symbol: sym)

proc evalTypeAnnot*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let (_, _, funcdef) = parseTypeAnnotation(sexpr)
  if $funcdef.first == "defn":
    return evalFunction(scope, sexpr)
  elif $funcdef.first == "c-func":
    return evalCFunc(scope, sexpr)
  elif $funcdef.first == "c-value":
    return evalCValue(scope, sexpr)
  else:
    let typ = scope.tryType(sexpr.rest.first)
    result = scope.evalSExpr(sexpr.last)
    result.typesym = typ.get # FIXME:
    if result.kind == semanticVariable:
      result.variable.value.typesym = typ.get

proc evalGenericsAnnot*(parentscope: var Scope, sexpr: Sexpr): SEmanticExpr =
  var scope = parentscope
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
      let semexpr = newSemanticExpr(sexpr.span, semanticProtocolFunc, fn.fntype.returntype, protocolfntype: fn.fntype)
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
      retsym.symbol.semexpr.primtype.isGenerics = true
  return newSemanticExpr(sexpr.span, semanticSymbol, notTypeSym, symbol: notTypeSym)

proc evalProtocol*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let protocolname = $sexpr.rest.first
  var funcs = newSeq[tuple[name: string, fntype: FuncType]]()
  if sexpr.rest.rest.kind != sexprNil:
    for fn in sexpr.rest.rest:
      let name = fn.first
      let (argtypes, rettype, _) = getTypeAnnotation(scope, fn.rest.first, isAnnot = false)
      funcs.add(($name, FuncType(returntype: rettype, argtypes: argtypes)))
  let protocol = Protocol(funcs: funcs)
  let semexpr = newSemanticExpr(sexpr.span, semanticProtocol, notTypeSym, protocol: protocol)
  let sym = newSymbol(scope, protocolname, semexpr)
  let semid = newSemanticIdent(sym)
  scope.module.addSymbol(semid, sym)
  return newSemanticExpr(sexpr.span, semanticSymbol, notTypeSym, symbol: sym)

proc evalStruct*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let typeexpr = sexpr.rest.first
  let structname = if typeexpr.kind == sexprIdent:
                     $typeexpr
                   else:
                     $typeexpr.first
  var argtypes = newSeq[Symbol]()
  if typeexpr.kind == sexprList:
    for gene in typeexpr.rest:
      argtypes.add(scope.getSymbol(scope.newSemanticIdent(gene)))
  var fields = newSeq[tuple[name: string, typesym: Symbol]]()
  for field in sexpr.rest.rest:
    let fieldname = $field.first
    let typesym = scope.tryType(field.rest.first)
    if typesym.isNone:
      field.rest.first.span.raiseError("undeclared type: $#" % $field.rest.first)
    fields.add((fieldname, typesym.get))
  let struct = Struct(
    isGenerics: false,
    argtypes: argtypes,
    fields: fields,
  )
  let semexpr = newSemanticExpr(sexpr.span, semanticStruct, notTypeSym, struct: struct)
  let sym = newSymbol(scope, structname, semexpr)
  struct.sym = sym
  semexpr.typesym = sym
  let semid = scope.newSemanticIdent(sexpr.span, structname, argtypes.getSemanticTypeArgs())
  scope.module.addSymbol(semid, sym)
  return newSemanticExpr(sexpr.span, semanticSymbol, notTypeSym, symbol: sym)

proc evalStructConstructor*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let sym = scope.tryType(sexpr.rest.first).get
  var struct: SemanticExpr
  if sym.semexpr.kind == semanticStruct:
    struct = sym.semexpr
  elif sym.semexpr.kind == semanticTypeGenerics:
    struct = sym.semexpr.typegenerics.typ.semexpr
  else:
    sexpr.rest.first.span.raiseError("$# is not type, actually $#" % [$sexpr.rest.first, $sym.semexpr.kind])
  let semexpr = newSemanticExpr(
    sexpr.span,
    semanticStructConstructor,
    sym,
    structconstructor: StructConstructor(structsym: sym, values: @[])
  )
  for field in struct.struct.fields:
    let attr = sexpr.getAttr(field.name)
    if attr.isNone:
      sexpr.span.raiseError("needs initialzie $# field." % field.name)
    semexpr.structconstructor.values.add((field.name, scope.evalSExpr(attr.get)))
  return semexpr

proc evalVariable*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let name = $sexpr.rest.first
  let value = scope.evalSExpr(sexpr.rest.rest.first)
  let typesym = value.typesym
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
  
  if value.canOwner:
    semexpr.refcounter = CTRefCounter(kind: ctrefOwner, count: 0)
    scope.addScopeValue(sym)
  else:
    semexpr.refcounter = CTRefCounter(kind: ctrefBorrow, owner: value.refcounter)
  semexpr.refinc

  return semexpr

proc evalIfExpr*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let cond = scope.evalSExpr(sexpr.rest.first)
  let tbody = scope.evalSExpr(sexpr.rest.rest.first)
  let fbody = scope.evalSExpr(sexpr.rest.rest.rest.first)
  let condtypesym = cond.typesym
  if not (condtypesym == scope.getSymbol(scope.newSemanticIdent(cond.span, "Bool", @[]))):
    sexpr.span.raiseError("cond expression is not Bool type")
  let ttypesym = tbody.typesym
  let ftypesym = fbody.typesym
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

proc evalWhileSyntax*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let cond = scope.evalSExpr(sexpr.rest.first)
  var body = newSeq[SemanticExpr]()
  for e in sexpr.rest.rest:
    body.add(scope.evalSExpr(e))
  let semexpr = newSemanticExpr(
    sexpr.span,
    semanticWhileSyntax,
    notTypeSym,
    whilesyntax: WhileSyntax(cond: cond, body: body),
  )
  return semexpr

proc evalSetSyntax*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let leftval = scope.evalSExpr(sexpr.rest.first)
  # if variable.kind != semanticSymbol:
  #   sexpr.rest.first.span.raiseError("this is not variable")
  let value = scope.evalSExpr(sexpr.rest.rest.first)
  # if not (variable.typesym == value.typesym):
  #   value.raiseError("illegal set (set! $# $#)" % [$variable.typesym, $value.typesym])
  if leftval.kind == semanticFuncCall and leftval.funccall.callfunc.semexpr.kind == semanticFunction:
    var args = leftval.funccall.args
    args.add(value)
    let argtypes = args.mapIt(it.typesym)
    let setfuncsym = scope.tryType(leftval.span, "set-$#!" % leftval.funccall.callfunc.name, argtypes)
    if setfuncsym.isNone:
      sexpr.span.raiseError("undeclared (set-$#! $#) function" % [leftval.funccall.callfunc.name, argtypes.join(" ")])
    let semexpr = newSemanticExpr(
      leftval.span,
      semanticFuncCall,
      notTypeSym,
      funccall: FuncCall(
        callfunc: setfuncsym.get,
        args: args
      )
    )
    return semexpr
  else:
    let semexpr = newSemanticExpr(
      sexpr.span,
      semanticSetSyntax,
      notTypeSym,
      setsyntax: SetSyntax(variable: leftval, value: value),
    )
    return semexpr

proc evalRequire*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let modulename = $sexpr.rest.first
  let modulepath = modulename.replace(".", "/")
  let module = scope.module.context.evalFile(modulepath)
  let asattr = sexpr.getAttr("as")
  let referattr = sexpr.getAttr("refer")
  let prefixname = if referattr.isSome and referattr.get.attr == "all":
                     ""
                   elif asattr.isSome:
                     asattr.get.strval & "."
                   else:
                     modulename & "."
  for semid, sym in module.semidsymbols:
    if not sym.isImported:
      var importsym = sym.scope.newSymbol(sym.name, sym.semexpr, isImported = true)
      let importname = prefixname & semid.name
      var importsemid = sym.scope.newSemanticIdent(sexpr.rest.first.span, importname, semid.args)
      scope.module.addSymbol(importsemid, importsym)
  return newSemanticExpr(sexpr.span, semanticRequire, notTypeSym, requireModule: module)

proc predefine*(scope: var Scope) =
  scope.defPrimitiveEval(internalSpan, "require", evalRequire)
  scope.defPrimitiveEval(internalSpan, "c-func", evalCFunc)
  scope.defPrimitiveEval(internalSpan, "c-type", evalCType)
  scope.defPrimitiveEval(internalSpan, "c-emit", evalCEmit)
  scope.defPrimitiveEval(internalSpan, ":", evalTypeAnnot)
  scope.defPrimitiveEval(internalSpan, "^", evalGenericsAnnot)
  scope.defPrimitiveEval(internalSpan, "defprotocol", evalProtocol)
  scope.defPrimitiveEval(internalSpan, "defstruct", evalStruct)
  scope.defPrimitiveEval(internalSpan, "construct", evalStructConstructor)
  scope.defPrimitiveEval(internalSpan, "var", evalVariable)
  scope.defPrimitiveEval(internalSpan, "if", evalIfExpr)
  scope.defPrimitiveEval(internalSpan, "while", evalWhileSyntax)
  scope.defPrimitiveEval(internalSpan, "set!", evalSetSyntax)
