
import sast
import sparser
import semantic
import semanticeval
import compile

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
    return newSemanticExpr(sexpr.span, semanticNotType, rettype)

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
  return newSemanticExpr(sexpr.span, semanticSymbol, getTypeSymbol(sym), symbol: sym)

proc evalCValue*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let (argtypes, _, vardef) = parseTypeAnnotation(sexpr)
  let varname = $vardef.rest.first
  let primattr = vardef.getAttr("name")
  let primname = if primattr.isSome:
                   primattr.get.strval
                 else:
                   varname
  scope.defPrimitiveValue(sexpr.span, $argtypes[0], varname, primname)
  return newSemanticExpr(sexpr.span, semanticNotType, notTypeSym)

proc genDestructor*(scope: var Scope, span: Span, garbage: SemanticExpr, res: var seq[SemanticExpr]) =
  if garbage.typesym.getSemExpr().kind == semanticStruct:
    for field in garbage.typesym.getSemExpr().struct.fields:
      let fieldgarbage = newSemanticExpr(
        span,
        semanticFieldAccess,
        field.typesym.getSpecTypeSym(),
        fieldaccess: FieldAccess(
          valuesym: garbage,
          fieldname: field.name
        )
      )
      scope.genDestructor(span, fieldgarbage, res)
  let trysym = scope.trySymbol(scope.newSemanticIdent(span, "destructor", @[garbage.typesym]))
  if trysym.isSome:
    let funccall = scope.evalFuncCall(span, "destructor", @[garbage],  @[garbage.typesym])
    res.add(funccall)

proc evalBody*(parentscope: var Scope, scope: var Scope, rettype: TypeSymbol, sexpr: SExpr): seq[SemanticExpr] =
  result = @[]
  if sexpr.kind == sexprNil:
    return
  if sexpr.len > 1:
    sexpr.each(e):
      result.add(scope.evalSExpr(e.first))
      if e.rest.rest.kind == sexprNil:
        break
  let lastsemexpr = scope.evalSExpr(sexpr.last)
  if scope.isReturnType(lastsemexpr.typesym, rettype):
    lastsemexpr.refinc
  elif rettype.getSymbol().name == "Void":
    result.add(lastsemexpr)
  else:
    lastsemexpr.raiseError("return value is not $# type" % rettype.debug)
  
  let (survived, garbages) = scope.getScopeValues()
  for survive in survived:
    parentscope.addScopeValue(survive)
  for garbage in garbages:
    let semexpr = newSemanticExpr(sexpr.span, semanticSymbol, garbage.semexpr.typesym, symbol: garbage)
    scope.genDestructor(sexpr.span, semexpr, result)

  if scope.isReturnType(lastsemexpr.typesym, rettype):
    result.add(lastsemexpr)

proc evalFunction*(parentscope: var Scope, sexpr: SExpr, addglobal = true): SemanticExpr =
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
    body: nil,
  )
  let fsemexpr = newSemanticExpr(sexpr.span, semanticFunction, rettype, function: f)
  let fsym = newSymbol(scope, $funcname, fsemexpr)
  f.sym = fsym
  let fsemid = newSemanticIdent(scope, sexpr.span, $funcname, argtypes)

  let fdecl = FuncDecl(
    fntype: FuncType(
      returntype: rettype,
      argtypes: argtypes,
    ),
    fndef: some(fsym)
  )
  let declsemexpr = newSemanticExpr(sexpr.span, semanticFuncDecl, rettype, funcdecl: fdecl)
  let declsym = newSymbol(scope, $funcname, declsemexpr)
  let declsemid = newSemanticIdent(scope, sexpr.span, $funcname, argtypes)

  if addglobal:
    scope.module.addSymbol(declsemid, declsym)
  f.body = evalBody(nullscope, scope, rettype, funcdef.rest.rest.rest)
  if f.body.len == 0:
    f.isReturn = false
  elif scope.isReturnType(f.body[^1].typesym, rettype):
    f.isReturn = true
  else:
    f.isReturn = false
  scope.module.addSymbol(fsemid, fsym, rewrite = true)
  return newSemanticExpr(sexpr.span, semanticSymbol, rettype, symbol: fsym)

proc evalMacro*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let macroname = $sexpr.rest.first
  let funcsemexpr = scope.evalFunction(sexpr, addglobal = false)
  let semexpr = newSemanticExpr(sexpr.span, semanticMacro, funcsemexpr.typesym, semmacro: SemMacro(funcsemexpr: funcsemexpr))
  let sym = newSymbol(scope, macroname, semexpr)
  let semid = newSemanticIdent(scope, sexpr.span, $macroname, funcsemexpr.function.fntype.argtypes)
  scope.module.addSymbol(semid, sym)
  ctsharedHandle = scope.module.loadCompileTime()
  return newSemanticExpr(sexpr.span, semanticSymbol, funcsemexpr.function.fntype.returntype, symbol: sym)

proc evalTypeAnnot*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let (_, _, funcdef) = parseTypeAnnotation(sexpr)
  if $funcdef.first == "defn":
    return evalFunction(scope, sexpr)
  if $funcdef.first == "defmacro":
    return evalMacro(scope, sexpr)
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
  let curindex = scope.genindex
  sexpr.rest.each(e):
    if e.first.kind != sexprList and e.first.len < 2:
      sexpr.span.raiseError("generics parameter should be list")
    let protocolsemid = scope.newSemanticIdent(e.first.rest.first)
    let protocolsymopt = scope.trySymbol(protocolsemid)
    if protocolsymopt.isNone:
      e.first.rest.first.span.raiseError("undeclared protocol: $#" % $e.first.rest.first)
    let semexpr = newSemanticExpr(
      sexpr.span,
      semanticGenerics,
      notTypeSym,
      generics: Generics(
        scopeindex: curindex,
        attr: $e.first,
        protocol: protocolsymopt.get,
        children: @[]
      )
    )
    let sym = newSymbol(scope, $e.first.first, semexpr)
    semexpr.typesym = getTypeSymbol(sym)
    let semid = newSemanticIdent(scope, e.first.first)
    if scope.trySymbol(semid).isNone:
      scope.addSymbol(semid, sym)

    for fn in protocolsymopt.get.semexpr.protocol.funcs:
      let semexpr = newSemanticExpr(sexpr.span, semanticProtocolFunc, fn.fntype.returntype, protocolfntype: fn.fntype)
      let sym = newSymbol(scope, fn.name, semexpr)
      let semid = newSemanticIdent(scope, sexpr.span, fn.name, fn.fntype.argtypes)
      scope.addSymbol(semid, sym)

    if e.rest.rest.kind == sexprNil:
      break

  let typeannot = sexpr.last
  var retsym = scope.evalSExpr(typeannot)
  if retsym.kind == semanticSymbol:
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
  return newSemanticExpr(sexpr.span, semanticNotType, notTypeSym)

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
  let semid = scope.newSemanticIdent(sexpr.span, protocolname, @[])
  scope.module.addSymbol(semid, sym)
  return newSemanticExpr(sexpr.span, semanticSymbol, notTypeSym, symbol: sym)

proc evalStruct*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let typeexpr = sexpr.rest.first
  let structname = if typeexpr.kind == sexprIdent:
                     $typeexpr
                   else:
                     $typeexpr.first
  var argtypes = newSeq[TypeSymbol]()
  if typeexpr.kind == sexprList:
    for gene in typeexpr.rest:
      let genesymopt = scope.tryType(gene)
      if genesymopt.isNone:
        gene.span.raiseError("undeclared type param: $#" % $gene)
      argtypes.add(genesymopt.get)
  var fields = newSeq[tuple[name: string, typesym: TypeSymbol]]()
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
  semexpr.typesym = getTypeSymbol(sym)
  let semid = scope.newSemanticIdent(sexpr.span, structname, argtypes)
  scope.module.addSymbol(semid, sym)
  return newSemanticExpr(sexpr.span, semanticSymbol, getTypeSymbol(sym), symbol: sym)

proc evalStructConstructor*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let sym = scope.tryType(sexpr.rest.first).get
  var struct: SemanticExpr
  if sym.getSemExpr().kind == semanticStruct:
    struct = sym.getSemExpr()
  elif sym.getSemExpr().kind == semanticTypeGenerics:
    struct = sym.getSemExpr().typegenerics.typ.getSemExpr()
  else:
    sexpr.rest.first.span.raiseError("$# is not type, actually $#" % [$sexpr.rest.first, $sym.getSemExpr().kind])
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
      scope: scope,
      name: name,
      value: value
    )
  )

  let sym = newSymbol(scope, name, semexpr)
  let semid = scope.newSemanticIdent(sexpr.span, name, @[])
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
  if condtypesym.getSymbol().name != "Bool":
    sexpr.span.raiseError("cond expression is not Bool type")
  let ttypesym = tbody.typesym
  let ftypesym = fbody.typesym
  let typesym = if ttypesym == ftypesym:
                  ttypesym
                else:
                  scope.getVoidSym(sexpr.span)
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
    scope.getVoidSym(sexpr.span),
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
  if leftval.kind == semanticFuncCall and leftval.funccall.callfunc.getSemExpr().kind == semanticFunction:
    var args = leftval.funccall.args
    args.add(value)
    let argtypes = args.mapIt(it.typesym)
    let setfuncsym = scope.tryType(leftval.span, "set-$#!" % leftval.funccall.callfunc.getSymbol().name, argtypes)
    if setfuncsym.isNone:
      sexpr.span.raiseError("undeclared (set-$#! $#) function" % [leftval.funccall.callfunc.getSymbol().name, argtypes.mapIt($it).join(" ")])
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
      scope.getVoidSym(sexpr.span),
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
  scope.defPrimitiveEval(internalSpan, ":", evalTypeAnnot)
  scope.defPrimitiveEval(internalSpan, "^", evalGenericsAnnot)
  scope.defPrimitiveEval(internalSpan, "defprotocol", evalProtocol)
  scope.defPrimitiveEval(internalSpan, "defstruct", evalStruct)
  scope.defPrimitiveEval(internalSpan, "construct", evalStructConstructor)
  scope.defPrimitiveEval(internalSpan, "var", evalVariable)
  scope.defPrimitiveEval(internalSpan, "if", evalIfExpr)
  scope.defPrimitiveEval(internalSpan, "while", evalWhileSyntax)
  scope.defPrimitiveEval(internalSpan, "set!", evalSetSyntax)