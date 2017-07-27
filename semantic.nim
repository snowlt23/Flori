
import tables, hashes
import strutils, sequtils
import sast
import options
import macros

type
  SemanticError* = object of Exception
  Symbol* = object
    module*: Module
    hash*: string
  Metadata* = object
    metatable*: Table[string, SemanticExpr]
  PrimitiveFuncKind* = enum
    primitiveCall
    primitiveInfix
  SemanticExprKind* = enum
    semanticSymbol
    semanticVariable
    semanticGenerics
    semanticGenericsSpec
    semanticIfExpr
    semanticFunction
    semanticMacro
    semanticStruct
    semanticPrimitiveType
    semanticPrimitiveValue
    semanticPrimitiveFunc
    semanticPrimitiveMacro
    semanticPrimitiveEval
    semanticModule
    semanticFuncCall
    semanticInt
    semanticString
  SemanticExpr* = ref object
    sexpr*: SExpr
    metadata*: Metadata
    typesym*: Symbol
    case kind*: SemanticExprKind
    of semanticSymbol:
      symbol*: Symbol
    of semanticVariable:
      variable*: Variable
    of semanticGenerics:
      generics*: Generics
    of semanticGenericsSpec:
      genericssym*: Symbol
      genericsparams*: seq[Symbol]
    of semanticIfExpr:
      ifexpr*: IfExpr
    of semanticFunction:
      function*: Function
    of semanticMacro: # TODO:
      discard
    of semanticStruct:
      struct*: Struct
    of semanticPrimitiveType:
      primTypeGenerics*: seq[Symbol]
      primTypeName*: string
    of semanticPrimitiveValue:
      primValue*: string
    of semanticPrimitiveFunc:
      primFuncKind*: PrimitiveFuncKind
      primFuncName*: string
      primFuncRetType*: string
    of semanticPrimitiveMacro:
      macroproc*: proc (scope: var Scope, sexpr: SExpr): SExpr
    of semanticPrimitiveEval:
      evalproc*: proc (scope: var Scope, sexpr: SExpr): SemanticExpr
    of semanticModule:
      module*: Module
    of semanticFuncCall:
      funccall*: FuncCall
    of semanticInt:
      intval*: int64
    of semanticString:
      strval*: string
  Variable* = ref object
    name*: string
    value*: SemanticExpr
  Generics* = ref object
    attr*: string
    protocol*: Symbol
  IfExpr* = ref object
    cond*: SemanticExpr
    tbody*: SemanticExpr
    fbody*: SemanticExpr
  Function* = ref object
    hash*: string
    name*: string
    argnames*: seq[string]
    argtypes*: seq[Symbol]
    rettype*: Symbol
    body*: seq[SemanticExpr]
  Struct* = ref object
    name*: string
    fields*: seq[tuple[name: string, typesym: Symbol]]
  Cffi* = ref object
    name*: string
    primname*: string
    argtypes*: seq[Symbol]
    rettype*: Symbol
  CCodegenInfo* = object
    headers*: OrderedTable[string, bool]
    decls*: seq[string]
    cffis*: seq[Cffi]
  Module* = ref object
    context*: SemanticContext
    name*: string
    semanticexprs*: OrderedTable[Symbol, SemanticExpr]
    toplevelcalls*: seq[SemanticExpr]
    exportedsymbols*: seq[Symbol]
    ccodegenInfo*: CCodegenInfo
  FuncCall* = ref object
    callfunc*: Symbol
    args*: seq[SemanticExpr]
  Scope* = object
    module*: Module
    scopesymbols*: OrderedTable[Symbol, SemanticExpr]
  SemanticContext* = ref object
    modules*: OrderedTable[string, Module]
    symcount*: int

proc newModule*(modulename: string): Module
proc evalSExpr*(scope: var Scope, sexpr: SExpr): SemanticExpr
proc getType*(scope: Scope, semexpr: SemanticExpr): Symbol
proc addSymbol*(scope: var Scope, sym: Symbol, value: SemanticExpr)
proc getSymbol*(scope: Scope, name: string): Symbol
proc getSemanticExpr*(scope: Scope, sym: Symbol): SemanticExpr
proc trySemanticExpr*(scope: Scope, sym: Symbol): Option[SemanticExpr]
proc addArgSymbols*(scope: var Scope, argtypesyms: seq[Symbol], funcdef: SExpr)

let globalModule* = newModule("global")
let notTypeSym* = Symbol(module: globalModule, hash: "not_type_symbol")
let notTypeSemExpr* = SemanticExpr(typesym: notTypeSym, kind: semanticSymbol, symbol: notTypeSym)

proc `$`*(symbol: Symbol): string =
  symbol.hash
proc hash*(symbol: Symbol): Hash =
  hash(symbol.hash)
proc `==`*(a, b: Symbol): bool =
  a.module.name == b.module.name and a.hash == b.hash
proc `==`*(symbol: Symbol, s: string): bool =
  symbol == Symbol(module: globalModule, hash: s)

proc newMetadata*(): MetaData =
  result.metatable = initTable[string, SemanticExpr]()

macro newSemanticExpr*(sexpr: SExpr, kind: SemanticExprKind, body: varargs[untyped]): SemanticExpr =
  let tmpid= genSym(nskVar, "tmp")
  result = quote do:
    var `tmpid` = SemanticExpr(kind: `kind`, sexpr: `sexpr`, typesym: notTypeSym, metadata: newMetadata())
  for b in body:
    expectKind(b, nnkExprColonExpr)
    let name = b[0]
    let value = b[1]
    result.add(quote do:
      `tmpid`.`name` = `value`
    )
  result.add(tmpid)
proc raiseError*(semexpr: SemanticExpr, s: string) =
  raise newException(SemanticError, "($#:$#) " & s % [$semexpr.sexpr.span.line, $semexpr.sexpr.span.linepos])
proc raiseError*(sexpr: SExpr, s: string) =
  raise newException(SemanticError, "($#:$#) " & s % [$sexpr.span.line, $sexpr.span.linepos])
proc expectSemantic*(semexpr: SemanticExpr, kind: SemanticExprKind) =
  if semexpr.kind != kind:
    semexpr.raiseError("expression is not $#" % $kind)
proc addMetadata*(semexpr: SemanticExpr, name: string, meta: SemanticExpr) =
  semexpr.metadata.metatable[name] = meta
proc getMetadata*(semexpr: SemanticExpr, name: string): SemanticExpr =
  if not semexpr.metadata.metatable.hasKey(name):
    semexpr.raiseError("expression hasn't `$#` metadata" % name)
  semexpr.metadata.metatable[name]

proc addSymbol*(module: Module, sym: Symbol, value: SemanticExpr) =
  if module.semanticexprs.hasKey(sym):
    raise newException(SemanticError, "couldn't redefine symbol: $#" % $sym)
  module.semanticexprs[sym] = value
proc getSymbol*(module: Module, symname: string): Symbol =
  let sym = Symbol(module: globalModule, hash: symname)
  if not module.semanticexprs.hasKey(sym):
    raise newException(SemanticError, "couldn't find symbol: $#" % symname)
  return sym
proc addStruct*(module: Module, sexpr: SExpr, struct: Struct) =
  let sym = Symbol(module: module, hash: struct.name)
  module.addSymbol(sym, newSemanticExpr(sexpr, semanticStruct, struct: struct))
proc addFunction*(module: Module, sexpr: SExpr, function: Function) =
  let sym = Symbol(module: module, hash: function.name & "_" & function.argtypes.mapIt($it).join("_"))
  module.addSymbol(sym, newSemanticExpr(sexpr, semanticFunction, function: function))
  module.exportedsymbols.add(sym)
proc addModule*(module: Module, sexpr: SExpr, importmodule: Module) =
  let sym = Symbol(module: module, hash: importmodule.name)
  module.addSymbol(sym, newSemanticExpr(sexpr, semanticModule, module: importmodule))
proc addToplevelCall*(module: Module, sexpr: SExpr, funccall: FuncCall) =
  module.toplevelcalls.add(newSemanticExpr(sexpr, semanticFuncCall, funccall: funccall))
proc addCffi*(module: Module, cffi: Cffi) =
  module.ccodegeninfo.cffis.add(cffi)
proc addDecl*(module: Module, decl: string) =
  module.ccodegeninfo.decls.add(decl)

proc newSemanticContext*(): SemanticContext =
  new result
  result.modules = initOrderedTable[string, Module]()

proc defPrimitiveType*(scope: var Scope, generics: seq[string], typename: string, primname: string) =
  scope.module.addSymbol(
    Symbol(module: globalModule, hash: typename),
    newSemanticExpr(newSNil(), semanticPrimitiveType, primTypeGenerics: generics.mapIt(getSymbol(scope, it)), primTypeName: primname)
  )
proc defPrimitiveValue*(scope: var Scope, typename: string, valuename: string, value: string) =
  var semexpr =newSemanticExpr(newSNil(), semanticPrimitiveValue, primValue: value)
  semexpr.typesym = scope.module.getSymbol(typename)
  scope.module.addSymbol(
    Symbol(module: globalModule, hash: valuename),
    semexpr
  )
proc defPrimitiveFunc*(scope: var Scope, funcname: string, rettype: string, kind: PrimitiveFuncKind, primname: string) =
  scope.module.addSymbol(
    Symbol(module: globalModule, hash: funcname),
    newSemanticExpr(newSNil(), semanticPrimitiveFunc, primFuncKind: kind, primFuncName: primname, primfuncRetType: rettype)
  )
proc defPrimitiveMacro*(scope: var Scope, macroname: string, macroproc: proc (scope: var Scope, sexpr: SExpr): SExpr) =
  scope.module.addSymbol(
    Symbol(module: globalModule, hash: macroname),
    newSemanticExpr(newSNil(), semanticPrimitiveMacro, macroproc: macroproc)
  )
proc defPrimitiveEval*(scope: var Scope, macroname: string, evalproc: proc (scope: var Scope, sexpr: SExpr): SemanticExpr) =
  scope.module.addSymbol(
    Symbol(module: globalModule, hash: macroname),
    newSemanticExpr(newSNil(), semanticPrimitiveEval, evalproc: evalproc)
  )

proc newCCodegenInfo*(): CCodegenInfo =
  result.headers = initOrderedTable[string, bool]()
  result.decls = @[]
proc addHeader*(info: var CCodegenInfo, name: string) =
  info.headers[name] = true

proc parseTypeAnnotation*(sexpr: SExpr): tuple[argtypes: seq[SExpr], rettype: SExpr, body: SExpr] =
  var argtypes = newSeq[SExpr]()
  var rettype: SExpr
  let body = sexpr.last
  for arg in sexpr.rest.list:
    if $arg.first == "->":
      rettype = arg.rest.first
      break
    elif arg.rest.kind == sexprNil:
      break
    else:
      argtypes.add(arg.first)
  if rettype == nil:
    rettype = ast(sexpr.span, newSIdent("Void"))
  result = (argtypes, rettype, body)
proc evalGenericsSpec*(scope: var Scope, sexpr: SExpr): Symbol =
  let genericsname = sexpr.first
  let genericssym = scope.getSymbol($genericsname)
  var genericsparams = newSeq[Symbol]()
  for generics in sexpr.rest:
    genericsparams.add(scope.getSymbol($generics))
  let sym = Symbol(module: scope.module, hash: $genericsname & "_" & genericsparams.mapIt($it).join("_"))
  scope.addSymbol(sym, newSemanticExpr(sexpr, semanticGenericsSpec, genericssym: genericssym, genericsparams: genericsparams))
  return sym
proc getTypeAnnotation*(scope: var Scope, sexpr: SExpr): tuple[argtypes: seq[Symbol], rettype: Symbol, body: SExpr] =
  let (argtypes, rettype, body) = parseTypeAnnotation(sexpr)
  var argtypesyms = newSeq[Symbol]()
  for argtype in argtypes:
    if argtype.kind == sexprList:
      argtypesyms.add(evalGenericsSpec(scope, argtype))
    else:
      argtypesyms.add(scope.getSymbol($argtype))
  let rettypesym = if rettype.kind == sexprList:
                     evalGenericsSpec(scope, rettype)
                   else:
                     scope.getSymbol($rettype)
  result = (argtypesyms, rettypesym, body)

proc getHashFromTypes*(scope: Scope, name: string, argtypes: seq[Symbol]): string =
  return name & "_" & argtypes.mapIt($it).join("_")
proc getHashFromFuncCall*(scope: Scope, name: string, args: seq[SemanticExpr]): string =
  var types = newSeq[Symbol]()
  for arg in args:
     types.add(scope.getType(arg))
  return name & "_" & types.mapIt($it).join("_")

proc cimportMacroExpand*(scope: var Scope, sexpr: SExpr): SExpr =
  let (argtypesyms, rettypesym, cffidef) = getTypeAnnotation(scope, sexpr)
  let funcname = cffidef.rest.first
  let nameattr = cffidef.getAttr("name")
  let headerattr = cffidef.getAttr("header")
  let nodeclattr = cffidef.hasAttr("nodecl")
  let primname = if nameattr.isSome:
                   $nameattr.get.strval
                 else:
                   $funcname
  let hash = scope.getHashFromTypes($funcname, argtypesyms)
  if nodeclattr:
    scope.defPrimitiveFunc(hash, $rettypesym, primitiveCall, primname)
  elif headerattr.isSome:
    scope.module.ccodegeninfo.addHeader(headerattr.get.strval)
    scope.defPrimitiveFunc(hash, $rettypesym, primitiveCall, primname)
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
  let argtypesyms = argtypes.mapIt(scope.getSymbol($it))
  var argnames = newSeq[string]()
  for arg in funcdef.rest.rest.first:
    argnames.add($arg)

  var scope = scope
  scope.addArgSymbols(argtypesyms, funcdef)
  let hash = scope.getHashFromTypes($funcname, argtypesyms)
  let f = Function(
    hash: hash,
    name: $funcname,
    argnames: argnames,
    argtypes: argtypesyms,
    rettype: scope.getSymbol($rettype),
    body: scope.evalFunctionBody(funcdef.rest.rest.rest),
  )
  scope.module.addFunction(sexpr, f)
  return newSemanticExpr(sexpr, semanticSymbol, symbol: Symbol(module: scope.module, hash: hash))

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
    scope.addSymbol(Symbol(module: scope.module, hash: $curexpr.first), semexpr)
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
    let typesym = scope.getSymbol($field.rest.first)
    fields.add((fieldname, typesym))
  let struct = Struct(
    name: structname,
    fields: fields,
  )
  scope.module.addStruct(sexpr, struct)
  return notTypeSemExpr

proc evalVariable*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let name = $sexpr.rest.first
  let value = scope.evalSExpr(sexpr.rest.rest.first)
  let typesym = scope.getType(value)
  scope.addSymbol(Symbol(module: scope.module, hash: name), value)
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
  if condtypesym != scope.getSymbol("Bool"):
    raise newException(SemanticError, "($#:$#) cond expression is not Bool type" % [$sexpr.span.line, $sexpr.span.linepos])
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

proc predefined*(scope: var Scope) =
  scope.defPrimitiveFunc("+_Int32_Int32", "Int32", primitiveInfix, "+")
  scope.defPrimitiveMacro("c-import", cimportMacroExpand)
  scope.defPrimitiveMacro("c-type", ctypeMacroExpand)
  scope.defPrimitiveEval(":", evalTypeAnnot)
  scope.defPrimitiveEval("^", evalGenericsAnnot)
  scope.defPrimitiveEval("defstruct", evalStruct)
  scope.defPrimitiveEval("var", evalVariable)
  scope.defPrimitiveEval("if", evalIfExpr)
proc newModule*(modulename: string): Module =
  new result
  result.name = modulename
  result.semanticexprs = initOrderedTable[Symbol, SemanticExpr]()
  result.toplevelcalls = @[]
  result.exportedsymbols = @[]
  result.ccodegenInfo = newCCodegenInfo()

proc newScope*(module: Module): Scope =
  result.module = module
  result.scopesymbols = initOrderedTable[Symbol, SemanticExpr]()
proc addSymbol*(scope: var Scope, sym: Symbol, value: SemanticExpr) =
  if scope.scopesymbols.hasKey(sym):
    raise newException(SemanticError, "couldn't redefine $#" % $sym)
  scope.scopesymbols[sym] = value
proc getSymbol*(scope: Scope, name: string): Symbol =
  let sym = Symbol(module: scope.module, hash: name)
  let globalsym = Symbol(module: globalModule, hash: name)
  if scope.scopesymbols.hasKey(sym):
    return sym
  elif scope.module.semanticexprs.hasKey(sym):
    return sym
  elif scope.scopesymbols.hasKey(globalsym):
    return globalsym
  elif scope.module.semanticexprs.hasKey(globalsym):
    return globalsym
  else:
    raise newException(SemanticError, "couldn't find symbol: $#" % name)
proc getSemanticExpr*(scope: Scope, sym: Symbol): SemanticExpr =
  if scope.scopesymbols.hasKey(sym):
    return scope.scopesymbols[sym]
  else:
    return scope.module.semanticexprs[sym]
proc trySemanticExpr*(scope: Scope, sym: Symbol): Option[SemanticExpr] =
  if scope.scopesymbols.hasKey(sym):
    return some(scope.scopesymbols[sym])
  elif scope.module.semanticexprs.hasKey(sym):
    return some(scope.module.semanticexprs[sym])
  else:
    return none(SemanticExpr)
proc addArgSymbols*(scope: var Scope, argtypesyms: seq[Symbol], funcdef: SExpr) =
  if funcdef.rest.rest.first.len != argtypesyms.len:
    raise newException(SemanticError, "($#:$#) argument length is not equals type length" % [$funcdef.span.line, $funcdef.span.linepos])
  for i, arg in funcdef.rest.rest.first:
    var semexpr = newSemanticExpr( funcdef, semanticSymbol, symbol: Symbol(module: scope.module, hash: $arg))
    semexpr.typesym = argtypesyms[i]
    scope.addSymbol(Symbol(module: scope.module, hash: $arg), semexpr)

proc getType*(scope: Scope, semexpr: SemanticExpr): Symbol =
  return semexpr.typesym
proc getRetType*(scope: Scope, sym: Symbol): Symbol =
  let semexpr = scope.getSemanticExpr(sym)
  if semexpr.kind == semanticFunction:
    return semexpr.function.rettype
  elif semexpr.kind == semanticPrimitiveFunc:
    return scope.getSymbol(semexpr.primFuncRetType)
  else:
    raise newException(SemanticError, "expression is not function")

proc evalFuncCall*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let semexpr = scope.trySemanticExpr(Symbol(module: globalModule, hash: $sexpr.first))
  if semexpr.isSome and semexpr.get.kind == semanticPrimitiveMacro:
    return scope.evalSExpr(semexpr.get.macroproc(scope, sexpr))
  if semexpr.isSome and semexpr.get.kind == semanticPrimitiveEval:
    return semexpr.get.evalproc(scope, sexpr)
  else:
    var args = newSeq[SemanticExpr]()
    for arg in sexpr.rest:
      args.add(scope.evalSExpr(arg))
    # TODO: support generics
    let sym = scope.getSymbol(scope.getHashFromFuncCall($sexpr.first, args))
    result = newSemanticExpr(
      sexpr,
      semanticFuncCall,
      funccall: FuncCall(
        callfunc: sym,
        args: args,
      ),
    )
    result.typesym = scope.getRetType(sym)

proc evalInt*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  result = newSemanticExpr(sexpr, semanticInt, intval: sexpr.intval)
  result.typesym = scope.getSymbol("Int32")
proc evalString*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  result = newSemanticExpr(sexpr, semanticString, strval: sexpr.strval)
  result.typesym = scope.getSymbol("CString")

proc evalSExpr*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  case sexpr.kind
  of sexprNil:
    return notTypeSemExpr
  of sexprList:
    return scope.evalFuncCall(sexpr)
  of sexprIdent:
    let sym = scope.getSymbol($sexpr)
    var semexpr = newSemanticExpr(sexpr, semanticSymbol, symbol: sym)
    semexpr.typesym = scope.getSemanticExpr(sym).typesym
    return semexpr
  of sexprInt:
    return scope.evalInt(sexpr)
  of sexprString:
    return scope.evalString(sexpr)
  else:
    raise newException(SemanticError, "($#:$#) $# is can't eval: $#" % [$sexpr.span.line, $sexpr.span.linepos, $sexpr.kind, $sexpr])

proc evalModule*(context: SemanticContext, modulename: string, sexpr: seq[SExpr]) =
  let modulename = modulename.replace("/", "_").replace("\\", "_")
  var module = newModule(modulename)
  var scope = newScope(module)
  scope.predefined()
  context.modules[modulename] = module
  for e in sexpr:
    let semexpr = scope.evalSExpr(e)
    if semexpr.kind == semanticSymbol and semexpr.symbol == notTypeSym:
      discard
    else:
      module.toplevelcalls.add(semexpr)
