
import tables, hashes
import strutils, sequtils
import sast
import options

type
  SemanticError* = object of Exception
  Symbol* = object
    module*: Module
    hash*: string
  PrimitiveFuncKind* = enum
    primitiveCall
    primitiveInfix
  SemanticExprKind* = enum
    semanticSymbol
    semanticVariable
    semanticFunction
    semanticStruct
    semanticPrimitiveType
    semanticPrimitiveValue
    semanticPrimitiveFunc
    semanticPrimitiveMacro
    semanticModule
    semanticFuncCall
    semanticCFFI
    semanticInt
    semanticString
  SemanticExpr* = object
    typesym*: Symbol
    case kind*: SemanticExprKind
    of semanticSymbol:
      symbol*: Symbol
    of semanticVariable:
      variable*: Variable
    of semanticFunction:
      function*: Function
    of semanticStruct:
      struct*: Struct
    of semanticPrimitiveType:
      primTypeName*: string
    of semanticPrimitiveValue:
      discard
    of semanticPrimitiveFunc:
      primFuncKind*: PrimitiveFuncKind
      primFuncName*: string
      primFuncRetType*: string
    of semanticPrimitiveMacro:
      macroproc*: proc (scope: Scope, sexpr: SExpr): SExpr
    of semanticModule:
      module*: Module
    of semanticFuncCall:
      funccall*: FuncCall
    of semanticCFFI:
      cffi*: CFFI
    of semanticInt:
      intval*: int64
    of semanticString:
      strval*: string
  Variable* = ref object
    name*: string
  Function* = ref object
    hash*: string
    name*: string
    argnames*: seq[string]
    argtypes*: seq[Symbol]
    rettype*: Symbol
    body*: seq[SemanticExpr]
  Struct* = ref object
    name*: string
  CCodegenInfo* = object
    headers*: OrderedTable[string, bool]
    decls*: seq[string]
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
  CFFI* = ref object
    name*: string
    primname*: string
    argtypes*: seq[Symbol]
    rettype*: Symbol
  Scope* = object
    module*: Module
    scopesymbols*: OrderedTable[Symbol, SemanticExpr]
  SemanticContext* = ref object
    modules*: OrderedTable[string, Module]
    symcount*: int

proc `$`*(symbol: Symbol): string =
  symbol.hash

proc newModule*(modulename: string): Module

let globalModule* = newModule("global")
let notTypeSym* = Symbol(module: globalModule, hash: "not_type_symbol")
let notTypeSemExpr* = SemanticExpr(typesym: notTypeSym, kind: semanticSymbol, symbol: notTypeSym)

proc hash*(symbol: Symbol): Hash =
  hash(symbol.hash)
proc `==`*(a, b: Symbol): bool =
  a.hash == b.hash

proc addSymbol*(module: Module, sym: Symbol, value: SemanticExpr) =
  if module.semanticexprs.hasKey(sym):
    raise newException(SemanticError, "couldn't redefine symbol: $#" % $sym)
  module.semanticexprs[sym] = value
proc addVariable*(module: Module, variable: Variable): Symbol =
  let sym = Symbol(module: module, hash: variable.name)
  module.context.symcount.inc
  module.addSymbol(sym, SemanticExpr(typesym: notTypeSym, kind: semanticVariable, variable: variable))
  return sym
proc addFunction*(module: Module, function: Function) =
  let sym = Symbol(module: module, hash: function.name & "_" & function.argtypes.mapIt($it).join("_"))
  module.addSymbol(sym, SemanticExpr(typesym: notTypeSym, kind: semanticFunction, function: function))
  module.exportedsymbols.add(sym)
proc addModule*(module: Module, importmodule: Module) =
  let sym = Symbol(module: module, hash: importmodule.name)
  module.addSymbol(sym, SemanticExpr(kind: semanticModule, module: importmodule))
proc addToplevelCall*(module: Module, funccall: FuncCall) =
  module.toplevelcalls.add(SemanticExpr(typesym: notTypeSym, kind: semanticFuncCall, funccall: funccall))
proc addCFFI*(module: Module, cffi: CFFI) =
  let sym = Symbol(module: module, hash: cffi.name)
  module.addSymbol(sym, SemanticExpr(typesym: notTypeSym, kind: semanticCFFI, cffi: cffi))

proc newSemanticContext*(): SemanticContext =
  new result
  result.modules = initOrderedTable[string, Module]()

proc defPrimitiveType*(module: Module, typename: string, primname: string) =
  module.addSymbol(
    Symbol(module: globalModule, hash: typename),
    SemanticExpr(typesym: notTypeSym, kind: semanticPrimitiveType, primTypeName: primname)
  )
proc defPrimitiveValue*(module: Module, typename: string) =
  module.addSymbol(
    Symbol(module: globalModule, hash: typename),
    SemanticExpr(typesym: notTypeSym, kind: semanticPrimitiveValue)
  )
proc defPrimitiveFunc*(module: Module, typename: string, rettype: string, kind: PrimitiveFuncKind, primname: string) =
  module.addSymbol(
    Symbol(module: globalModule, hash: typename),
    SemanticExpr(typesym: notTypeSym, kind: semanticPrimitiveFunc, primFuncKind: kind, primfuncname: primname, primfuncrettype: rettype)
  )
proc defPrimitiveMacro*(module: Module, macroname: string, macroproc: proc (scope: Scope, sexpr: SExpr): SExpr) =
  module.addSymbol(
    Symbol(module: globalModule, hash: macroname),
    SemanticExpr(typesym: notTypeSym, kind: semanticPrimitiveMacro, macroproc: macroproc)
  )

proc newCCodegenInfo*(): CCodegenInfo =
  result.headers = initOrderedTable[string, bool]()
  result.decls = @[]
proc addHeader*(info: var CCodegenInfo, name: string) =
  info.headers[name] = true

proc getType*(scope: Scope, semexpr: SemanticExpr): Symbol
proc getSymbol*(scope: Scope, name: string): Symbol

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
proc getHashFromTypes*(scope: Scope, name: string, argtypes: seq[Symbol]): string =
  return name & "_" & argtypes.mapIt($it).join("_")
proc getHashFromFuncCall*(scope: Scope, name: string, args: seq[SemanticExpr]): string =
  var types = newSeq[Symbol]()
  for arg in args:
     types.add(scope.getType(arg))
  return name & "_" & types.mapIt($it).join("_")

proc cheaderMacroExpand*(scope: Scope, sexpr: SExpr): SExpr =
  scope.module.ccodegeninfo.addHeader(sexpr.rest.first.strval)
  let (argtypes, rettype, funcdef) = parseTypeAnnotation(sexpr.rest.rest.first)
  let funcname = funcdef.rest.first
  let primname = funcdef.rest.rest.first.strval
  let argtypesyms = argtypes.mapIt(scope.getSymbol($it))
  let hash = scope.getHashFromTypes($funcname, argtypesyms)
  scope.module.defPrimitiveFunc(hash, $rettype, primitiveCall, primname)
  return newSNil()

proc predefined*(module: Module) =
  # module.defPrimitiveValue("nil")
  module.defPrimitiveType("Void", "void")
  module.defPrimitiveType("Int32", "int32_t")
  module.defPrimitiveType("String", "char*")
  module.defPrimitiveFunc("+_Int32_Int32", "Int32", primitiveInfix, "+")
  module.defPrimitiveMacro("c-header", cheaderMacroExpand)
proc newModule*(modulename: string): Module =
  new result
  result.name = modulename
  result.semanticexprs = initOrderedTable[Symbol, SemanticExpr]()
  result.toplevelcalls = @[]
  result.exportedsymbols = @[]
  result.ccodegenInfo = newCCodegenInfo()
  result.predefined()

proc newScope*(module: Module): Scope =
  result.module = module
  result.scopesymbols = initOrderedTable[Symbol, SemanticExpr]()
proc addArgSymbols*(scope: var Scope, argtypesyms: seq[Symbol], funcdef: SExpr) =
  for i, arg in funcdef.rest.rest.first:
    scope.scopesymbols.add(
      Symbol(module: scope.module, hash: $arg),
      SemanticExpr(
        typesym: argtypesyms[i],
        kind: semanticSymbol,
        symbol: Symbol(module: scope.module, hash: $arg)
      )
    )
proc getSymbol*(scope: Scope, name: string): Symbol =
  let sym = Symbol(module: scope.module, hash: name)
  if scope.scopesymbols.hasKey(sym):
    return sym
  elif scope.module.semanticexprs.hasKey(sym):
    return sym
  else:
    raise newException(SemanticError, "module hasn't symbol: $#" % name)
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

proc evalSExpr*(scope: Scope, sexpr: SExpr): SemanticExpr

proc evalFunctionBody*(scope: Scope, sexpr: SExpr): seq[SemanticExpr] =
  result = @[]
  for e in sexpr:
    result.add(scope.evalSExpr(e))

proc evalFunction*(scope: Scope, sexpr: SExpr)  =
  let (argtypes, rettype, funcdef) = parseTypeAnnotation(sexpr)
  let funcname = funcdef.rest.first
  let argtypesyms = argtypes.mapIt(scope.getSymbol($it))
  var argnames = newSeq[string]()
  for arg in funcdef.rest.rest.first:
    argnames.add($arg)

  var scope = scope
  scope.addArgSymbols(argtypesyms, funcdef)
  let f = Function(
    hash: scope.getHashFromTypes($funcname, argtypesyms),
    name: $funcname,
    argnames: argnames,
    argtypes: argtypesyms,
    rettype: scope.getSymbol($rettype),
    body: scope.evalFunctionBody(funcdef.rest.rest.rest),
  )
  scope.module.addFunction(f)

proc evalCFFI*(scope: Scope, sexpr: SExpr) =
  let (argtypes, rettype, cffidef) = parseTypeAnnotation(sexpr)
  let funcname = cffidef.rest.first
  let nameattr = cffidef.getAttr("name")
  let headerattr = cffidef.getAttr("header")
  let argtypesyms = argtypes.mapIt(scope.getSymbol($it))
  let primname = if nameattr.isSome:
                   $nameattr.get.strval
                 else:
                   $funcname
  let hash = scope.getHashFromTypes($funcname, argtypesyms)
  if headerattr.isSome:
    scope.module.ccodegeninfo.addHeader(headerattr.get.strval)
    scope.module.defPrimitiveFunc(hash, $rettype, primitiveCall, primname)
  else:
    let cffi = CFFI(
      name: $funcname,
      primname: primname,
      argtypes: argtypesyms,
      rettype: scope.getSymbol($rettype),
    )
    scope.module.addCFFI(cffi)

proc evalFuncCall*(scope: Scope, sexpr: SExpr): SemanticExpr =
  let semexpr = scope.trySemanticExpr(Symbol(hash: $sexpr.first))
  if semexpr.isSome and semexpr.get.kind == semanticPrimitiveMacro:
    return scope.evalSExpr(semexpr.get.macroproc(scope, sexpr))
  else:
    var args = newSeq[SemanticExpr]()
    for arg in sexpr.rest:
      args.add(scope.evalSExpr(arg))
    let sym = scope.getSymbol(scope.getHashFromFuncCall($sexpr.first, args))
    return SemanticExpr(
      typesym: scope.getRetType(sym),
      kind: semanticFuncCall,
      funccall: FuncCall(
        callfunc: sym,
        args: args,
      ),
    )

proc evalInt*(scope: Scope, sexpr: SExpr): SemanticExpr =
  return SemanticExpr(typesym: scope.getSymbol("Int32"), kind: semanticInt, intval: sexpr.intval)
proc evalString*(scope: Scope, sexpr: SExpr): SemanticExpr =
  return SemanticExpr(typesym: scope.getSymbol("String"), kind: semanticString, strval: sexpr.strval)

proc evalSExpr*(scope: Scope, sexpr: SExpr): SemanticExpr =
  case sexpr.kind
  of sexprNil:
    return notTypeSemExpr
  of sexprList:
    if sexpr.first.kind == sexprAttr and $sexpr.first == ":":
      if sexpr.last.first.kind == sexprIdent and $sexpr.last.first == "defn":
        scope.evalFunction(sexpr)
      elif sexpr.last.first.kind == sexprIdent and $sexpr.last.first == "c-ffi":
        scope.evalCFFI(sexpr)
      else:
        raise newException(SemanticError, "($#:$#) couldn't apply type annotation" % [$sexpr.span.line, $sexpr.span.linepos])
      return notTypeSemExpr
    elif sexpr.first.kind == sexprIdent and $sexpr.first == "defn":
      raise newException(SemanticError, "($#:$#) defn requires `:` type annotation" % [$sexpr.span.line, $sexpr.span.linepos])
    elif sexpr.first.kind == sexprIdent and $sexpr.first == "c-ffi":
      raise newException(SemanticError, "($#:$#) c-ffi requires `:` type annotation" % [$sexpr.span.line, $sexpr.span.linepos])
    else:
      return scope.evalFuncCall(sexpr)
  of sexprIdent:
    return scope.getSemanticExpr(scope.getSymbol($sexpr))
  of sexprInt:
    return scope.evalInt(sexpr)
  of sexprString:
    return scope.evalString(sexpr)
  else:
    raise newException(SemanticError, "($#:$#) $# is can't eval: $#" % [$sexpr.span.line, $sexpr.span.linepos, $sexpr.kind, $sexpr])

proc evalModule*(context: SemanticContext, modulename: string, sexpr: seq[SExpr]) =
  var module = newModule(modulename)
  var scope = newScope(module)
  context.modules[modulename] = module
  for e in sexpr:
    let semexpr = scope.evalSExpr(e)
    if semexpr.kind == semanticSymbol and semexpr.symbol == notTypeSym:
      discard
    else:
      module.toplevelcalls.add(semexpr)
