
import tables, hashes
import strutils, sequtils
import sast

type
  SemanticError* = object of Exception
  SemanticExprKind* = enum
    semanticSymbol
    semanticVariable
    semanticFunction
    semanticStruct
    semanticPrimitiveType
    semanticPrimitiveValue
    semanticPrimitiveFunc
    semanticModule
    semanticFuncCall
    semanticInt
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
      discard
    of semanticPrimitiveValue:
      discard
    of semanticPrimitiveFunc:
      primitiverettype*: string
    of semanticModule:
      module*: Module
    of semanticFuncCall:
      funccall*: FuncCall
    of semanticInt:
      intval*: int64
  Variable* = ref object
    name*: string
  Function* = ref object
    name*: string
    argnames*: seq[string]
    argtypes*: seq[Symbol]
    rettype*: Symbol
    body*: seq[SemanticExpr]
  Struct* = ref object
    name*: string
  Module* = ref object
    context*: SemanticContext
    name*: string
    semanticexprs*: OrderedTable[Symbol, SemanticExpr]
    toplevelcalls*: seq[SemanticExpr]
    exportedsymbols*: seq[Symbol]
  FuncCall* = ref object
    callfunc*: Symbol
    args*: seq[SemanticExpr]
  Scope* = object
    module*: Module
    scopesymbols*: OrderedTable[Symbol, SemanticExpr]
  SemanticContext* = ref object
    modules*: OrderedTable[string, Module]
    symcount*: int

let notTypeSym* = Symbol(hash: "not_type_symbol")
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
  let sym = Symbol(hash: variable.name)
  module.context.symcount.inc
  module.addSymbol(sym, SemanticExpr(typesym: notTypeSym, kind: semanticVariable, variable: variable))
  return sym
proc addFunction*(module: Module, function: Function) =
  let sym = Symbol(hash: function.name & "_" & function.argtypes.mapIt($it).join("_"))
  module.addSymbol(sym, SemanticExpr(typesym: notTypeSym, kind: semanticFunction, function: function))
  module.exportedsymbols.add(sym)
proc addModule*(module: Module, importmodule: Module) =
  let sym = Symbol(hash: importmodule.name)
  module.addSymbol(sym, SemanticExpr(kind: semanticModule, module: importmodule))
proc addToplevelCall*(module: Module, funccall: FuncCall) =
  module.toplevelcalls.add(SemanticExpr(typesym: notTypeSym, kind: semanticFuncCall, funccall: funccall))

proc newSemanticContext*(): SemanticContext =
  new result
  result.modules = initOrderedTable[string, Module]()

proc defPrimitiveType*(module: Module, typename: string) =
  module.addSymbol(
    Symbol(hash: typename),
    SemanticExpr(typesym: notTypeSym, kind: semanticPrimitiveType)
  )
proc defPrimitiveValue*(module: Module, typename: string) =
  module.addSymbol(
    Symbol(hash: typename),
    SemanticExpr(typesym: notTypeSym, kind: semanticPrimitiveValue)
  )
proc defPrimitiveFunc*(module: Module, typename: string, rettype: string) =
  module.addSymbol(
    Symbol(hash: typename),
    SemanticExpr(typesym: notTypeSym, kind: semanticPrimitiveFunc, primitiverettype: rettype)
  )
proc predefined*(module: Module) =
  module.defPrimitiveValue("nil")
  module.defPrimitiveType("Int32")
  module.defPrimitiveFunc("+_Int32_Int32", "Int32")
proc newModule*(modulename: string): Module =
  new result
  result.name = modulename
  result.semanticexprs = initOrderedTable[Symbol, SemanticExpr]()
  result.exportedsymbols = @[]
  result.predefined()

proc newScope*(module: Module): Scope =
  result.module = module
  result.scopesymbols = initOrderedTable[Symbol, SemanticExpr]()
proc addArgSymbols*(scope: var Scope, argtypesyms: seq[Symbol], funcdef: SExpr) =
  for i, arg in funcdef.rest.rest.first:
    scope.scopesymbols.add(
      Symbol(hash: $arg),
      SemanticExpr(
        typesym: argtypesyms[i],
        kind: semanticSymbol,
        symbol: Symbol(hash: $arg)
      )
    )
proc getSymbol*(scope: Scope, name: string): Symbol =
  let sym = Symbol(hash: name)
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

proc getType*(scope: Scope, semexpr: SemanticExpr): Symbol

proc getHashFromFuncCall*(scope: Scope, name: string, args: seq[SemanticExpr]): string =
  var types = newSeq[Symbol]()
  for arg in args:
     types.add(scope.getType(arg))
  return name & "_" & types.mapIt($it).join("_")

proc getType*(scope: Scope, semexpr: SemanticExpr): Symbol =
  return semexpr.typesym
proc getRetType*(scope: Scope, sym: Symbol): Symbol =
  let semexpr = scope.getSemanticExpr(sym)
  if semexpr.kind == semanticFunction:
    return semexpr.function.rettype
  elif semexpr.kind == semanticPrimitiveFunc:
    return scope.getSymbol(semexpr.primitiverettype)
  else:
    raise newException(SemanticError, "expression is not function")

proc evalSExpr*(scope: Scope, sexpr: SExpr): SemanticExpr

proc evalFunctionBody*(scope: Scope, sexpr: SExpr): seq[SemanticExpr] =
  result = @[]
  for e in sexpr:
    result.add(scope.evalSExpr(e))

proc evalFunction*(scope: Scope, sexpr: SExpr)  =
  var argtypes = newSeq[SExpr]()
  var rettype: SExpr
  var funcdef: SExpr
  for arg in sexpr.rest.list:
    if $arg.first == "->":
      rettype = arg.rest.first
      funcdef = arg.rest.rest.first
      break
    else:
      argtypes.add(arg.first)
  if rettype == nil:
    rettype = ast(sexpr.span, newSIdent("void"))
  let funcname = funcdef.rest.first
  let argtypesyms = argtypes.mapIt(scope.getSymbol($it))
  var argnames = newSeq[string]()
  for arg in funcdef.rest.rest.first:
    argnames.add($arg)

  var scope = scope
  scope.addArgSymbols(argtypesyms, funcdef)
  let f = Function(
    name: $funcname,
    argnames: argnames,
    argtypes: argtypesyms,
    rettype: scope.getSymbol($rettype),
    body: scope.evalFunctionBody(funcdef.rest.rest.rest),
  )
  scope.module.addFunction(f)

proc evalFuncCall*(scope: Scope, sexpr: SExpr): SemanticExpr =
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

proc evalSExpr*(scope: Scope, sexpr: SExpr): SemanticExpr =
  case sexpr.kind
  of sexprNil:
    discard
  of sexprList:
    if sexpr.first.kind == sexprIdent and $sexpr.first == ":":
      scope.evalFunction(sexpr)
      return notTypeSemExpr
    elif sexpr.first.kind == sexprIdent and $sexpr.first == "defn":
      raise newException(SemanticError, "($#:$#) defn requires `the` type annotation" % [$sexpr.span.line, $sexpr.span.linepos])
    else:
      return scope.evalFuncCall(sexpr)
  of sexprIdent:
    return scope.getSemanticExpr(scope.getSymbol($sexpr))
  of sexprInt:
    return scope.evalInt(sexpr)
  else:
    raise newException(SemanticError, "($#:$#) $# is can't eval: $#" % [$sexpr.span.line, $sexpr.span.linepos, $sexpr.kind, $sexpr])

proc evalModule*(context: SemanticContext, modulename: string, sexpr: seq[SExpr]) =
  var module = newModule(modulename)
  var scope = newScope(module)
  context.modules[modulename] = module
  for e in sexpr:
    discard scope.evalSExpr(e)
