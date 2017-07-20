
import tables, hashes
import options
import strutils, sequtils
import sast

type
  SemanticError* = object of Exception
  SymbolKind* = enum
    symbolVariable
    symbolFunction
    symbolStruct
    symbolPrimitiveType
    symbolPrimitiveValue
    symbolModule
  SymbolValue* = object
    case kind*: SymbolKind
    of symbolVariable:
      variable*: Variable
    of symbolFunction:
      function*: Function
    of symbolStruct:
      struct*: Struct
    of symbolPrimitiveType:
      primitivename*: string
    of symbolPrimitiveValue:
      primitivevalue*: string
    of symbolModule:
      module*: Module
  Variable* = ref object
    name*: string
  Function* = ref object
    name*: string
    argtypes*: seq[Symbol]
    rettype*: Symbol
    body*: SExpr
  Struct* = ref object
    name*: string
  Module* = ref object
    context*: SemanticContext
    name*: string
    symbolvalues*: Table[Symbol, SymbolValue]
    exportedsymbols*: seq[Symbol]
  SemanticContext* = ref object
    modules*: Table[string, Module]
    symcount*: int

proc hash*(symbol: Symbol): Hash =
  hash(symbol.hash)
proc `==`*(a, b: Symbol): bool =
  a.hash == b.hash
proc `$`*(symbol: Symbol): string =
  symbol.hash

proc addVariable*(module: Module, variable: Variable): Symbol =
  let sym = Symbol(hash: variable.name)
  module.context.symcount.inc
  module.symbolvalues[sym] = SymbolValue(
    kind: symbolVariable,
    variable: variable,
  )
  return sym
proc addFunction*(module: Module, function: Function) =
  let sym = Symbol(hash: function.name & "_" & function.argtypes.mapIt($it).join("_"))
  module.symbolvalues[sym] = SymbolValue(
    kind: symbolFunction,
    function: function,
  )
  module.exportedsymbols.add(sym)
proc addModule*(module: Module, importmodule: Module) =
  let sym = Symbol(hash: importmodule.name)
  module.symbolvalues[sym] = SymbolValue(
    kind: symbolModule,
    module: importmodule,
  )
  # module.exportedsymbols.add(sym)

proc newSemanticContext*(): SemanticContext =
  new result
  result.modules = initTable[string, Module]()

proc defPrimitiveType*(module: Module, typename: string, primitivename: string) =
  module.symbolvalues[Symbol(hash: typename)] = SymbolValue(kind: symbolPrimitiveType, primitivename: primitivename)
proc defPrimitiveValue*(module: Module, typename: string, primitivename: string) =
  module.symbolvalues[Symbol(hash: typename)] = SymbolValue(kind: symbolPrimitiveValue, primitivevalue: primitivename)
proc predefined*(module: Module) =
  module.defPrimitiveValue("nil", "NULL")
  module.defPrimitiveType("Int32", "int32_t")
proc getSymbol*(module: Module, name: string): Symbol =
  if not module.symbolvalues.hasKey(Symbol(hash: name)):
    raise newException(SemanticError, "module hasn't symbol: $#" % name)
  return Symbol(hash: name)
proc getSymbolValue*(module: Module, sym: Symbol): SymbolValue =
  module.symbolvalues[sym]
proc newModule*(modulename: string): Module =
  new result
  result.name = modulename
  result.symbolvalues = initTable[Symbol, SymbolValue]()
  result.exportedsymbols = @[]
  result.predefined()

proc getType*(module: Module, sexpr: SExpr): Symbol

proc getTypeFromFunctionCall*(module: Module, sexpr: SExpr): Symbol =
  var types = newSeq[string]()
  for arg in sexpr.rest:
     types.add($getType(module, arg))
  let hash = $sexpr.first & "_" & types.mapIt($it).join("_")
  let sym = module.getSymbol(hash)
  return getSymbolValue(module, sym).function.rettype

proc getType*(module: Module, sexpr: SExpr): Symbol =
  case sexpr.kind
  of sexprNil:
    module.getSymbol("nil")
  of sexprList:
    module.getTypeFromFunctionCall(sexpr)
  of sexprIdent:
    module.getSymbol($sexpr)
  of sexprInt:
    module.getSymbol("Int32")

proc evalFunction*(module: Module, sexpr: SExpr) =
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
  let f = Function(
    name: $funcname,
    argtypes: argtypes.mapIt(module.getType(it)),
    rettype: module.getType(rettype),
    body: funcdef,
  )
  module.addFunction(f)

proc evalSExpr*(module: Module, sexpr: SExpr) =
  case sexpr.kind
  of sexprNil:
    discard
  of sexprList:
    if sexpr.first.kind == sexprIdent and $sexpr.first == "the":
      module.evalFunction(sexpr)
    elif sexpr.first.kind == sexprIdent and $sexpr.first == "defn":
      raise newException(SemanticError, "($#:$#) defn requires `the` type annotation" % [$sexpr.span.line, $sexpr.span.linepos])
    else:
      echo module.getType(sexpr)
  else:
    raise newException(SemanticError, "($#:$#) $# is can't eval: $#" % [$sexpr.span.line, $sexpr.span.linepos, $sexpr.kind, $sexpr])
  echo module.symbolvalues

proc evalModule*(context: SemanticContext, modulename: string, sexpr: seq[SExpr]) =
  var module = newModule(modulename)
  context.modules[modulename] = module
  for e in sexpr:
    module.evalSExpr(e)
