
import tables, hashes
import strutils, sequtils
import sast

type
  SemanticError* = object of Exception
  SymbolKind* = enum
    symbolVariable
    symbolFunction
    symbolStruct
    symbolModule
  SymbolValue* = object
    case kind*: SymbolKind
    of symbolVariable:
      variable*: Variable
    of symbolFunction:
      function*: Function
    of symbolStruct:
      struct*: Struct
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
  let sym = Symbol(hash: variable.name & $module.context.symcount)
  module.context.symcount.inc
  module.symbolvalues[sym] = SymbolValue(
    kind: symbolVariable,
    variable: variable,
  )
  return sym
proc addFunction*(module: Module, function: Function): Symbol =
  let sym = Symbol(hash: function.name & "_" & function.argtypes.mapIt($it).join("_") & "_" & $function.rettype)
  module.symbolvalues[sym] = SymbolValue(
    kind: symbolFunction,
    function: function,
  )
  return sym
proc addModule*(module: Module, importmodule: Module): Symbol =
  let sym = Symbol(hash: importmodule.name)
  module.symbolvalues[sym] = SymbolValue(
    kind: symbolModule,
    module: importmodule,
  )
  return sym

proc newSemanticContext*(): SemanticContext =
  new result
  result.modules = initTable[string, Module]()
proc newModule*(modulename: string): Module =
  new result
  result.name = modulename
  result.symbolvalues = initTable[Symbol, SymbolValue]()

proc evalFunction*(module: Module, sexpr: SExpr) =
  echo sexpr

proc evalModule*(context: SemanticContext, modulename: string, sexpr: SExpr) =
  var module = newModule(modulename)
  context.modules[modulename] = module
  case sexpr.kind
  of sexprNil:
    discard
  of sexprList:
    if sexpr.first.kind == sexprIdent and $sexpr.first == "the":
      module.evalFunction(sexpr)
    elif sexpr.first.kind == sexprIdent and $sexpr.first == "defn":
      raise newException(SemanticError, "($#:$#) defn requires `the` type annotation" % [$sexpr.span.line, $sexpr.span.linepos])
  else:
    raise newException(SemanticError, "($#:$#) $# is can't eval: $#" % [$sexpr.span.line, $sexpr.span.linepos, $sexpr.kind, $sexpr])
