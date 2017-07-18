
import tables
import sast

type
  Function* = object
    argtypes*: seq[string]
    rettypes*: string
    name*: string
  SemanticContext* = object
    functions*: Table[string, Function]

proc newSemanticContext*(): SemanticContext =
  result.functions = initTable[string, Function]()

proc eval*(context: var SemanticContext, sexpr: SExpr) =
  discard
