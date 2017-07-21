
import sparser
import sast
import semantic

type
  InterpreterOpKind* = enum
    opAdd
    opSub
    opMul
    opDiv
    opAssign
  InterpreterOp* = object
    case kind*: InterpreterOpKind
    of opAdd, opSub, opMul, opDiv:
      discard
    of op
  InterpreterValue* = ptr object
    
  InterpreterContext* = object
    semcontext*: SemanticContext

proc newInterpreterContext*(): InterpreterContext =
  result.semcontext = newSemanticContext()

proc newInterpreterValue*() =


proc eval*(context: InterpreterContext, sexpr: SExpr):  =
