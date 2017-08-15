
import sast
import sparser
import semantic

type
  FFISExpr {.exportc: "__flori_SExpr".} = object
    sexpr: SExpr
  BackquoteEvaluatorObj {.exportc: "__flori_BackquoteEvaluator".} = object
  BackquoteEvaluator = ptr BackquoteEvaluatorObj

proc ffiFirst(sexpr: FFISExpr): FFISExpr {.exportc: "__flori_first".} =
  FFISExpr(sexpr: sexpr.sexpr.first)
proc ffiRest(sexpr: FFISExpr): FFISExpr {.exportc: "__flori_rest".} =
  FFISExpr(sexpr: sexpr.sexpr.rest)

proc ffiQuoteEval(cstr: cstring): SExpr {.exportc: "__flori_quote_eval".} =
   parseSExpr("quote", $cstr)

proc ffiNewBackquoteEvaluator(sexpr: SExpr): BackquoteEvaluator {.exportc: "__flori_new_backquote_evaluator".} =
  result = cast[BackquoteEvaluator](alloc(sizeof(BackquoteEvaluatorObj)))
  # TODO:
proc ffiBackquoteEval(be: BackquoteEvaluator, name: cstring, sexpr: SExpr) {.exportc: "__flori_backquote_eval".} =
  discard
  # TODO:
proc ffiBackquoteGetSExpr(be: BackquoteEvaluator): SExpr {.exportc: "__flori_backquote_get_sexpr".} =
  discard
  # TODO:
proc ffiBackquoteDestructor(be: BackquoteEvaluator) {.exportc: "__flori_backquote_destructor".} =
  discard
  # TODO:
