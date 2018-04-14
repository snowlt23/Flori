
import fexpr_core
import newpassmacro, sempass

proc processFPass*(scope: Scope, fexpr: var FExpr) =
  processSemPass(scope, fexpr)
