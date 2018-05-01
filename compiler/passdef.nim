
import fexpr_core
import passmacro, sempass

proc processFPass*(scope: Scope, fexpr: var FExpr) =
  processSemPass(scope, fexpr)
