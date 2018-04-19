
import fexpr_core
import newpassmacro, sempass, effectpass

proc processFPass*(scope: Scope, fexpr: var FExpr) =
  processSemPass(scope, fexpr)
