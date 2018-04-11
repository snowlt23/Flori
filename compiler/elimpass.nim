
import fexpr_core, marking
import passmacro

definePass ElimPass

proc elimToplevelPass*(scope: Scope, fexpr: var FExpr) {.pass: ElimPass.} =
  if fexpr.isElimEvaluated:
    return
  fexpr.isElimEvaluated = true
  if fexpr.hasInternalMark and fexpr.internalMark in {internalDefn}:
    fexpr.isEliminated = true
    return
    
  case fexpr.kind
  of fexprArray, fexprList:
    for son in fexpr.mitems:
      scope.rootPass(son)
    scope.nextPass(fexpr)
  of fexprBlock:
    for son in fexpr.mitems:
      scope.rootPass(son)
    scope.nextPass(fexpr)
  of fexprSeq:
    for i in 1..<fexpr.len:
      scope.rootPass(fexpr[i])
    scope.nextPass(fexpr)
  else:
    scope.nextPass(fexpr)

proc elimMarkingPass*(scope: Scope, fexpr: var FExpr) {.pass: ElimPass.} =
  if fexpr.isFuncCall:
    if fexpr[0].kind == fexprSymbol and not fexpr[0].symbol.fexpr.internalPragma.compiletime:
      fexpr[0].symbol.fexpr.isEliminated = false
      scope.rootPass(fexpr[0].symbol.fexpr.defn.body)
  scope.nextPass(fexpr)

instPass ElimPass, processElimPass
