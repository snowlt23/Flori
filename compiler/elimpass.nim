
import options
import fexpr_core
import passmacro

var elimRoot*: proc(scope: Scope, fexpr: FExpr) = nil

proc elimToplevelPass*(scope: Scope, fexpr: FExpr): bool =
  if fexpr.metadata.isElimEvaluated:
    return false
  fexpr.metadata.isElimEvaluated = true
  if fexpr.metadata.internal == internalDefn:
    if fexpr.metadata.exportc.isSome:
      fexpr.metadata.isEliminated = false
      scope.elimRoot(fexpr.fnBody)
      return false
    fexpr.metadata.isEliminated = true
    return false
  elif fexpr.metadata.internal == internalMacro:
    fexpr.metadata.isEliminated = false
    scope.elimRoot(fexpr.fnBody)
    return false
    
  case fexpr.kind
  of fexprArray, fexprList:
    for son in fexpr.mitems:
      scope.elimRoot(son)
    return true
  of fexprBlock:
    for son in fexpr.mitems:
      scope.elimRoot(son)
    return true
  of fexprSeq:
    for i in 1..<fexpr.len:
      scope.elimRoot(fexpr[i])
    return true
  else:
    return true

proc elimMarkingPass*(scope: Scope, fexpr: FExpr): bool =
  if fexpr.metadata.internal == internalWhile:
    scope.elimRoot(fexpr[1])
    scope.elimRoot(fexpr[2])
  elif fexpr.metadata.internal == internalIf:
    let branches = fexpr.getIfBranches()
    for b in branches:
      if b.cond.isSome:
        scope.elimRoot(fexpr[b.cond.get])
      scope.elimRoot(fexpr[b.body])
  elif fexpr.metadata.internal == internalDef:
    scope.elimRoot(fexpr[2])
  elif fexpr.metadata.internal == internalSet:
    scope.elimRoot(fexpr[1])
    scope.elimRoot(fexpr[2])
  elif fexpr.metadata.internal == internalFieldAccess:
    scope.elimRoot(fexpr[1])
  elif fexpr.isFuncCall:
    if fexpr[0].hasTyp and fexpr[0].metadata.typ.kind == symbolFuncType:
      discard
    elif fexpr[0].kind == fexprSymbol:
      fexpr[0].symbol.fexpr.metadata.isEliminated = false
      scope.elimRoot(fexpr[0].symbol.fexpr.fnBody)
  elif fexpr.kind == fexprSymbol and fexpr.symbol.kind == symbolFunc:
    fexpr.symbol.fexpr.metadata.isEliminated = false
    scope.elimRoot(fexpr.symbol.fexpr.fnBody)
  return true

definePass processElimPass, elimRoot, (Scope, FExpr):
  elimToplevelPass
  elimMarkingPass

proc resetElim*(scope: Scope, fexpr: FExpr) =
  fexpr.metadata.isElimEvaluated = false
  if fexpr.kind in fexprContainer:
    for son in fexpr.mitems:
      scope.resetElim(son)
