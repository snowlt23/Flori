
import fexpr_core
import passmacro

var elimRoot*: proc(scope: Scope, fexpr: var FExpr) = nil

proc elimToplevelPass*(scope: Scope, fexpr: var FExpr): bool =
  if fexpr.isElimEvaluated:
    return false
  fexpr.isElimEvaluated = true
  if fexpr.hasInternalMark and fexpr.internalMark == internalDefn:
    fexpr.isEliminated = true
    return false
  elif fexpr.hasInternalMark and fexpr.internalMark == internalMacro:
    fexpr.isEliminated = false
    scope.elimRoot(fexpr.defn.body)
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

proc elimMarkingPass*(scope: Scope, fexpr: var FExpr): bool =
  if fexpr.hasInternalMark and fexpr.internalMark == internalWhile:
    scope.elimRoot(fexpr[1])
    scope.elimRoot(fexpr[2])
  elif fexpr.hasInternalMark and fexpr.internalMark == internalIf:
    for b in fexpr.internalIfexpr.elifbranch.mitems:
      scope.elimRoot(b.cond)
      scope.elimRoot(b.body)
      scope.elimRoot(fexpr.internalIfexpr.elsebranch)
  elif fexpr.hasInternalMark and fexpr.internalMark == internalDef:
    scope.elimRoot(fexpr[2])
  elif fexpr.hasInternalMark and fexpr.internalMark == internalSet:
    scope.elimRoot(fexpr[1])
    scope.elimRoot(fexpr[2])
  elif fexpr.hasInternalMark and fexpr.internalMark == internalFieldAccess:
    scope.elimRoot(fexpr[1])
  elif fexpr.isFuncCall:
    if fexpr[0].hasTyp and fexpr[0].typ.kind == symbolFuncType:
      discard
    elif fexpr[0].kind == fexprSymbol:
      fexpr[0].symbol.fexpr.isEliminated = false
      scope.elimRoot(fexpr[0].symbol.fexpr.defn.body)
  elif fexpr.kind == fexprSymbol and fexpr.symbol.kind == symbolFunc:
    fexpr.symbol.fexpr.isEliminated = false
    scope.elimRoot(fexpr.symbol.fexpr.defn.body)
  return true

definePass processElimPass, elimRoot, (Scope, var FExpr):
  elimToplevelPass
  elimMarkingPass

proc resetElim*(scope: Scope, fexpr: var FExpr) =
  fexpr.isElimEvaluated = false
  if fexpr.kind in fexprContainer:
    for son in fexpr.mitems:
      scope.resetElim(son)
