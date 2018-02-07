
import types, scope

type
  CTRCError* = object of Exception

proc initCTRC*(): CTRC =
  result.tracked = true
  result.refcnt = 1
  result.destroyed = false

proc inc*(ctrc: var CTRC) =
  if not ctrc.tracked: raise newException(CTRCError, "cannot apply inc to untracked CTRC.")
  ctrc.refcnt.inc
proc dec*(ctrc: var CTRC) =
  if not ctrc.tracked: raise newException(CTRCError, "cannot apply dec to untracked CTRC.")
  ctrc.refcnt.dec
  if ctrc.refcnt == 0:
    ctrc.destroyed = true

proc tracking*(scope: Scope, value: FExpr) =
  scope.scopevalues.add(value)
