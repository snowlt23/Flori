
import types, scope

type
  CTRCError* = object of Exception

proc initCTRC*(): CTRC =
  new result
  result.refcnt = 1
  result.link = nil

proc tracked*(ctrc: CTRC): bool =
  ctrc != nil

proc inc*(ctrc: var CTRC) =
  if ctrc.link == nil:
    if not ctrc.tracked: raise newException(CTRCError, "cannot apply inc to untracked CTRC.")
    ctrc.refcnt.inc
  else:
    ctrc.link.inc
proc dec*(ctrc: var CTRC) =
  if ctrc.link == nil:
    if not ctrc.tracked: raise newException(CTRCError, "cannot apply dec to untracked CTRC.")
    ctrc.refcnt.dec
  else:
    ctrc.link.dec
    
proc cnt*(ctrc: CTRC): int =
  if ctrc.link == nil:
    ctrc.refcnt
  else:
    ctrc.link.cnt
proc `cnt=`*(ctrc: var CTRC, val: int) =
  if ctrc.link == nil:
    ctrc.refcnt = val
  else:
    ctrc.link.cnt = val
    
proc destroyed*(ctrc: CTRC): bool =
  ctrc.cnt <= 0
    
proc tracking*(scope: Scope, value: FExpr) =
  scope.scopevalues.add(value)
