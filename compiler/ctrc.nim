
import types, scope, metadata

type
  CTRCError* = object of Exception

proc initCTRC*(): CTRC =
  new result
  result.refcnt = 1
  result.depends = @[]
  result.destroyed = false

proc tracked*(fexpr: FExpr): bool =
  fexpr.hasCTRC
  
proc inc*(ctrc: CTRC): bool =
  if ctrc.destroyed:
    return false
  ctrc.refcnt.inc
  return true

proc dec*(ctrc: CTRC) =
  if ctrc.refcnt <= 0:
    return
  
  ctrc.refcnt.dec
  if ctrc.refcnt == 0:
    ctrc.destroyed = true
    for depend in ctrc.depends:
      depend.dec

proc revive*(ctrc: CTRC) =
  ctrc.refcnt = 1
  ctrc.destroyed = false
  
proc depend*(ctrc: CTRC, d: CTRC): bool =
  ctrc.depends.add(d)
  return d.inc
  
proc tracking*(scope: Scope, value: FExpr) =
  scope.scopevalues.add(value)
