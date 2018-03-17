
import types, scope, metadata

import options

type
  CTRCError* = object of Exception

proc initCTRC*(cnt = 1): CTRC =
  new result
  result.refcnt = cnt
  result.depends = @[]
  result.dest = false
  result.alias = none(CTRC)
  
proc isret*(ctrc: CTRC): bool =
  if ctrc.alias.isSome:
    return ctrc.alias.get.isret
  return ctrc.ret
proc `isret=`*(ctrc: CTRC, value: bool) =
  if ctrc.alias.isSome:
    ctrc.alias.get.isret = value
    return
  ctrc.ret = value

proc destroyed*(ctrc: CTRC): bool =
  if ctrc.alias.isSome:
    return ctrc.alias.get.destroyed
  return ctrc.dest
proc `destroyed=`*(ctrc: CTRC, value: bool) =
  if ctrc.alias.isSome:
    ctrc.alias.get.destroyed = value
    return
  ctrc.dest = value
  
proc cnt*(ctrc: CTRC): int =
  if ctrc.alias.isSome:
    return cnt(ctrc.alias.get)
  
  ctrc.refcnt
proc `cnt=`*(ctrc: CTRC, value: int) =
  if ctrc.alias.isSome:
    ctrc.alias.get.cnt = value
    return
  
  ctrc.refcnt = value

proc tracked*(fexpr: FExpr): bool =
  fexpr.hasCTRC
  
proc inc*(ctrc: CTRC): bool =
  if ctrc.alias.isSome:
    return inc(ctrc.alias.get)
  
  if ctrc.destroyed:
    return false
  ctrc.refcnt.inc
  return true

proc dec*(ctrc: CTRC) =
  if ctrc.alias.isSome:
    dec(ctrc.alias.get)
    return
    
  if ctrc.refcnt <= 0:
    return
  
  ctrc.refcnt.dec
  if ctrc.refcnt == 0:
    ctrc.destroyed = true
    for depend in ctrc.depends:
      depend.dec

proc revive*(ctrc: CTRC) =
  if ctrc.alias.isSome:
    revive(ctrc.alias.get)
    return
    
  ctrc.refcnt = 1
  ctrc.destroyed = false
  
proc depend*(ctrc: CTRC, d: CTRC): bool =
  if ctrc.alias.isSome:
    return depend(ctrc.alias.get, d)
  if d.alias.isSome:
    return depend(ctrc, d.alias.get)
    
  ctrc.depends.add(d)
  return d.inc
  
proc tracking*(scope: Scope, value: FExpr) =
  scope.scopevalues.add(value)
