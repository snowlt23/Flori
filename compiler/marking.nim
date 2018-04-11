
import types, fexpr, metadata

import tables

proc newMarking*(scope: Scope, typesym: Symbol): Marking =
  new result
  result.scope = scope
  result.typesym = typesym
  result.owned = true
  result.dynamic = dynUnique
  result.fieldbody = initTable[Name, Marking]()
  if typesym.fexpr.hasDeftype:
    for b in typesym.fexpr.deftype.body:
      result.fieldbody[name(b[0])] = newMarking(scope, b[1].symbol)

proc returnFrom*(frm: Marking) =
  if frm.dynamic == dynBorrow:
    returnFrom(frm.origin)
  frm.owned = false
  frm.dynamic = dynShare
  for key, value in frm.fieldbody:
    returnFrom(value)
    
proc getFrom*(mark: Marking, frm: Marking) =
  if frm.dynamic == dynUnique and mark.scope.level >= frm.scope.level:
    frm.owned = false
    frm.dynamic = dynShare
    mark.owned = true
    mark.dynamic = dynUnique
  elif frm.dynamic == dynBorrow and mark.scope.level >= frm.scope.level:
    frm.origin.owned = false
    frm.origin.dynamic = dynShare
    mark.owned = true
    mark.dynamic = dynUnique
  elif frm.dynamic == dynUnique:
    mark.owned = false
    mark.dynamic = dynBorrow
    mark.origin = frm
  elif frm.dynamic == dynBorrow:
    mark.owned = false
    mark.dynamic = dynBorrow
    mark.origin = frm.origin
  else:
    mark.scope = frm.scope
    mark.owned = frm.owned
    mark.dynamic = frm.dynamic
    mark.origin = frm.origin

proc `==`*(a, b: Marking): bool =
  if a.dynamic != b.dynamic:
    return false
  for key, value in a.fieldbody:
    if value != b.fieldbody[key]:
      return false
  return true

proc copy*(mark: Marking): Marking =
  result = newMarking(mark.scope, mark.typesym)
  result.owned = mark.owned
  result.origin = mark.origin
  result.dynamic = mark.dynamic
  for key, value in result.fieldbody.mpairs:
    value = mark.fieldbody[key].copy
