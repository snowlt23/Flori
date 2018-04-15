
import types, fexpr, scope, metadata
import ccodegen

import strutils, sequtils
import tables
import options

proc newMarking*(scope: Scope, typesym: Symbol): Marking =
  new result
  result.scope = scope
  result.typesym = typesym
  result.owned = true
  result.fieldbody = initTable[Name, Marking]()
  if typesym.fexpr.hasDeftype:
    for b in typesym.fexpr.deftype.body:
      result.fieldbody[name(b[0])] = newMarking(scope, b[1].symbol)

proc returnFrom*(frm: Marking) =
  frm.owned = false
  for key, value in frm.fieldbody:
    returnFrom(value)
      
proc moveFrom*(mark: Marking, frm: Marking) =
  if frm.owned:
    frm.owned = false
    mark.owned = true
    mark.origin = mark
  else:
    mark.owned = false
    mark.origin = mark
  
proc debugMarking*(marking: Marking, indent: int): string =
  result = $marking.owned
  if marking.fieldbody.len != 0:
    result &= " {\n"
    for key, value in marking.fieldbody:
      result &= " ".repeat(indent+2) & $key & ":" & debugMarking(value, indent+2) & "\n"
    result &= " ".repeat(indent) & "}"

proc copy*(mark: Marking): Marking =
  result = newMarking(mark.scope, mark.typesym)
  result.owned = mark.owned
  result.origin = mark.origin
  for key, value in result.fieldbody.mpairs:
    value = mark.fieldbody[key].copy
