
import fexpr_core

import tables

proc newMarking*(typesym: Symbol): Marking =
  new result
  result.typesym = typesym
  result.owned = true
  result.dynamic = dynNone
  result.fieldbody = initTable[Name, Marking]()
  if typesym.fexpr.hasDeftype:
    for b in typesym.fexpr.deftype.body:
      result.fieldbody[name(b[0])] = newMarking(b[1].symbol)

proc moveFrom*(mark: Marking, frm: Marking) =
  if not frm.owned:
    frm.typesym.fexpr.error("move value should be owned.")
  frm.owned = false
  mark.owned = true
proc borrowFrom*(mark: Marking, frm: Marking) =
  mark.owned = false
  mark.origin = frm
