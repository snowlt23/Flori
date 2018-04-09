
import fexpr_core

import tables

proc newMarking*(typesym: Symbol): Marking =
  new result
  result.typesym = typesym
  result.owned = true
  result.dynamic = dynNone
  result.fieldbody = initTable[Name, Marking]()
  for b in typesym.fexpr.deftype.body:
    result.fieldbody[name(b[0])] = newMarking(b[1].symbol)

proc move*(mark: Marking): Marking =
  if not mark.owned:
    mark.typesym.fexpr.error("move value should be owned.")
  mark.owned = false
  result = newMarking(mark.typesym)
proc borrow*(mark: Marking): Marking =
  result = newMarking(mark.typesym)
  result.owned = false
  result.dynamic = dynBorrow
  result.origin = mark
