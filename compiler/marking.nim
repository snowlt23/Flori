
import fexpr_core

type
  DynamicKind* = enum
    dynUnique
    dynBorrow
    dynShare
    # dynPool
  Marking* = ref object
    owned*: bool
    dynamic*: DynamicKind
    fieldbody*: Table[Name, Marking]

proc newMarking*(typesym: Symbol): Marking =
  new result
  result.owned = true
  result.dynamic = dynUnique
  result.fieldbody = initTable[Name, Marking]()
  for b in typesym.fexpr.deftype.body:
    fieldbody[name(b[0])] = newMarking(b[1].symbol)
