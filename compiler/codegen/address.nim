
import tables
import tacode

type
  AddressTable* = Table[string, bool]

proc analyzeAddress*(fn: TAFn): AddressTable =
  result = initTable[string, bool]()
  for code in fn.body:
    if code.hasDist:
      result[code.getname] = false
    if code.kind == TACodeKind.AAddr:
      assert(code.aaddr.value.kind == TAAtomKind.AVar)
      result[code.aaddr.value.avar.name] = true
