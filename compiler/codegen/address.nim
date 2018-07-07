
import tables
import tacode

type
  AddressTable* = Table[string, bool]

proc analyzeAddress*(ctx: TAContext): AddressTable =
  result = initTable[string, bool]()
  for fn in ctx.fns:
    for code in fn.body:
      if code.hasDist:
        result[code.getname] = false
      if code.kind == TACodeKind.AAddr:
        assert(code.aaddr.value.kind == TAAtomKind.AVar)
        result[code.aaddr.value.avar.name] = true
