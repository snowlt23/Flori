
import semtree

import tables

type
  SemPass* = ref object of RootObj
  SemPassContext* = ref object
    passes*: seq[SemPass]
    modules*: Table[string, SemScope]

proc newSemPassContext*(): SemPassContext =
  SemPassContext(passes: @[], modules: initTable[string, SemScope]())

method execute*(pass: SemPass, ctx: SemPassContext) {.base.} =
  discard
  
proc register*(ctx: SemPassContext, pass: SemPass) =
  ctx.passes.add(pass)

proc execute*(ctx: SemPassContext) =
  for pass in ctx.passes:
    pass.execute(ctx)
