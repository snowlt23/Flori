
import semtree

import tables

type
  SemPass* = ref object of RootObj
  SemPassContext* = ref object
    passes*: seq[SemPass]
    modules*: Table[ScopeIdent, SemScope]

method execute*(pass: SemPass, ctx: SemPassContext) {.base.} =
  discard
