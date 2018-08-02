
import ../fcore, ../passmacro, ../internalpass
import ../codegen/vop

let prelude = """
`+ => $typed(int, int)
  internalop("int_add")

`- => $typed(int, int)
  internalop("int_sub")

`< => $typed(int, int)
  internalop("int_lesser")
"""

initRootScope()
let scope = newFScope("testmodule", "testmodule.flori")
scope.importFScope(internalScope.obj.name, internalScope)
var vopctx = initVOPCtx()
proc evalTest*(src: string): seq[FExpr] =
  result = parseToplevel("testmodule.flori", src)
  for f in result.mitems:
    scope.rootPass(f)
    discard vopctx.vop(f)
discard evalTest(prelude)

let fexprs = evalTest("""
if 4<5:
  9
else:
  10
""")

echo vopctx

