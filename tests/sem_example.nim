
import unittest
import options
import strutils, sequtils
import compiler.fcore, compiler.internalpass, compiler.passmacro

initRootScope()
let scope = newFScope("testmodule", "testmodule.flori")
scope.importFScope(internalScope.obj.name, internalScope)
proc evalTest*(src: string): seq[FExpr] =
  result = parseToplevel("testmodule.flori", src)
  for f in result.mitems:
    scope.rootPass(f)

# prelude
discard evalTest("""
`+ =>
  $typed(intlit, intlit)
  internalop("int_add")
""")

proc inferred*(f: FExpr): string =
  "$# => $$typed($#) $$named($#) $$returned($#)" % [$f.args[0], f.internal.obj.argtypes.get.mapIt($it).join(", "), f.internal.obj.argnames.get.mapIt($it).join(", "), $f.internal.obj.returntype.get]

let f1 = evalTest("""
add5 =>
  x + 5
""")[0]
echo f1
echo "inferred: ", inferred(f1)
echo ""

let f2 = evalTest("""
`+ =>
  $typed(bool, bool)
  internalop("int_add")
add9 =>
  x + 9
""")
echo f2[0]
echo f2[1]
echo "inferred: ", inferred(f2[1])
echo ""

let f3 = evalTest("""
id => x
""")[0]
echo f3
echo "inferred: ", inferred(f3)
echo ""
