
import unittest
import options
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

suite "semantic pass test":
  test "call":
    var f = evalTest("""
add5 => a + 5
""")
    check $f[0].args[1].typ.get == "intlit"
  test "call stmt":
    var f = evalTest("""
add9 =>
  a + 9
""")
    check $f[0].args[1].sons[0].typ.get == "intlit"
  test "argument":
    var f = evalTest("""
id => x
""")
    check $f[0].args[1].typ.get == "void"
  test "fib":
    var f = evalTest("""
fib =>
  if n<2
    n
  else
    fib(n-1) + fib(n-2)
""")
    check $f[0].args[1].sons[0].typ.get == "intlit"
