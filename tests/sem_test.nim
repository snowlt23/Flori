
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
`- =>
  $typed(intlit, intlit)
  internalop("int_sub")
`< =>
  $typed(intlit, intlit)
  internalop("int_lesser")
""")

proc getargnames(f: FExpr): string =
  f.internal.obj.argnames.get.mapIt($it).join(" ")
proc getargtypes(f: FExpr): string =
  f.internal.obj.argtypes.get.mapIt($it).join(" ")

suite "semantic pass test":
  test "call":
    var f = evalTest("""
add5 => a + 5
""")
    check f[0].getargnames == "a"
    check f[0].getargtypes == "intlit"
    check $f[0].args[1].gettype == "intlit"
  test "call stmt":
    var f = evalTest("""
add9 =>
  a + 9
""")
    check f[0].getargnames == "a"
    check f[0].getargtypes == "intlit"
    check $f[0].args[1].sons[0].gettype == "intlit"
  test "generalize":
    var f = evalTest("""
id => x
""")
    check f[0].getargnames == "x"
    check f[0].getargtypes == "T0"
    check f[0].args[1].typ.get.kind == symbolLink
    check f[0].args[1].typ.get.wrapped.kind == symbolGenerics
    check $f[0].args[1].typ.get == "T0"
  test "fib":
    var f = evalTest("""
fib =>
  if n<2
    n
  else
    fib(n-1) + fib(n-2)
""")
    check f[0].getargnames == "n"
    check f[0].getargtypes == "intlit"
    check $f[0].args[1].sons[0].typ.get == "intlit"
