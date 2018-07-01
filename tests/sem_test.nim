
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
`+ => $typed(int, int)
  internalop("int_add")
# `+ => $typed(cstring, cstring)
#   $returned(cstring)
`- => $typed(int, int)
  internalop("int_sub")
`< => $typed(int, int)
  internalop("int_lesser")
""")

proc names(f: FExpr): string =
  if f.internal.obj.argnames.isNone:
    f.error("$# hasn't argnames" % $f)
  f.internal.obj.argnames.get.mapIt($it).join(" ")
proc types(f: FExpr): string =
  if f.internal.obj.argtypes.isNone:
    f.error("$# hasn't argtypes" % $f)
  f.internal.obj.argtypes.get.mapIt($it).join(" ")
proc returntype(f: FExpr): string =
  $f.internal.obj.returntype

suite "semantic pass test":
  test "call":
    var f = evalTest("""
add5 => a + 5
""")
    check f[0].names== "a"
    check f[0].types == "int"
    check $f[0].args[1].gettype == "int"
  test "call stmt":
    var f = evalTest("""
add9 =>
  a + 9
""")
    check f[0].names == "a"
    check f[0].types == "int"
    check $f[0].args[1].gettype == "int"
  test "generalize":
    var f = evalTest("""
id => x
""")
    check f[0].names == "x"
    check f[0].types == "T0"
    check f[0].args[1].typ.get.kind == symbolLink
    check f[0].args[1].typ.get.wrapped.kind == symbolGenerics
    check $f[0].args[1].typ.get == "T0"
  test "if":
    var f = evalTest("""
tostr =>
  if b: "true"
  else: "false"
""")
    check f[0].names == "b"
    check f[0].types == "bool"
    check f[0].returntype == "cstring"
  test "fib":
    var f = evalTest("""
fib =>
  if n<2:
    n
  else:
    fib(n-1) + fib(n-2)
""")
    check f[0].names == "n"
    check f[0].types == "int"
    check $f[0].args[1].typ.get == "int"
  test "struct":
    var f1 = evalTest("""
myint => $struct(l: int, r: int)
left => mi.l
right => mi.r
""")
    check f1[0].names == "l r"
    check f1[0].types == "int int"
    check f1[1].names == "mi"
    check f1[1].types == "myint"
    check f1[1].returntype == "int"
    check f1[2].names == "mi"
    check f1[2].types == "myint"
    check f1[2].returntype == "int"
    var f2 = evalTest("""
left1 => left(r) + 1
right1 => right(r) + 1
""")
    check f2[0].names == "r"
    check f2[0].types == "myint"
    check $f2[0].args[1].args[0].gettype == "int"
    check $f2[0].internal.obj.returntype == "int"
    expect FExprError:
      discard evalTest("""
left9 => left(r) + "9"
""")
  test "template struct":
    var f1 = evalTest("""
vector => $template $struct(x, y, z)
""")
    check f1[0].names == "x y z"
    check f1[0].types == "T0 T1 T2"
    var f2 = evalTest("""
`+ => vector(a.x + b.x, a.y + b.y, a.z + b.z)
""")
    check f2[0].names == "a b"
    check f2[0].types == "vector vector"
    check $f2[0].internal.obj.returntype == "vector"
