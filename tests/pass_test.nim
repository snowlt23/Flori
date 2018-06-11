
import unittest
import options
import compiler.fcore, compiler.internalpass, compiler.passmacro

let prelude = """
type Bool {}

fn `+(a IntLit, b IntLit) IntLit
fn `-(a IntLit, b IntLit) IntLit
fn `<(a IntLit, b IntLit) Bool
fn `==(a IntLit, b IntLit) Bool
fn printf(fmt StrLit, x IntLit)
"""

initRootScope()
let scope = newFScope("testmodule", "testmodule.flori")
scope.importFScope(internalScope.obj.name, internalScope)
proc evalTest*(src: string): seq[FExpr] =
  result = parseToplevel("testmodule.flori", src)
  for f in result.mitems:
    scope.rootPass(f)
discard evalTest(prelude)

suite "semantic pass test":
  test "call":
    var fexprs = evalTest("""
printf("%d", 9)
""")
    check $fexprs[^1][1][0].typ.get == "StrLit"
    check $fexprs[^1][1][1].typ.get == "IntLit"
    check $fexprs[^1].typ.get == "Void"
  test "infix call":
    var fexprs = evalTest("""
4 + 5
""")
    check $fexprs[^1].typ.get == "IntLit"
  test "fn":
    var fexprs = evalTest("""
fn add5(x IntLit) IntLit {
  x + 5
}
add5(4)
""")
    check $fexprs[^1].typ.get == "IntLit"
  test "if":
    var fexprs = evalTest("""
if (1 == 1) {
  1
} else {
  2
}
""")
    check $fexprs[^1].typ.get == "IntLit"
  test "while":
    var fexprs = evalTest("""
while (1 == 2) {
  printf("%d", 9)
}
""")
    check $fexprs[^1].typ.get == "Void"
    check $fexprs[^1][0] == "while"
  test "local def":
    var fexprs = evalTest("""
nine := 9
printf("%d", nine)
""")
    check $fexprs[^2].typ.get == "Void"
    check $fexprs[^1][1][1] == "nine"
    check $fexprs[^1][1][1].typ.get == "IntLit"
  test "generics init":
    var fexprs = evalTest("""
type Wrap[T] {
  x T
}
init(Wrap[IntLit]){9}
""")
    check $fexprs[^1].typ.get == "Wrap[IntLit]"
  test "generics fn":
    var fexprs = evalTest("""
type Wrap[T] {
  x T
}
fn wrap[T](x T) Wrap[T] {
  init(Wrap[T]){x}
}
wrap(9)
""")
    check $fexprs[^1].typ.get == "Wrap[IntLit]"
  test "recursion call":
    var fexprs = evalTest("""
fn fib(n IntLit) IntLit {
  if (n < 2) {
    n
  } else {
    fib(n-1) + fib(n-2)
  }
}

printf("%d\n", fib(30))
""")
    check $fexprs[^1][1][1].typ.get == "IntLit"
  test "generics call fn":
    var fexprs = evalTest("""
type Wrap[T] {
  x T
}
fn wrap[T](x T) Wrap[T] {
  init(Wrap[T]){x}
}
fn main() {
  w := wrap[IntLit](9)
}
main()
""")
  test "generics cannot instantiate":
    expect(FExprError):
      var fexprs = evalTest("""
type Wrap[T] {
  x T
}
fn wrap[T](x IntLit) Wrap[T] {
}
wrap(9)
""")
  # test "import":
#     var fexprs = evalTest("""
# import "core/prelude"
# """)
  test "ref type":
    var fexprs = evalTest("""
fn `+=(a ref IntLit, b IntLit)
fn add5(x ref IntLit) {
  x += 5
}
a := 1
add5(a)
""")
    check $fexprs[^1].typ.get == "Void"
  test "pragma test":
    var fexprs = evalTest("""
fn `+(a IntLit, b IntLit) $[internalop "+"]
fn `-(a IntLit, b IntLit) $[internalop "-"]
""")
    check fexprs[0].obj.internal.get.obj.internalop == internalAdd
    check fexprs[1].obj.internal.get.obj.internalop == internalSub
