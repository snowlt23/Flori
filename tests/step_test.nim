
import unittest
import compiler.parser, compiler.types, compiler.fexpr, compiler.scope, compiler.metadata
import compiler.passmacro, compiler.passdef, compiler.internalpass

let prelude = """
type Void $[importc "void", header nodeclc]
type Bool $[importc "bool", header "stdbool.h"]
type CString $[importc "char*", header nodeclc]
type Int $[importc "int64_t", header "stdint.h"]

fn `+(a Int, b Int) Int $[importc "+", header nodeclc, pattern infixc]
fn `-(a Int, b Int) Int $[importc "-", header nodeclc, pattern infixc]
fn `<(a Int, b Int) Bool $[importc "<", header nodeclc, pattern infixc]
fn `==(a Int, b Int) Bool $[importc "==", header nodeclc, pattern infixc]
fn printf(fmt CString, x Int) $[importc "printf", header "stdio.h"]
"""

suite "semantic step test":
  test "call":
    let ctx = newSemanticContext()
    var fexprs = parseToplevel("testmodule.flori", prelude & """
printf("%d", 9)
""")
    ctx.semModule(processSemPass, name("testmodule"), fexprs)
    check $fexprs[^1][1][0].typ == "testmodule.CString"
    check $fexprs[^1][1][1].typ == "testmodule.Int"
    check $fexprs[^1].typ == "testmodule.Void"
  test "infix call":
    let ctx = newSemanticContext()
    var fexprs = parseToplevel("testmodule.flori", prelude & """
4 + 5
""")
    ctx.semModule(processSemPass, name("testmodule"), fexprs)
    check $fexprs[^1].typ == "testmodule.Int"
  test "fn":
    let ctx = newSemanticContext()
    var fexprs = parseToplevel("testmodule.flori", prelude & """
fn add5(x Int) Int {
  x + 5
}
add5(4)
""")
    ctx.semModule(processSemPass, name("testmodule"), fexprs)
    check $fexprs[^1].typ == "testmodule.Int"
  test "if":
    let ctx = newSemanticContext()
    var fexprs = parseToplevel("testmodule.flori", prelude & """
if (1 == 1) {
  1
} else {
  2
}
""")
    ctx.semModule(processSemPass, name("testmodule"), fexprs)
    check $fexprs[^1].typ == "testmodule.Int"
    check fexprs[^1].internalMark == internalIf
  test "while":
    let ctx = newSemanticContext()
    var fexprs = parseToplevel("testmodule.flori", prelude & """
while (1 == 2) {
  printf("%d", 9)
}
""")
    ctx.semModule(processSemPass, name("testmodule"), fexprs)
    check $fexprs[^1].typ == "testmodule.Void"
    check $fexprs[^1][0] == "while"
    check fexprs[^1].internalMark == internalWhile
  test "local def":
    let ctx = newSemanticContext()
    var fexprs = parseToplevel("testmodule.flori", prelude & """
nine := 9
printf("%d", nine)
""")
    ctx.semModule(processSemPass, name("testmodule"), fexprs)
    check fexprs[^2].internalMark == internalDef
    check $fexprs[^2].typ == "testmodule.Void"
    check $fexprs[^1][1][1] == "testmodule.nine"
    check $fexprs[^1][1][1].typ == "testmodule.Int"
  test "generics init":
    let ctx = newSemanticContext()
    var fexprs = parseToplevel("testmodule.flori", prelude & """
type Wrap[T] {
  x T
}
init(Wrap[Int]){9}
""")
    ctx.semModule(processSemPass, name("testmodule"), fexprs)
    check fexprs[^1].internalMark == internalInit
    check $fexprs[^1].typ == "testmodule.Wrap[testmodule.Int]"
  test "generics fn":
    let ctx = newSemanticContext()
    var fexprs = parseToplevel("testmodule.flori", prelude & """
type Wrap[T] {
  x T
}
fn wrap[T](x T) Wrap[T] {
  init(Wrap[T]){x}
}
wrap(9)
""")
    ctx.semModule(processSemPass, name("testmodule"), fexprs)
    check $fexprs[^1].typ == "testmodule.Wrap[testmodule.Int]"
  test "recursion call":
    let ctx = newSemanticContext()
    var fexprs = parseToplevel("testmodule.flori", prelude & """
fn fib(n Int) Int {
  if (n < 2) {
    n
  } else {
    fib(n-1) + fib(n-2)
  }
}

printf("%d\n", fib(30))
""")
    ctx.semModule(processSemPass, name("testmodule"), fexprs)
    check $fexprs[^1][1][1].typ == "testmodule.Int"
  test "generics call fn":
    let ctx = newSemanticContext()
    var fexprs = parseToplevel("testmodule.flori", prelude & """
type Wrap[T] {
  x T
}
fn wrap[T](x T) Wrap[T] {
  init(Wrap){x}
}
fn main() {
  wrap[Int](9)
}
main()
""")
  test "generics cannot instantiate":
    let ctx = newSemanticContext()
    var fexprs = parseToplevel("testmodule.flori", prelude & """
type Wrap[T] {
  x T
}
fn wrap[T](x Int) Wrap[T] {
}
wrap(9)
""")
    expect(FExprError):
      ctx.semModule(processSemPass, name("testmodule"), fexprs)
  test "import":
    let ctx = newSemanticContext()
    var fexprs = parseToplevel("testmodule.flori", prelude & """
import core/prelude
""")
    ctx.semModule(processSemPass, name("testmodule"), fexprs)
    check fexprs[^1].internalMark == internalImport
    
