
import unittest
import compiler.parser, compiler.types, compiler.fexpr, compiler.scope, compiler.metadata
import compiler.passmacro, compiler.passdef, compiler.internalpass

let prelude = """
type Void $[importc "void", header nodeclc]
type Bool $[importc "bool", header "stdbool.h"]
type IntLit $[importc "int64_t", header "stdint.h"]
type StrLit $[importc "char*", header nodeclc]

type Int $[importc "int64_t", header "stdint.h"]
type CString $[importc "char*", header nodeclc]

fn int(x ref IntLit) ref Int $[converter, importc, patternc "$1"]
fn int(x IntLit) Int $[converter, importc, patternc "$1"]
fn to_cstring(x StrLit) CString $[converter, importc, patternc "$1"]

fn `+(a Int, b Int) Int $[importc "+", header nodeclc, patternc infixc]
fn `-(a Int, b Int) Int $[importc "-", header nodeclc, patternc infixc]
fn `<(a Int, b Int) Bool $[importc "<", header nodeclc, patternc infixc]
fn `==(a Int, b Int) Bool $[importc "==", header nodeclc, patternc infixc]
fn printf(fmt CString, x Int) $[importc "printf", header "stdio.h"]
"""

suite "semantic step test":
  test "call":
    let ctx = newSemanticContext()
    var fexprs = parseToplevel("testmodule.flori", prelude & """
printf("%d", 9)
""")
    let scope = ctx.newScope(name("testmodule"), "testmodule.flori")
    ctx.semModule(processFPass, name("testmodule"), scope, fexprs)
    check $fexprs[^1][1][0].typ == "CString"
    check $fexprs[^1][1][1].typ == "Int"
    check $fexprs[^1].typ == "Void"
  test "infix call":
    let ctx = newSemanticContext()
    var fexprs = parseToplevel("testmodule.flori", prelude & """
4 + 5
""")
    let scope = ctx.newScope(name("testmodule"), "testmodule.flori")
    ctx.semModule(processFPass, name("testmodule"), scope, fexprs)
    check $fexprs[^1].typ == "Int"
  test "fn":
    let ctx = newSemanticContext()
    var fexprs = parseToplevel("testmodule.flori", prelude & """
fn add5(x Int) Int {
  x + 5
}
add5(4)
""")
    let scope = ctx.newScope(name("testmodule"), "testmodule.flori")
    ctx.semModule(processFPass, name("testmodule"), scope, fexprs)
    check $fexprs[^1].typ == "Int"
  test "if":
    let ctx = newSemanticContext()
    var fexprs = parseToplevel("testmodule.flori", prelude & """
if (1 == 1) {
  1
} else {
  2
}
""")
    let scope = ctx.newScope(name("testmodule"), "testmodule.flori")
    ctx.semModule(processFPass, name("testmodule"), scope, fexprs)
    check $fexprs[^1].typ == "IntLit"
    check fexprs[^1].internalMark == internalIf
  test "while":
    let ctx = newSemanticContext()
    var fexprs = parseToplevel("testmodule.flori", prelude & """
while (1 == 2) {
  printf("%d", 9)
}
""")
    let scope = ctx.newScope(name("testmodule"), "testmodule.flori")
    ctx.semModule(processFPass, name("testmodule"), scope, fexprs)
    check $fexprs[^1].typ == "Void"
    check $fexprs[^1][0] == "while"
    check fexprs[^1].internalMark == internalWhile
  test "local def":
    let ctx = newSemanticContext()
    var fexprs = parseToplevel("testmodule.flori", prelude & """
nine := 9
printf("%d", nine)
""")
    let scope = ctx.newScope(name("testmodule"), "testmodule.flori")
    ctx.semModule(processFPass, name("testmodule"), scope, fexprs)
    check fexprs[^2].internalMark == internalDef
    check $fexprs[^2].typ == "Void"
    check $fexprs[^1][1][1] == "nine"
    check $fexprs[^1][1][1].typ == "Int"
  test "generics init":
    let ctx = newSemanticContext()
    var fexprs = parseToplevel("testmodule.flori", prelude & """
type Wrap[T] {
  x T
}
init(Wrap[Int]){9}
""")
    let scope = ctx.newScope(name("testmodule"), "testmodule.flori")
    ctx.semModule(processFPass, name("testmodule"), scope, fexprs)
    check fexprs[^1].internalMark == internalInit
    check $fexprs[^1].typ == "Wrap[Int]"
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
    let scope = ctx.newScope(name("testmodule"), "testmodule.flori")
    ctx.semModule(processFPass, name("testmodule"), scope, fexprs)
    check $fexprs[^1].typ == "Wrap[IntLit]"
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
    let scope = ctx.newScope(name("testmodule"), "testmodule.flori")
    ctx.semModule(processFPass, name("testmodule"), scope, fexprs)
    check $fexprs[^1][1][1].typ == "Int"
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
      let scope = ctx.newScope(name("testmodule"), "testmodule.flori")
      ctx.semModule(processFPass, name("testmodule"), scope, fexprs)
  test "import":
    let ctx = newSemanticContext()
    var fexprs = parseToplevel("testmodule.flori", prelude & """
import "core/prelude"
""")
    let scope = ctx.newScope(name("testmodule"), "testmodule.flori")
    ctx.semModule(processFPass, name("testmodule"), scope, fexprs)
    check fexprs[^1].internalMark == internalImport
  test "ref type":
    let ctx = newSemanticContext()
    var fexprs = parseToplevel("testmodule.flori", prelude & """
fn `+=(a ref Int, b Int) $[importc, header nodeclc, patternc infixc]
fn add5(x ref Int) {
  x += 5
}
a := 1
add5(a)
""")
    let scope = ctx.newScope(name("testmodule"), "testmodule.flori")
    ctx.semModule(processFPass, name("testmodule"), scope, fexprs)
    check $fexprs[^1].typ == "Void"
