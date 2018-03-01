
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
