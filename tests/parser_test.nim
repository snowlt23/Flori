
import unittest
import compiler.image, compiler.fexpr, compiler.lexer, compiler.parser

proc parse(src: string): FExpr =
  parseFExpr("testmodule.flori", src)

suite "F expression parser test":
  test "infix":
    let fexpr = parse("""
1 + 2 * 3
""")
    check fexpr.kind == fexprInfix
    check $fexpr.call == "+"
    check fexpr.args[0].kind == fexprIntLit
    check $fexpr.args[0] == "1"
    check fexpr.args[1].kind == fexprInfix
    check $fexpr.args[1].call == "*"
    check $fexpr.args[1] == "2 * 3"
  test "call":
    let fexpr = parse("""
fib(n-1) + fib(n-2)
""")
    check fexpr.kind == fexprInfix
    check $fexpr.call == "+"
    check $fexpr.args[0] == "fib(n - 1)"
    check $fexpr.args[1] == "fib(n - 2)"
  test "block":
    let fexpr = parse("""
add5 =>
  x + 5
""")
    check fexpr.kind == fexprInfix
    check $fexpr.call == "=>"
    check $fexpr.args[0] == "add5"
    check fexpr.args[1].kind == fexprInfix
    check $fexpr.args[1] == "x + 5"
  test "fib":
    let fexpr = parse("""
fib =>
  if n<2:
    n
  else:
    fib(n-1) + fib(n-2)
""")
    check fexpr.kind == fexprInfix
    check $fexpr.call == "=>"
    check $fexpr.args[0] == "fib"
    check fexpr.args[1].kind == fexprIf
    check $fexpr.args[1].ifbranch.args[0] == "n < 2"
    check $fexpr.args[1].ifbranch.args[1].sons[0] == "n"
  test "attributed word":
    let fexpr = parse("""
vec3 => $template $struct(x, y, z)
  $typed(int, int, int)
""")
    check fexpr.kind == fexprInfix
    check $fexpr.call == "=>"
    check $fexpr.args[0] == "vec3"
    check $fexpr.args[1].sons[0] == "$template"
    check $fexpr.args[1].sons[1] == "$struct(x, y, z)"
    check $fexpr.args[1].sons[2] == "$typed(int, int, int)"
