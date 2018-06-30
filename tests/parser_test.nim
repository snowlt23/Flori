
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
    check fexpr.args[1].kind == fexprBlock
    check $fexpr.args[1].sons[0] == "x + 5"
  test "fib":
    let fexpr = parse("""
fib =>
  if n<2
    n
  else
    fib(n-1) + fib(n-2)
""")
    check fexpr.kind == fexprInfix
    check $fexpr.call == "=>"
    check $fexpr.args[0] == "fib"
    check fexpr.args[1].sons[0].kind == fexprIf
    check $fexpr.args[1].sons[0].ifcond == "n < 2"
    # check $fexpr.args[1].kind == fexprIf
    # check $fexpr.args[1].ifcond == "n < 2"
    # check $fexpr.args[1].ifbody == "n"
