
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
