
import unittest
import compiler.fexpr, compiler.parser

suite "F expression parser test":
  test "atom":
    let fexprs = parseToplevel("test.flori", """
    9
    "Yukari"
    Maki
    """)
    check fexprs[0].kind == fexprIntLit
    check $fexprs[0] == "9"
    check fexprs[1].kind == fexprStrLit
    check $fexprs[1] == "\"Yukari\""
    check fexprs[2].kind == fexprIdent
    check $fexprs[2] == "Maki"
  test "list":
    let fexprs = parseToplevel("test.flori", """
    (1, 2, 3)
    """)[0]
    check fexprs.kind == fexprList
    check $fexprs == "(1, 2, 3)"
  test "array":
    let fexprs = parseToplevel("test.flori", """
    [1, 2, 3]
    """)[0]
    check fexprs.kind == fexprArray
    check $fexprs == "[1, 2, 3]"
  test "block":
    let fexpr = parseToplevel("test.flori", """
    {
      1
      2
      3
    }
    """)[0]
    check fexpr.kind == fexprBlock
    check $fexpr == """
{
  1
  2
  3
}"""
  test "seq block":
    let fexpr = parseToplevel("test.flori", """
    loop {
      1
      2
      3
    }
    """)[0]
    check fexpr.len == 2
    check fexpr.kind == fexprSeq
    check fexpr[0].kind == fexprIdent
    check fexpr[1].kind == fexprBlock
    check $fexpr == """
loop {
  1
  2
  3
}"""
  test "seq if":
    let fexprs = parseToplevel("test.flori", """
    if (true) {
      1
    } else {
      2
    }
    """)
    check fexprs[0].len == 5
    check fexprs[0][0].kind == fexprIdent
    check $fexprs[0][0] == "if"
    check fexprs[0][1].kind == fexprList
    check fexprs[0][2].kind == fexprBlock
    check fexprs[0][3].kind == fexprIdent
    check $fexprs[0][3] == "else"
    check fexprs[0][4].kind == fexprBlock
    check $fexprs[0] == """
if (true) {
  1
} else {
  2
}"""
  test "fn":
    let fexpr = parseToplevel("test.flori", """
    fn add5(x Int32) Int32 {
      x + 5
    }
    """)[0]
    check fexpr[0].kind == fexprIdent
    check $fexpr[0] == "fn"
    check fexpr[1].kind == fexprIdent
    check $fexpr[1] == "add5"
    check fexpr[2].kind == fexprList
    check fexpr[3].kind == fexprIdent
    check fexpr[4].kind == fexprBlock
    check fexpr[4][0].kind == fexprCall
    check $fexpr[4][0][0] == "+"
    check $fexpr[4][0][1] == "x"
    check $fexpr[4][0][2] == "5"
  test "var":
    let fexpr = parseToplevel("test.flori", """
    x := 9
    """)[0]
    check fexpr.kind == fexprCall
    check fexpr[0].kind == fexprInfix
    check $fexpr[0] == ":="
    check fexpr[1].kind == fexprIdent
    check $fexpr[1] == "x"
    check fexpr[2].kind == fexprIntLit
    check $fexpr[2] == "9"
  test "fn pragma":
    let fexpr = parseToplevel("test.flori", """
    fn `+(a Int32, b Int32) Int32 [importc, nodecl, infix]
    """)[0]
    check fexpr[0].kind == fexprIdent
    check $fexpr[0] == "fn"
    check fexpr[1].kind == fexprQuote
    check $fexpr[1] == "`+"
    check fexpr[1].quoted.kind == fexprInfix
    check $fexpr[1].quoted == "+"
  test "struct pragma":
    let fexpr = parseToplevel("test.flori", """
    struct Int32 [importc "int32_t", header "stdint.h"]
    """)[0]
    check fexpr[0].kind == fexprIdent
    check $fexpr[0] == "struct"
    check fexpr[1].kind == fexprIdent
    check $fexpr[1] == "Int32"
    check fexpr[2].kind == fexprArray
