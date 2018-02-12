
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
  test "call function":
    let fexpr = parseToplevel("test.flori", """
    println("Hello Yukari!")
    """)[0]
    check fexpr.len == 2
    check fexpr.kind == fexprSeq
    check fexpr[0].kind == fexprIdent
    check $fexpr[0] == "println"
    check fexpr[1].kind == fexprList
    check fexpr[1][0].kind == fexprStrLit
    check $fexpr[1][0] == "\"Hello Yukari!\""
    check $fexpr == "println(\"Hello Yukari!\")"
  test "if":
    let fexpr = parseToplevel("test.flori", """
    if (true) {
      println("True!")
    } else {
      println("False!")
    }
    """)[0]
    check fexpr.kind == fexprSeq
    check fexpr[0].kind == fexprIdent
    check $fexpr[0] == "if"
    check fexpr[1].kind == fexprList
    check $fexpr[1][0] == "true"
    check fexpr[2].kind == fexprBlock
    check fexpr[3].kind == fexprIdent
    check $fexpr[3] == "else"
    check fexpr[4].kind == fexprBlock
  test "pragma":
    let fexpr = parseToplevel("test.flori", """
    $[importc "test", header nodeclc]
    """)[0]
    check fexpr.kind == fexprSeq
    check $fexpr[0] == "$"
    check $fexpr[1] == "[importc \"test\", header nodeclc]"
  test "fn":
    let fexpr = parseToplevel("test.flori", """
    fn add5(x Int) Int {
      x + 5
    }
    """)[0]
    check fexpr.kind == fexprSeq
    check fexpr[0].kind == fexprIdent
    check $fexpr[0] == "fn"
    check fexpr[1].kind == fexprIdent
    check $fexpr[1] == "add5"

    check fexpr[2].kind == fexprList
    check fexpr[2][0].kind == fexprSeq
    check $fexpr[2][0][0] == "x"
    check $fexpr[2][0][1] == "Int"

    check $fexpr[3] == "Int"

    check fexpr[4].kind == fexprBlock
    check fexpr[4][0].kind == fexprSeq
    check $fexpr[4][0][0] == "+"
    check $fexpr[4][0][1] == "x"
    check $fexpr[4][0][2] == "5"
