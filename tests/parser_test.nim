
import unittest
import compiler.ast, compiler.parser

suite "F expression parser test":
  test "atom":
    let fexprs = parseToplevel("test.flori", """
    9
    "Yukari"
    Maki
    """)
    check fexprs[0][0].kind == fexprIntLit
    check $fexprs[0] == "9"
    check fexprs[1][0].kind == fexprStrLit
    check $fexprs[1] == "\"Yukari\""
    check fexprs[2][0].kind == fexprIdent
    check $fexprs[2] == "Maki"
  test "list":
    let fexprs = parseToplevel("test.flori", """
    (1, 2, 3)
    """)
    check fexprs[0].kind == fexprSeq
    check fexprs[0][0].kind == fexprList
    check $fexprs[0] == "(1, 2, 3)"
  test "array":
    let fexprs = parseToplevel("test.flori", """
    [1, 2, 3]
    """)
    check fexprs[0].kind == fexprSeq
    check fexprs[0][0].kind == fexprArray
    check $fexprs[0] == "[1, 2, 3]"
  test "block":
    let fexprs = parseToplevel("test.flori", """
    {
      1
      2
      3
    }
    """)
    check fexprs[0].kind == fexprSeq
    check fexprs[0][0].kind == fexprBlock
    check $fexprs[0] == """
{
1
2
3
}"""
  test "seq block":
    let fexprs = parseToplevel("test.flori", """
    loop {
      1
      2
      3
    }
    """)
    check fexprs[0].len == 2
    check fexprs[0].kind == fexprSeq
    check fexprs[0][0].kind == fexprIdent
    check fexprs[0][1].kind == fexprBlock
    check $fexprs[0] == """
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
    check $fexprs[0][0] == "if"
    check $fexprs[0] == """
if (true) {
1
} else {
2
}"""
