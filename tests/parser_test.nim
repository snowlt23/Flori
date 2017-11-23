
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
    (1 2 3)
    """)[0]
    check fexprs.kind == fexprList
    check $fexprs == "(1 2 3)"
  test "array":
    let fexprs = parseToplevel("test.flori", """
    [1 2 3]
    """)[0]
    check fexprs.kind == fexprArray
    check $fexprs == "[1 2 3]"
  test "map":
    let fexpr = parseToplevel("test.flori", """
    {:importc "+" :header nodecl :pattern infix}
    """)[0]
    check fexpr.kind == fexprMap
    check $fexpr == "{:importc \"+\" :header nodecl :pattern infix}"
  test "println":
    let fexpr = parseToplevel("test.flori", """
    (println "Hello Yukari!")
    """)[0]
    check fexpr.len == 2
    check fexpr.kind == fexprList
    check fexpr.car.kind == fexprIdent
    check $fexpr.car == "println"
    check fexpr.cdr.car.kind == fexprStrLit
    check $fexpr.cdr.car == "\"Hello Yukari!\""
    check $fexpr == "(println \"Hello Yukari!\")"
  test "nested list":
    let fexpr = parseToplevel("test.flori", """
    (if true
      (println "True!")
      (println "False!"))
    """)[0]
    check fexpr.kind == fexprList
    check fexpr.car.kind == fexprIdent
    check fexpr.cdr.car.kind == fexprIdent
    check fexpr.cdr.cdr.car.kind == fexprList
    check $fexpr.cdr.cdr.car == "(println \"True!\")"
    check fexpr.cdr.cdr.cdr.car.kind == fexprList
    check $fexpr.cdr.cdr.cdr.car == "(println \"False!\")"
    check $fexpr == """(if true (println "True!") (println "False!"))"""
  test "pragma":
    let fexpr = parseToplevel("test.flori", """
    ${:importc "test" :header nodeclc}
    """)[0]
    check fexpr.kind == fexprList
    check $fexpr[0] == "pragma"
    check $fexpr[1] == "{:importc \"test\" :header nodeclc}"
  test "fn":
    let fexpr = parseToplevel("test.flori", """
    (defn add5 [^int x] ^int
      (+ x 5))
    """)[0]
    check fexpr.kind == fexprList
    check fexpr.car.kind == fexprIdent
    check $fexpr.car == "defn"
    check fexpr.cdr.car.kind == fexprIdent
    check $fexpr.cdr.car == "add5"

    check fexpr.cdr.cdr.car.kind == fexprArray
    check fexpr.cdr.cdr.car[0].kind == fexprList
    check $fexpr.cdr.cdr.car[0] == "^int"
    check $fexpr.cdr.cdr.car[0].car == "type"
    check $fexpr.cdr.cdr.car[0].cdr.car == "int"

    check $fexpr.cdr.cdr.cdr.car == "^int"
    check $fexpr.cdr.cdr.cdr.car.car == "type"
    check $fexpr.cdr.cdr.cdr.car.cdr.car == "int"
