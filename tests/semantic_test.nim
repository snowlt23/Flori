
import unittest
import tables
import options
import compiler.types, compiler.fexpr, compiler.parser
import compiler.scope, compiler.semantic, compiler.internal

let prelude = """
type Void $[importc "void", header nodeclc]
type Bool $[importc "bool", header "stdbool.h"]
type CString $[importc "char*", header nodeclc]
type Int $[importc "int64_t", header "stdint.h"]

fn `+(a Int, b Int) Int $[importc "+", header nodeclc, pattern infixc]
fn `==(a Int, b Int) Bool $[importc "==", header nodeclc, pattern infixc]
fn printf(fmt CString, x Int) $[importc "printf", header "stdio.h"]
"""

suite "semantic":
  test "infix":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      1 + 1
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check fexprs[^1].typ.isSome
    check $fexprs[^1].typ.get == "testmodule.Int"
  test "call":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      printf("%d", 9)
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check fexprs[^1].typ.isSome
    check $fexprs[^1].typ.get == "testmodule.Void"
  test "if":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      if (1 == 1) {
        1
      } else {
        2
      }
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check fexprs[^1].typ.isSome
    check $fexprs[^1].typ.get == "testmodule.Int"
  test "while":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      while (1 == 2) {
        printf("%d", 9)
      }
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check $fexprs[^1].typ.get == "testmodule.Void"
    check $fexprs[^1][0] == "while"
    check fexprs[^1].internalMark == internalWhile
  test "toplevel def":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      nine := 9
      printf("%d", nine)
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check fexprs[^2].internalMark == internalDef
    check $fexprs[^2].typ.get == "testmodule.Void"
    check $fexprs[^1][2] == "nine"
    check $fexprs[^1][2].typ.get == "testmodule.Int"
  test "local def":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      fn test() {
        name := "feelsgoodman"
      }
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check fexprs[^1][3][0].internalMark == internalDef
    check $fexprs[^1][3][0].typ.get == "testmodule.Void"
  test "deftype generics":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      type Vec[T] {
        p Ptr[T]
        len Int
      }
      fn genvec() Vec!Int {}
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check fexprs[^2].internalDeftypeExpr.generics.isSome
    let retsym = fexprs[^1].internalDefnExpr.ret.symbol
    check $retsym == "testmodule.Vec"
    check $retsym.types[0] == "testmodule.Int"
