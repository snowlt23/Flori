
import unittest
import compiler.fcore

proc parse(src: string): FExpr =
  parseFExpr("testmodule.flori", src)
let scope = newFScope("testmodule", "testmodule.flori")
proc arginfer(f: FExpr) =
  scope.arginfer(f)

suite "infer test":
  test "add5":
    let fexpr = parse("""
add5 =>
  x + 5
""")

