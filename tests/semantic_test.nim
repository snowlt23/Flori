
import unittest
import tables
import options
import compiler.types, compiler.fexpr, compiler.parser
import compiler.scope, compiler.semantic, compiler.internal

let prelude = """
(deftype void ${:importc "void" :header nodeclc})
(deftype bool ${:importc "bool" :header "stdbool.h"})
(deftype cstring ${:importc "char*" :header nodeclc})
(deftype int ${:importc "int64_t" :header "stdint.h"})

(defn + [^int a ^int b] ^int ${:importc "+" :header nodeclc :pattern infixc})
(defn = [^int a ^int b] ^bool ${:importc "==" :header nodeclc :pattern infixc})
(defn printf [^cstring fmt ^int x] ${:importc "printf" :header "stdio.h"})
"""

suite "semantic":
  test "infix":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      (+ 1 1)
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check fexprs[^1].typ.isSome
    check $fexprs[^1].typ.get == "testmodule.int"
  test "call":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      (printf "%d" 9)
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check fexprs[^1].typ.isSome
    check $fexprs[^1].typ.get == "testmodule.void"
  test "if":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      (if (= 1 1)
        1
        2)
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check fexprs[^1].typ.isSome
    check $fexprs[^1].typ.get == "testmodule.int"
  test "while":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      (while (= 1 2)
        (printf "%d" 9))
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check $fexprs[^1].typ.get == "testmodule.void"
    check $fexprs[^1][0] == "while"
    check fexprs[^1].internalMark == internalWhile
  test "toplevel def":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      (def nine 9)
      (printf "%d" nine)
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check fexprs[^2].internalMark == internalDef
    check $fexprs[^2].typ.get == "testmodule.void"
    check $fexprs[^1][2] == "nine"
    check fexprs[^1][2].typ.get.name == name("int")
  test "local def":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      (defn test []
        (def name "feelsgoodman"))
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check fexprs[^1][3].internalMark == internalDef
    check $fexprs[^1][3].typ.get == "testmodule.void"
  test "deftype":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      (deftype vec $[:a] [^(ptr :a) p ^int len])
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check fexprs[^1].internalDeftypeExpr.generics.isSome
