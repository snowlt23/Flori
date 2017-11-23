
import unittest
import tables
import options
import compiler.types, compiler.fexpr, compiler.parser, compiler.scope
import compiler.semantic

let prelude = """
(deftype void ${:importc "void" :header nodeclc})
(deftype bool ${:importc "bool" :header "stdbool.h"})
(deftype cstring ${:importc "char*" :header nodeclc})
(deftype int ${:importc "int64_t" :header nodeclc})

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
