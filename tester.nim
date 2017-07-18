
import sast
import sparser

let src = """
(defn hello (n)
  (print hello))
"""
let structsrc = """
(defstruct MyInt
  (object
    (x Int32)
    (y Int32)
    (z Int32)))
"""
let fnsrc = """
@(the Int32 -> Int32)
(defn add5 (x)
  (+ x 5))
"""

let srcexpr = parseSExpr(src)
echo srcexpr
echo debug(srcexpr)

let structexpr = parseSExpr(structsrc)
echo structexpr
echo debug(structexpr)

let fnexpr = parseSExpr(fnsrc)
echo fnexpr
echo debug(fnexpr)
