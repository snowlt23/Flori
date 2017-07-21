
import sast
import sparser
import semantic
import tables

let fnsrc = """
@(: Int32 -> Int32)
(defn add5 [x]
  (+ x 5))

(add5 1)
"""

var context = newSemanticContext()
let sexpr = parseToplevel(fnsrc)
context.evalModule("top", sexpr)
echo context.modules["top"].semanticexprs
