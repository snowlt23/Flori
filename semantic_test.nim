
import sast
import sparser
import semantic

let fnsrc = """
@(the Int32 -> Int32)
(defn add5 (x)
  (+ x 5))
"""

var context = newSemanticContext()
let sexpr = parseSExpr(fnsrc)
context.evalModule("top", sexpr)
