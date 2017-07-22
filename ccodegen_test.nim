
import sast
import sparser
import semantic
import ccodegen
import compile

import tables
import sequtils

let fnsrc = """
@(header "math.h")
@(: Int32 -> Int32)
(c-ffi abs "abs")

@(: Int32 -> Int32)
(defn add5 [x]
  (abs (+ x 5)))

(add5 1)
"""

var semcontext = newSemanticContext()
let sexpr = parseToplevel(fnsrc)
echo sexpr
semcontext.evalModule("top", sexpr)

var cgencontext = newCCodegenContext()
cgencontext.genContext(semcontext)
cgencontext.compile("add")

echo semcontext.modules["top"].semanticexprs
# echo cgencontext.getMainSrc()
