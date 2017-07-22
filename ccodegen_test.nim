
import sast
import sparser
import semantic
import ccodegen
import compile

import tables
import sequtils

let fnsrc = """
@(: Int32 -> Int32)
(c-ffi abs :header "math.h")

@(: String)
(c-ffi print :name "printf" :header "stdio.h")

@(: Int32 -> Int32)
(defn add5 [x]
  (abs (+ x 5)))

(add5 1)
(print "Hello Yukari!\n")
"""

var semcontext = newSemanticContext()
let sexpr = parseToplevel(fnsrc)
echo sexpr
semcontext.evalModule("top", sexpr)

var cgencontext = newCCodegenContext()
cgencontext.genContext(semcontext)
cgencontext.compile("hello")

echo semcontext.modules["top"].semanticexprs
# echo cgencontext.getMainSrc()
