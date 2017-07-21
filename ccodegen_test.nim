
import sast
import sparser
import semantic
import ccodegen

import tables

let fnsrc = """
@(: Int32 -> Int32)
(defn add5 [x]
  (+ x 5))

(add5 1)
"""

var semcontext = newSemanticContext()
let sexpr = parseToplevel(fnsrc)
semcontext.evalModule("top", sexpr)

var cgencontext = newCCodegenContext()
cgencontext.genContext(semcontext)
# cgencontext.writeToFiles()

echo cgencontext.modules["top"].src
