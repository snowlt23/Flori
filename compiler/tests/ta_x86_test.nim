

import ../fcore, ../passmacro, ../internalpass
import ../codegen/tacode, ../codegen/tagen, ../codegen/taopt, ../codegen/ta_x86

import tables
import os, osproc, strutils

let prelude = """
type Bool $[internalsize 4] {}

fn `+(a IntLit, b IntLit) IntLit $[internalop "+"]
fn `-(a IntLit, b IntLit) IntLit $[internalop "-"]
fn `<(a IntLit, b IntLit) Bool $[internalop "<"]
"""

initRootScope()
var tactx = newTAContext()

let scope = newFScope("testmodule", "testmodule.flori")
scope.importFScope(internalScope.obj.name, internalScope)
proc evalTest*(src: string): seq[FExpr] =
  result = parseToplevel("testmodule.flori", src)
  for f in result.mitems:
    scope.rootPass(f)
    discard tactx.convertFExpr(f)
discard evalTest(prelude)

# let fexprs = evalTest("""
# fn fib(x IntLit) IntLit {
#   if (x < 2) {
#     x
#   } else {
#     fib(x-1) + fib(x-2)
#   }
# }
# fib(30)
# """)
let fexprs = evalTest("""
fn add5(x IntLit) IntLit {
  a := x + 5
  a
}
fn sub5(x IntLit) IntLit {
  x - 5
}
add5(4)
sub5(9)
""")

for f in fexprs:
  echo f

var asmctx = newAsmContext(newSeq[uint8]())
for i, c in tactx.codes:
  if tactx.revlabels.hasKey(i):
    asmctx.addLabel(tactx.revlabels[i])
  asmctx.generateFromTACode(c)
var bin = ""
for b in asmctx.buffer:
  bin.add(cast[char](b))
writeFile("fib.bin", bin)

echo "\n=>\n"
let outp = execProcess("objdump -b binary -M intel -m i386 -D fib.bin")
echo outp.split("\n")[6..^1].join("\n")
