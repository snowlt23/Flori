

import ../fcore, ../passmacro, ../internalpass
import ../codegen/tacode, ../codegen/tagen, ../codegen/taopt, ../codegen/ta_x86, ../codegen/jit

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
""")

for f in fexprs:
  echo f

let jitbuf = initJitBuffer(1024)
let add5 = toProc[proc (a: int32): int32 {.cdecl.}](jitbuf.getproc())
var asmctx = newAsmContext(jitbuf)
for i, c in tactx.codes:
  if tactx.revlabels.hasKey(i):
    asmctx.addLabel(tactx.revlabels[i])
  asmctx.generateFromTACode(c)
var bin = ""
for b in asmctx.buffer:
  bin.add(cast[char](b))
writeFile("fib.bin", bin)

echo "\n=> TACode\n"
stdout.write(tactx)

echo "=> x86\n"
let outp = execProcess("objdump -b binary -M intel -m i386 -D fib.bin")
echo outp.split("\n")[6..^1].join("\n")

echo "add5(4) => ", add5(4)
