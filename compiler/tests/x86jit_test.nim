

import ../fcore, ../passmacro, ../internalpass
import ../codegen/tacode, ../codegen/liveness, ../codegen/tagen, ../codegen/x86code, ../codegen/x86gen, ../codegen/jit

import tables
import os, osproc, strutils
import times

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
# """)
# let fexprs = evalTest("""
# fn add5(x IntLit) IntLit {
#   a := x + 5
#   a
# }
# """)
let fexprs = evalTest("""
fn fib(n IntLit) IntLit {
  if (n < 2) {
    n
  } else {
    fib(n-1) + fib(n-2)
  }
}
""")

for f in fexprs:
  echo f
# echo tactx

let jitbuf = initJitBuffer(1024)
let fib = toProc[proc (a: int32): int32 {.cdecl.}](jitbuf.getproc())
var asmctx = newAsmContext(jitbuf)

# echo tactx.toX86Context()
# var x86ctx = tactx.toX86Context().naiveRegalloc()
var x86ctx = tactx.toX86Context().simpleRegalloc(tactx.analyzeLiveness())
echo "\n=> X86Code\n"
stdout.write(x86ctx)

asmctx.generateX86(x86ctx)
var bin = ""
for b in asmctx.buffer:
  bin.add(cast[char](b))
writeFile("fib.bin", bin)

let outp = execProcess("objdump -b binary -M intel -m i386 -D fib.bin")
echo "=> x86\n"
echo outp.split("\n")[6..^1].join("\n")

template bench(body: untyped) =
  let s = epochTime()
  body
  let e = epochTime()
  echo "Elapsed: ", e - s, "s"

bench:
  echo "fib(38) => ", fib(38)
