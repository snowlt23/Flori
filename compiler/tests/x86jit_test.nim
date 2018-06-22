

import ../fcore, ../passmacro, ../internalpass
import ../codegen/tacode, ../codegen/liveness, ../codegen/tagen, ../codegen/x86code, ../codegen/x86gen, ../codegen/jit
import ../codegen/x86tiling, ../codegen/taopt

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
# fn add5(x IntLit) IntLit {
#   a := x + 5
#   a
# }
# """)
# let fexprs = evalTest("""
# fn fib(n IntLit) IntLit {
#   if (n < 2) {
#     n
#   } else {
#     fib(n-1) + fib(n-2)
#   }
# }
# """)
let fexprs = evalTest("""
fn fib(n IntLit) IntLit {
  a := 0
  b := 1
  c := 0
  i := 1
  while (i < n) {
    c = a + b
    a = b
    b = c
    i = i + 1
  }
  c
}
""")

for f in fexprs:
  echo f
echo tactx

let jitbuf = initJitBuffer(1024)
let fib = toProc[proc (a: int32): int32 {.cdecl.}](jitbuf.getproc())
var asmctx = newAsmContext(jitbuf)

# echo tactx
# echo "=>\n"
# echo tactx.optimize()
# echo tactx.toX86Context()
echo tactx
echo tactx.analyzeLiveness()
# var x86ctx = tactx.x86Tiling().naiveRegalloc()
# echo tactx.x86Tiling()
var x86ctx = tactx.x86Tiling().simpleRegalloc(tactx.analyzeLiveness())
# var x86ctx = tactx.optimize().x86Tiling().simpleRegalloc(tactx.analyzeLiveness())
# echo "\n=> X86Code\n"
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
