
import ../fcore, ../passmacro, ../internalpass
import ../codegen/tacode, ../codegen/tagen
import ../codegen/x86code, ../codegen/x86tiling, ../codegen/x86gen, ../codegen/jit
import ../codegen/taopt, ../codegen/liveness
import gentest

import tables
import os, osproc, strutils
import times

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
echo tactx.optimize()

let jitbuf = initJitBuffer(1024)
let fibp = toProc[pointer](jitbuf.getproc())
var asmctx = newAsmContext(jitbuf)

# var x86ctx = tactx.x86Tiling().naiveRegalloc()
# var x86ctx = tactx.x86Tiling().simpleRegalloc(tactx.analyzeLiveness())
# var x86ctx = tactx.optimize().x86Tiling().simpleRegalloc(tactx.analyzeLiveness())
var x86ctx = tactx.optimize().x86Tiling().freqRegalloc(tactx.analyzeLiveness())
echo x86ctx

asmctx.generateX86(x86ctx)
echo objdump(asmctx.buffer.toBin)

bench:
  echo "fib(38) => ", procffi(38, fibp)
