
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
  if (n < 2) {
    n
  } else {
    fib(n-1) + fib(n-2)
  }
}
""")

for f in fexprs:
  echo f
echo tactx

let jitbuf = initJitBuffer(1024)
let fib = toProc[proc (a: int32): int32 {.cdecl.}](jitbuf.getproc())
var asmctx = newAsmContext(jitbuf)

# var x86ctx = tactx.x86Tiling().naiveRegalloc()
var x86ctx = tactx.x86Tiling().simpleRegalloc(tactx.analyzeLiveness())
# var x86ctx = tactx.optimize().x86Tiling().simpleRegalloc(tactx.analyzeLiveness())
echo x86ctx

asmctx.generateX86(x86ctx)
echo objdump(asmctx.buffer.toBin)

bench:
  echo "fib(38) => ", fib(38)
