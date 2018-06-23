
import ../fcore, ../passmacro, ../internalpass
import ../codegen/tacode, ../codegen/tagen
import ../codegen/x86code, ../codegen/x86tiling, ../codegen/x86gen, ../codegen/jit
import ../codegen/taopt, ../codegen/liveness
import gentest

import tables
import os, osproc, strutils
import times

let fexprs = evalTest("""
fn add5(n IntLit) IntLit {
  a := n + 5
  a
}
""")

for f in fexprs:
  echo f
echo tactx

let jitbuf = initJitBuffer(1024)
let add5 = toProc[proc (a: int32): int32 {.cdecl.}](jitbuf.getproc())
var asmctx = newAsmContext(jitbuf)

# var x86ctx = tactx.x86Tiling().naiveRegalloc()
# var x86ctx = tactx.x86Tiling().simpleRegalloc(tactx.analyzeLiveness())
var x86ctx = tactx.optimize().x86Tiling().simpleRegalloc(tactx.analyzeLiveness())
echo x86ctx

asmctx.generateX86(x86ctx)
echo objdump(asmctx.buffer.toBin)

bench:
  echo "add5(4) => ", add5(4)
