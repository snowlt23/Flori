
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
echo tactx.optimize()

let jitbuf = initJitBuffer(1024)
# let fib = toProc[proc (a: int32): int32 {.cdecl.}](jitbuf.getproc())
let fibp = toProc[pointer](jitbuf.getproc())
var asmctx = newAsmContext(jitbuf)

# var x86ctx = tactx.x86Tiling().naiveRegalloc()
# var x86ctx = tactx.x86Tiling().simpleRegalloc(tactx.analyzeLiveness())
# var x86ctx = tactx.optimize().x86Tiling().simpleRegalloc(tactx.analyzeLiveness())
var x86ctx = tactx.optimize().x86Tiling().freqRegalloc(tactx.analyzeLiveness())
echo x86ctx

asmctx.generateX86(x86ctx)
echo objdump(asmctx.buffer.toBin)

import dynlib
let lib = loadLib("fib.dll")
let cfib = cast[proc (n: int32): int32 {.cdecl.}](lib.checkedSymAddr("fib"))

bench:
  echo "GCC -O2: fib(38) => ", cfib(38)
bench:
  echo "Flori: fib(38) => ", procffi(38, fibp)
