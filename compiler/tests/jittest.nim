
import ../codegen/jit
import ../codegen/asm_x86

import osproc
import strutils

proc main() =
  var buf = initJitBuffer(1024)
  let jit_add = toProc[proc (a: int32, b: int32): int32 {.cdecl.}](buf.getproc())
  buf.enter(0, 0)
  buf.mov(eax, esp, 8)
  buf.mov(ebx, esp, 12)
  buf.add(eax, ebx)
  buf.leave()
  buf.ret()
  
  writeFile("jit.bin", buf.toBin(0, buf.len-1))
  let outp = execProcess("objdump -b binary -M intel -m i386 -D jit.bin")
  echo outp.split("\n")[6..^1].join("\n")
  
  echo jit_add(4, 5)
main()
