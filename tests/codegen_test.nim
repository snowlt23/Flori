
import compiler.fcore, compiler.passmacro, compiler.internalpass
import compiler.codegen.jit
import compiler.codegen.vop
import compiler.codegen.vop_x86, compiler.codegen.reg_x86, compiler.codegen.gen_x86

import unittest
import os, osproc
import strutils, sequtils
import tables

let prelude = """
`+ => $typed(int, int)
  internalop("int_add")

`- => $typed(int, int)
  internalop("int_sub")

`< => $typed(int, int)
  internalop("int_lesser")

addr => $typed(undef)
  internalop("addr")
deref => $typed(undef)
  internalop("deref")
"""

template instImage(testname: string) =
  initFlori()
  let scope = newFScope(testname, testname & ".flori")
  scope.importFScope(internalScope.obj.name, internalScope)
  let jitbuf = initJitBuffer(1024)
  proc evaltest(src: string): seq[FExpr] {.discardable.} =
    result = parseToplevel(testname & ".flori", src)
    var fn = initVOPFn()
    for f in result.mitems:
      # semantic
      scope.rootPass(f)
      # codegen
      discard fn.vop(f)
    var asmctx = newAsmContext(jitbuf)
    asmctx.generateX86(fn.naiveRegalloc())
  proc getptr(): pointer =
    toProc[pointer](jitbuf.getproc())
  proc objdump(): string =
    objdump(jitbuf.toBin())
  evaltest(prelude)

proc objdump*(bin: string): string =
  writeFile("tmp.bin", bin)
  let outp = execProcess("objdump -b binary -M intel -m i386 -D tmp.bin")
  result = outp.split("\n")[6..^1].join("\n")
  removeFile("tmp.bin")

{.push stackTrace:off.}
proc ffiCall*(p: pointer) =
  asm """
    movl %0, %%edx
    call %%edx
    ::"d"(`p`)
  """
proc ffiInt*(p: pointer): int32 =
  asm """
    movl %1, %%edx
    call %%edx
    movl %%eax, %0
    :"=a"(`result`)
    :"d"(`p`)
  """
proc ffiInt*(p: pointer, n: int32): int32 =
  asm """
    movl %1, %%ecx
    movl %2, %%edx
    call %%edx
    movl %%eax, %0
    :"=a"(`result`)
    :"c"(`n`), "d"(`p`)
  """
proc ffiCString*(p: pointer): cstring =
  asm """
    movl %0, %%edx
    call %%edx
    movl %%eax, %0
    :"=a"(`result`)
    :"d"(`p`)
  """
{.pop.}

suite "codegen test":
  test "basic":
    instImage("basic")
    evaltest("1 + 2")
    echo objdump()
  test "add5":
    instImage("add5")
    let p = getptr()
    evaltest("add5 => x + 5")
    check ffiInt(p, 4) == 9
  test "fib rec":
    instImage("fibrec")
    let p = getptr()
    evaltest("""
fib =>
  if n<2: n
  else: fib(n-1) + fib(n-2)
""")
    check ffiInt(p, 38) == 39088169
  test "fib loop":
    instImage("fibloop")
    let p = getptr()
    evaltest("""
fib =>
  a := 0
  b := 1
  c := 0
  i := 1
  while i<n:
    c = a + b
    a = b
    b = c
    i = i + 1
  c
""")
    check ffiInt(p, 38) == 39088169
  test "strlit":
    instImage("strlit")
    let p = getptr()
    evaltest("""
returnstr => "YUKARI"
""")
    check $ffiCString(p) == "YUKARI"
  test "cffi":
    instImage("cffi")
    let p = getptr()
    evaltest("""
abs => $cffi("abs") $cdecl $dll("msvcrt") $typed(int) $returned(int)
main => abs(x)
""")
    check ffiInt(p, 9) == 9
  test "cffi printf":
    instImage("cffi")
    let p = getptr()
    evaltest("""
printf => $cffi("printf") $cdecl $dll("msvcrt") $typed(cstring, int) $returned(void)
main => printf("%d", 9)
""")
    ffiCall(p)
  test "struct":
    instImage("struct")
    evaltest("""
vector => $struct(x: int, y: int, z: int)
""")
    let p = getptr()
    evaltest("""
main => vector(1, 2, 3)
""")
    ffiCall(p)
  test "struct field":
    instImage("struct")
    evaltest("""
vector => $struct(x: int, y: int, z: int)
""")
    let p = getptr()
    evaltest("""
main => vector(1, 2, 3).y
""")
    check ffiInt(p) == 2
  test "struct ret":
    instImage("struct")
    evaltest("""
vector => $struct(x: int, y: int, z: int)
myvec => vector(1, 2, 3)
""")
    let p = getptr()
    evaltest("""
main => myvec().y
""")
    check ffiInt(p) == 2

