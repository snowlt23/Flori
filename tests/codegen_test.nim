
import compiler.fcore, compiler.passmacro, compiler.internalpass
import compiler.codegen.tacode, compiler.codegen.tagen
import compiler.codegen.x86code, compiler.codegen.x86tiling, compiler.codegen.x86gen, compiler.codegen.jit
import compiler.codegen.taopt, compiler.codegen.liveness, compiler.codegen.address
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
  var plat = newX86Platform()
  gCtx.notevals = @[]
  proc evaltest(src: string): seq[FExpr] {.discardable.} =
    result = parseToplevel(testname & ".flori", src)
    for f in result.mitems:
      var asmctx = newAsmContext(jitbuf)
      var tafn = emptyTAFn()
      scope.rootPass(f)

      for f in gCtx.notevals:
        var tafn = emptyTAFn()
        discard tafn.convertFExpr(f)
        tafn = tafn.optimize()
        let liveness = tafn.analyzeLiveness()
        let addrtable = tafn.analyzeAddress()
        var (x86fn, x86plat) = tafn.x86Tiling().freqRegalloc(liveness, addrtable, plat)
        plat = asmctx.generateX86(x86fn, x86plat)
      gCtx.notevals = @[]

      discard tafn.convertFExpr(f)
      tafn = tafn.optimize()
      let liveness = tafn.analyzeLiveness()
      let addrtable = tafn.analyzeAddress()
      var (x86fn, x86plat) = tafn.x86Tiling().freqRegalloc(liveness, addrtable, plat)
      plat = asmctx.generateX86(x86fn, x86plat)
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

