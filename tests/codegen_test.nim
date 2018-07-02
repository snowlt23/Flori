
import compiler.fcore, compiler.passmacro, compiler.internalpass
import compiler.codegen.tacode, compiler.codegen.tagen
import compiler.codegen.x86code, compiler.codegen.x86tiling, compiler.codegen.x86gen, compiler.codegen.jit
import compiler.codegen.taopt, compiler.codegen.liveness
import unittest

let prelude = """
`+ =>
  $typed(int, int)
  internalop("int_add")
`- =>
  $typed(int, int)
  internalop("int_sub")
`< =>
  $typed(int, int)
  internalop("int_lesser")
"""

template instImage(testname: string) =
  initRootScope()
  var tactx {.inject.} = newTAContext()
  let scope = newFScope(testname, testname & ".flori")
  scope.importFScope(internalScope.obj.name, internalScope)
  proc evaltest(src: string): seq[FExpr] {.discardable.} =
    result = parseToplevel(testname & ".flori", src)
    for f in result.mitems:
      scope.rootPass(f)
      discard tactx.convertFExpr(f)
  evaltest(prelude)

template jittest(): pointer =
  let jitbuf = initJitBuffer(1024)
  let p = toProc[pointer](jitbuf.getproc())
  var asmctx = newAsmContext(jitbuf)
  var x86ctx = tactx.optimize().x86Tiling().freqRegalloc(tactx.analyzeLiveness())
  asmctx.generateX86(x86ctx)
  p

{.push stackTrace:off.}
proc procffi*(p: pointer, n: int32): int32 =
  asm """
    movl %1, %%ecx
    movl %2, %%edx
    call %%edx
    movl %%eax, %0
    :"=a"(`result`)
    :"c"(`n`), "d"(`p`)
  """
{.pop.}

suite "codegen test":
  test "add5":
    instImage("add5")
    evaltest("add5 => x + 5")
    check procffi(jittest(), 4) == 9
  test "fib rec":
    instImage("fibrec")
    evaltest("""
fib =>
  if n<2: n
  else: fib(n-1) + fib(n-2)
""")
    check procffi(jittest(), 38) == 39088169
  test "fib loop":
    instImage("fibloop")
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
    check procffi(jittest(), 38) == 39088169
