
import ../fcore, ../passmacro, ../internalpass
import ../codegen/tacode, ../codegen/tagen
import os, osproc
import strutils, sequtils
import options
import times

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

initRootScope()
var tactx* = newTAContext()
let scope = newFScope("testmodule", "testmodule.flori")
scope.importFScope(internalScope.obj.name, internalScope)
proc evalTest*(src: string): seq[FExpr] =
  result = parseToplevel("testmodule.flori", src)
  for f in result.mitems:
    scope.rootPass(f)
    discard tactx.convertFExpr(f)
discard evalTest(prelude)

proc inferred*(f: FExpr): string =
  "$# => $$typed($#) $$named($#) $$returned($#)" % [$f.args[0], f.internal.obj.argtypes.get.mapIt($it).join(", "), f.internal.obj.argnames.get.mapIt($it).join(", "), $f.internal.obj.returntype]

{.push stackTrace:off.}
proc procffi*(n: int32, p: pointer): int32 =
  asm """
    movl %1, %%ecx
    movl %2, %%edx
    call %%edx
    movl %%eax, %0
    :"=a"(`result`)
    :"c"(`n`), "d"(`p`)
  """
{.pop.}

template bench*(body: untyped) =
  let s = epochTime()
  body
  let e = epochTime()
  echo "Elapsed: ", e - s, "s"

proc objdump*(bin: string): string =
  writeFile("tmp.bin", bin)
  let outp = execProcess("objdump -b binary -M intel -m i386 -D tmp.bin")
  result = outp.split("\n")[6..^1].join("\n")
  removeFile("tmp.bin")
