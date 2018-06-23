
import ../fcore, ../passmacro, ../internalpass
import ../codegen/tacode, ../codegen/tagen
import os, osproc
import strutils, sequtils
import times

let prelude = """
type Bool $[internalsize 4] {}

fn `+(a IntLit, b IntLit) IntLit $[internalop "+"]
fn `-(a IntLit, b IntLit) IntLit $[internalop "-"]
fn `<(a IntLit, b IntLit) Bool $[internalop "<"]
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
