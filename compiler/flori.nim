
import docopt
import os
import strutils

import fcore, passmacro, internalpass, internalffi
import codegen.tacode, codegen.tagen, codegen.taopt, codegen.liveness
import codegen.x86code, codegen.x86tiling, codegen.x86gen
import codegen.jit

let doc = """
Flori programming language.

Usage:
  flori repl

Options:
  -h --help      Show this screen.
  --version      Show version.
"""

let prelude = """
`+ => $typed(int, int)
  internalop("int_add")
`- => $typed(int, int)
  internalop("int_sub")
`-- => 0 - x
`< => $typed(int, int)
  internalop("int_lesser")
printf => $cffi("printf") $cdecl $dll("msvcrt") $typed(cstring, int) $returned(void)
println => printf("%d\n", x)
print_str => $cffi("printf") $cdecl $dll("msvcrt") $typed(cstring) $returned(void)
saveimage => $cffi("saveimage") $cdecl $internalffi $typed(cstring) $returned(void)
loadimage => $cffi("loadimage") $cdecl $internalffi $typed(cstring) $returned(void)
"""

proc ffiCall*(p: pointer) =
  asm """
    movl %0, %%edx
    call %%edx
    ::"d"(`p`)
  """

initRootScope()
var plat = newX86Platform()

import os, osproc
proc objdump*(bin: string): string =
  writeFile("tmp.bin", bin)
  let outp = execProcess("objdump -b binary -M intel -m i386 -D tmp.bin")
  result = outp.split("\n")[6..^1].join("\n")
  removeFile("tmp.bin")

proc saveimageX86(filename: cstring) {.cdecl.} =
  saveimage($filename, plat)
proc loadimageX86(filename: cstring) {.cdecl.} =
  plat = loadimage[X86Platform]($filename)
  gImage.buffer.relocation()
  internalScope.relocInternalEval()
  relocRootScope()
  # echo objdump(gImage.buffer.toBin())

proc initPlatform() =
  addInternalFFI("saveimage", saveimageX86)
  addInternalFFI("loadimage", loadimageX86)

proc startREPL() =
  initPlatform()
  let scope = newFScope("<repl>", "<repl>")
  scope.importFScope(internalScope.obj.name, internalScope)
  var preludef = parseToplevel("<repl>", prelude)
  for f in preludef.mitems:
    scope.rootPass(f)
    var tactx = newTAContext()
    var asmctx = newAsmContext(gImage.buffer)
    discard tactx.convertFExpr(f)
    tactx = tactx.optimize()
    let liveness = tactx.analyzeLiveness()
    var (x86ctx, x86plat) = tactx.x86Tiling().freqRegalloc(liveness, plat)
    plat = asmctx.generateX86(x86ctx, x86plat)

  var s = ""
  while true:
    stdout.write("> ")
    flushFile(stdout)
    var line = ""
    var iscont = stdin.readLine(line)
    if not iscont:
      break
    s &= line
    try:
      var f = parseFExpr("<repl>", s)
      scope.rootPass(f)
      var call = false
      if not (f.kind == fexprInfix and $f.call == "=>"):
        if $f.gettype == "void":
          f = quoteFExpr(f.span, "main => `embed", [f])
        else:
          f = quoteFExpr(f.span, "main => println(`embed)", [f])
        scope.rootPass(f)
        call = true
      let callp = toProc[pointer](gImage.buffer.getproc())

      var tactx = newTAContext()
      var asmctx = newAsmContext(gImage.buffer)
      discard tactx.convertFExpr(f)
      tactx = tactx.optimize()
      let liveness = tactx.analyzeLiveness()
      var (x86ctx, x86plat) = tactx.x86Tiling().freqRegalloc(liveness, plat)
      plat = asmctx.generateX86(x86ctx, x86plat)

      if call:
        ffiCall(callp)

      flushFile(stdout)
      s = ""
    except MoreToken:
      continue
    except LexerError:
      echo getCurrentExceptionMsg()
    except ParseError:
      echo getCurrentExceptionMsg()
    except IOError:
      break

proc main() =
  let args = docopt(doc, version = "Flori 0.2.0")
  if args["repl"]:
    startREPL()

main()
