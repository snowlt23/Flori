
import docopt
import os
import strutils
import os, osproc

import fcore, passmacro, internalpass, internalffi
import codegen.tacode, codegen.tagen, codegen.taopt, codegen.liveness, codegen.address
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

addr => $typed(a)
  internalop("addr")

printf_int => $cffi("printf") $cdecl $dll("msvcrt") $typed(cstring, int) $returned(void)
printf_ptr => $cffi("printf") $cdecl $dll("msvcrt") $typed(cstring, pointer) $returned(void)

println => printf_int("%d\n", x)
print_int => printf_int("%d", x)
print_str => $cffi("printf") $cdecl $dll("msvcrt") $typed(cstring) $returned(void)
print_ptr => printf_ptr("%p", p)
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

proc evalFlori*(scope: FScope, f: var FExpr) =
  scope.rootPass(f)

  for f in gCtx.notevals:
    var tafn = emptyTAFn()
    var asmctx = newAsmContext(gImage.buffer)
    discard tafn.convertFExpr(f)
    tafn = tafn.optimize()
    let liveness = tafn.analyzeLiveness()
    let addrtable = tafn.analyzeAddress()
    var (x86ctx, x86plat) = tafn.x86Tiling().freqRegalloc(liveness, addrtable, plat)
    plat = asmctx.generateX86(x86ctx, x86plat)

  var call = false
  if not (f.kind == fexprInfix and $f.call == "=>"):
    if $f.gettype == "void":
      f = quoteFExpr(f.span, "main => `embed", [f])
    else:
      f = quoteFExpr(f.span, "main => println(`embed)", [f])
    scope.rootPass(f)
    call = true
  let callp = toProc[pointer](gImage.buffer.getproc())

  var tafn = emptyTAFn()
  var asmctx = newAsmContext(gImage.buffer)
  discard tafn.convertFExpr(f)
  tafn = tafn.optimize()
  let liveness = tafn.analyzeLiveness()
  let addrtable = tafn.analyzeAddress()
  var (x86ctx, x86plat) = tafn.x86Tiling().freqRegalloc(liveness, addrtable, plat)
  plat = asmctx.generateX86(x86ctx, x86plat)

  if call:
    ffiCall(callp)

proc evalFlori(scope: FScope, filename: string, src: string) =
  var f = parseFExpr(filename, src)
  if f.kind == fexprBlock and f.sons.len == 0:
    return
  scope.evalFlori(f)

proc startREPL() =
  initPlatform()
  gCtx.notevals = @[]
  let scope = newFScope("<repl>", "<repl>")
  scope.importFScope(internalScope.obj.name, internalScope)
  var preludef = parseToplevel("<repl>", prelude)
  for f in preludef.mitems:
    scope.evalFlori(f)

  var incont = false
  var s = ""
  while true:
    if incont:
      stdout.write("- ")
    else:
      stdout.write("> ")
    flushFile(stdout)
    var line = ""
    var hasline = stdin.readLine(line)
    if not hasline:
      break
    s &= line & "\n"
    if line.startsWith("  "):
      continue
    try:
      scope.evalFlori("<repl>", s)
      flushFile(stdout)
      s = ""
      incont = false
    except MoreToken:
      incont = true
      continue
    except LexerError:
      s = ""
      incont = false
    except ParseError:
      s = ""
      incont = false
    except FExprError:
      s = ""
      incont = false

proc main() =
  let args = docopt(doc, version = "Flori 0.2.0")
  if args["repl"]:
    startREPL()

main()
