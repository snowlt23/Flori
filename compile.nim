
import sast
import sparser
import semantic
import ccodegen

import os
import tables
import strutils

let cachedir = "floricache"

proc writeToFiles*(context: CCodegenContext) =
  if not existsDir(cachedir):
    createDir(cachedir)
  for sym, module in context.modules:
    writeFile(cachedir/sym & ".c", module.toplevel & "\n" & module.src)
  writeFile(cachedir/"main.c", context.getMainSrc())

proc getFilenames*(context: CCodegenContext): seq[string] =
  result = @[]
  for sym in context.modules.keys:
    result.add(cachedir/sym & ".c")

proc execCompileCmd*(context: CCodegenContext, outname: string, optlevel: string) =
  discard execShellCmd "gcc -o $# -O$# $# $#" % [outname, optlevel, cachedir/"main.c", context.getFilenames.join(" ")]

proc compile*(context: CCodegenContext, outname: string, optlevel: string) =
  context.writeToFiles()
  context.execCompileCmd(outname, optlevel)

proc compileFlori*(filename: string, outname: string, optlevel: string) =
  var semcontext = newSemanticContext()
  semcontext.includepaths.add("./")
  semcontext.includepaths.add(getAppDir() / "core")
  semcontext.includepaths.add(filename.splitFile().dir)
  semcontext.evalTopfile(filename)
  var cgencontext = newCCodegenContext()
  cgencontext.genContext(semcontext)
  cgencontext.compile(outname, optlevel)
