
import sast
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
    writeFile(cachedir/sym & ".c", module.src)
  writeFile(cachedir/"main.c", context.getMainSrc())

proc getFilenames*(context: CCodegenContext): seq[string] =
  result = @[]
  for sym in context.modules.keys:
    result.add(cachedir/sym & ".c")

proc execCompileCmd*(context: CCodegenContext, outname: string) =
  discard execShellCmd "gcc -o $# $# $#" % [outname, cachedir/"main.c", context.getFilenames.join(" ")]

proc compile*(context: CCodegenContext, outname: string) =
  context.writeToFiles()
  context.execCompileCmd(outname)
