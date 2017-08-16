
import sast
import sparser
import semantic
import ccodegen
import semanticeval

import os
import tables
import strutils
import dynlib

let cachedir = "floricache"

proc writeToFiles*(context: CCodegenContext) =
  if not existsDir(cachedir):
    createDir(cachedir)
  for sym, module in context.modules:
    writeFile(cachedir/sym & ".c", module.getSrc())
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

#
# Compile Time
#

let ctsharedname = cachedir/"compiletime_tmp"

proc compileCompileTime*(context: CCodegenContext) =
  context.writeToFiles()
  discard execShellCmd "gcc -shared -o $# -O0 $# $# $#" % [ctsharedname, cachedir/"main.c", context.getFilenames.join(" "), cachedir/"macrolib.a"]
proc loadCompileTime*(module: Module): LibHandle =
  var cgencontext = newCCodegenContext()
  discard cgencontext.genModule(module.name, module, compiletime = true)
  cgencontext.compileCompileTime()
  return loadLib(ctsharedname)
