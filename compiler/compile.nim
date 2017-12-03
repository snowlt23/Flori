
import fexpr, parser
import scope, semantic, internal
import ccodegen

import os
import strutils

const cachedir* = "floricache"

proc compileWithGCC*(pass: CCodegenContext, dir: string, options: string) =
  discard execShellCmd "gcc $# $#" % [options, pass.cfilenames(dir).join(" ")]

proc genGCCOptions*(output: string, optlevel: int): string =
  "-o$# -O$#" % [output, $optlevel]

proc compileFlori*(filepath: string, output: string, optlevel: int) =
  let semctx = newSemanticContext()
  let genctx = newCCodegenContext()
  semctx.evalFile(filepath)
  genctx.codegen(semctx)
  genctx.writeModules(cachedir)
  genctx.compileWithGCC(cachedir, genGCCOptions(output, optlevel))
