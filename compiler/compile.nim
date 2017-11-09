
import ast, parser
import sempass, sempass_default, sempass_create, sempass_ccodegen

import os
import strutils

const cachedir* = "floricache"

proc compileWithGCC*(pass: CCodegenPass, dir: string, options: string) =
  discard execShellCmd "gcc $# $#" % [options, pass.filenames(dir).join(" ")]

proc genGCCOptions*(output: string, optlevel: int): string =
  "-o$# -O$#" % [output, $optlevel]

proc compileFlori*(filepath: string, output: string, optlevel: int) =
  let passctx = newDefaultSemPassContext()
  let genpass = newCCodegenPass()
  passctx.register(genpass)
  let sexprs = parseToplevel(filepath, readFile(filepath))
  let (filedir, filename, _) = filepath.splitFile()
  passctx.createModuleFromSExpr(filename, sexprs)
  passctx.execute()
  genpass.write(cachedir)
  genpass.compileWithGCC(cachedir, genGCCOptions(output, optlevel))
