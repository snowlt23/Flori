
import parser, linmem, image, fexpr, scope
import passdef, internalpass, elimpass
# import ccodegen, jscodegen
import ccodegen
import compileutils

import os
import strutils
import times
import docopt

type
  CCKind* = enum
    ccTCC
    ccGCC
  CCOptions* = object
    cc*: CCKind
    filepath*: string
    output*: string
    optlevel*: int
    bench*: bool
    ccoptions*: string
    moptions*: string
    srccomment*: bool
    defines*: seq[string]

template bench*(name: string, body: untyped) =
  if options.bench:
    let s = epochTime()
    body
    echo name, " Elapsed: ", epochTime() - s
  else:
    body

proc genGCCOptions*(options: CCOptions): string =
  "-o$# -O$# -I$# $#" % [options.output, $options.optlevel, getAppDir() / "../ffi", options.ccoptions]
proc genTCCOptions*(options: CCOptions): string =
  "-o$# -I$# $#" % [options.output.exe, getAppDir() / "../ffi", options.ccoptions]

proc ccoptions*(args: Table[string, Value]): CCOptions =
  result.cc = if args["--cc"]:
                if $args["--cc"] == "gcc":
                  ccGCC
                elif $args["--cc"] == "tcc":
                  ccTCC
                else:
                  ccGCC
              else:
                ccGCC
  result.filepath = $args["<name>"]
  result.output = if args["-o"]:
                     $args["-o"]
                   else:
                     result.filepath.splitFile.name
  result.optlevel = if args["--opt"]:
                      parseInt($args["--opt"])
                    else:
                      0
  if result.optlevel < 0 or 3 < result.optlevel:
    quit "optlevel should be 0 <= level <= 3"
  result.bench = bool(args["--bench"])
  result.ccoptions = if args["--ccoptions"]:
                       $args["--ccoptions"]
                     else:
                       ""
  result.moptions = if args["--moptions"]:
                       $args["--moptions"]
                     else:
                       ""
  result.srccomment = bool(args["--src-comment"])
  result.defines = @(args["-d"])

proc compileFloriC*(options: CCOptions) =
  discard newSemContext(options.moptions, options.defines)
  let genctx = newCCodegenContext()
  bench "eval":
    if not existsFile(options.filepath):
      quit "flori: Please exists flori file."
    discard gCtx.semFile(options.filepath)
    for top in gCtx.globaltoplevels.mitems:
      top.metadata.scope.resetElim(top)
    for top in gCtx.globaltoplevels.mitems:
      top.metadata.scope.processElimPass(top)
  bench "codegen":
    if not existsDir(cachedir):
      createDir(cachedir)
    let src = genctx.codegenSingle(gCtx).replace("#include \"floriffi.h\"\n")
    if options.srccomment:
      let origsrc = readFile(options.filepath)
      writeFile(cachedir / "flori_compiled.c", "/*\n" & origsrc & "\n*/\n" & src)
    else:
      writeFile(cachedir / "flori_compiled.c", src)
  bench "cc":
    case options.cc
    of ccGCC:
      genctx.compileWithGCC(cachedir, genGCCOptions(options))
    of ccTCC:
      genctx.compileWithTCC(cachedir, genTCCOptions(options))
      
# proc compileFloriJS*(options: CCOptions, sourcemap: bool) =
#   let semctx = newSemanticContext(options.moptions)
#   bench "eval":
#     discard semctx.semFile(options.filepath)
#     for top in semctx.globaltoplevels.mitems:
#       top.internalScope.resetElim(top)
#       top.internalScope.processElimPass(top)
#   let genctx = newJSCodegenContext(semctx)
#   bench "codegen":
#     if not existsDir(cachedir):
#       createDir(cachedir)
#     let src = genctx.codegenSingle(semctx).replace("#include \"floriffi.h\"\n")
#     if sourcemap:
#       let mapprefix = "\n//# sourceMappingURL=data:application/json;base64,"
#       writeFile(cachedir / "flori_compiled.js", src & mapprefix & genctx.generateSourcemap(options.output & ".js"))
#     else:
#       writeFile(cachedir / "flori_compiled.js", src)
#   copyFile(cachedir / "flori_compiled.js", options.output & ".js")
