
import parser, types, fexpr, scope, metadata
import passmacro, passdef, internalpass
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
  "-o$# -I$# $#" % [options.output, getAppDir() / "../ffi", options.ccoptions]

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

proc compileFlori*(options: CCOptions) =
  let semctx = newSemanticContext(options.ccoptions)
  let genctx = newCCodegenContext()
  bench "eval":
    discard semctx.semFile(processSemPass, options.filepath)
  bench "codegen":
    if not existsDir(cachedir):
      createDir(cachedir)
    let src = readFile(getAppDir() / "../ffi/floriffi.h") & "\n" & genctx.codegenSingle(semctx).replace("#include \"floriffi.h\"\n")
    writeFile(cachedir / "flori_compiled.c", src)
  bench "cc":
    case options.cc
    of ccGCC:
      genctx.compileWithGCC(cachedir, genGCCOptions(options))
    of ccTCC:
      genctx.compileWithTCC(cachedir, genTCCOptions(options))
