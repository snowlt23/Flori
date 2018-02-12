
import fexpr, parser
import scope, semantic, internal
import ccodegen
import compileutils

import os
import strutils

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

import times
template bench*(name: string, body: untyped) =
  if options.bench:
    let s = epochTime()
    body
    echo name, " Elapsed: ", epochTime() - s
  else:
    body

proc genGCCOptions*(options: CCOptions): string =
  "-o$# -O$# -Iffi" % [options.output, $options.optlevel]
proc genTCCOptions*(options: CCOptions): string =
  "-o$# -Iffi" % [options.output]

proc ccoptions*(cc: CCKind, filepath: string, output: string, optlevel: int, bench: bool): CCOptions =
  result.cc = cc
  result.filepath = filepath
  result.output = output
  result.optlevel = optlevel
  result.bench = bench

proc compileFlori*(options: CCOptions) =
  let semctx = newSemanticContext()
  let genctx = newCCodegenContext()
  bench "eval":
    semctx.evalFile(options.filepath)
  bench "codegen":
    genctx.codegen(semctx)
  bench "write":
    genctx.writeModules(cachedir)
  bench "cc":
    case options.cc
    of ccGCC:
      genctx.compileWithGCC(cachedir, genGCCOptions(options))
    of ccTCC:
      genctx.compileWithTCC(cachedir, genTCCOptions(options))
