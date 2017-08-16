
import sast
import sparser
import semantic
import semanticeval
import ccodegen
import compile
import docopt
import os
import strutils

let doc = """
Flori language compiler.

Usage:
  flori <filename> [-o=<outname>] [--opt-level=<level>]
  flori (-h | --help)
  flori --version
Options:
  -h --help    Show this screen.
  --version    Show version.
  -o=<outname> Output filename.
  --opt-level=<level> Spec optimization level.
"""

proc compileFlori*(filename: string, outname: string, optlevel: string) =
  var semcontext = newSemanticContext()
  semcontext.includepaths.add("./")
  semcontext.includepaths.add(getAppDir() / "core")
  semcontext.includepaths.add(filename.splitFile().dir)
  semcontext.evalTopfile(filename)
  var cgencontext = newCCodegenContext()
  cgencontext.genContext(semcontext)
  cgencontext.compile(outname, optlevel)

proc main() =
  let args = docopt(doc, version = "Flori 0.1.0")
  let filename = $args["<filename>"]
  let outputname = if args["-o"]:
                     $args["-o"]
                   else:
                     filename.splitFile().name
  let optlevel = if args["--opt-level"]:
                   $args["--opt-level"]
                 else:
                   "0"
  if not existsFile(filename):
    raise newException(OSError, "couldn't find $#" % filename)
  compileFlori(filename, outputname, optlevel)
main()
