
import sast
import sparser
import semantic
import ccodegen
import compile
import docopt
import os
import strutils

let doc = """
Flori language compiler.

Usage:
  flori <filename> [-o=<outname>]
  flori (-h | --help)
  flori --version
Options:
  -h --help    Show this screen.
  --version    Show version.
  -o=<outname> Output filename.
"""

proc main() =
  let args = docopt(doc, version = "Flori 0.1.0")
  let filename = $args["<filename>"]
  let outputname = if args["-o"]:
                    $args["-o"]
                  else:
                      filename.splitFile().name
  if not existsFile(filename):
    raise newException(OSError, "couldn't find $#" % filename)
  compileFlori(filename, outputname)
main()
