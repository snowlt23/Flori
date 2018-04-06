
import docopt
import os
import strutils
import compile

let doc = """
Flori programming language.

Usage:
  flori c <name> [-o=<outname>] [--opt=<level>] [--cc=<cc>] [--bench] [--ccoptions=<options>]
  flori js <name> [-o=<outname>] [--bench] [--ccoptions=<options>]

Options:
  -h --help      Show this screen.
  --version      Show version.
  -o=<outname>   Specialize output filename.
  --opt=<level>  Optimize level.
  --cc=<cc>      Select C Compiler.
    gcc  GNU C Compiler (default)
    tcc  Tiny C Compiler
  --bench        Show benchmark information.
  --ccoptions=<options>  CC Options.
"""

proc main() =
  let args = docopt(doc, version = "Flori 0.1.0")
  if args["c"]:
    compileFloriC(ccoptions(args))
  elif args["js"]:
    compileFloriJS(ccoptions(args))

main()
