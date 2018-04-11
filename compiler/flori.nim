
import docopt
import os
import strutils
import compile

let doc = """
Flori programming language.

Usage:
  flori c <name> [-o=<outname>] [--opt=<level>] [--cc=<cc>] [--bench] [--ccoptions=<options>] [--src-comment]
  flori js <name> [-o=<outname>] [--bench] [--ccoptions=<options>] [--sourcemap]

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
  --sourcemap    Generate sourcemap in JS backend.
  --src-comment  Add src comment to generated file.
"""

proc main() =
  let args = docopt(doc, version = "Flori 0.1.0")
  if args["c"]:
    compileFloriC(ccoptions(args))
  elif args["js"]:
    let usesourcemap = bool(args["--sourcemap"])
    compileFloriJS(ccoptions(args), usesourcemap)

main()
