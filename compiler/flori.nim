
import docopt
import os
import strutils
import compile

let doc = """
Flori programming language.

Usage:
  flori c <name> [-o=<outname>] [--opt=<level>] [--cc=<cc>] [--bench] [--ccoptions=<options>] [--moptions=<options>] [--src-comment] [-d=<defines>...] [--imagedump=<file>]
  flori js <name> [-o=<outname>] [--bench] [--sourcemap]

Options:
  -h --help      Show this screen.
  --version      Show version.
  -o=<outname>   Specialize output filename.
  --opt=<level>  Optimize level.
  -d=<defines>   Define for compiletime switch.
  --cc=<cc>      Select C Compiler.
    gcc  GNU C Compiler (default)
    tcc  Tiny C Compiler
  --bench        Show benchmark information.
  --ccoptions=<options>  CC Options.
  --moptions=<options>  CC Options for macro evaluate.
  --sourcemap    Generate sourcemap in JS backend.
  --src-comment  Add src comment to generated file.
  --imagedump=<file>  Dump compiler image to file.
  --loadimage    Load compiler image fom file.
"""

proc main() =
  let args = docopt(doc, version = "Flori 0.1.0")
  if args["c"]:
    compileFloriC(ccoptions(args))
  elif args["js"]:
    quit "jscodegen is under maintained"
    # let usesourcemap = bool(args["--sourcemap"])
    # compileFloriJS(ccoptions(args), usesourcemap)

main()
