
import docopt
import os
import strutils
import compile

let doc = """
Flori programming language.

Usage:
  flori c <name> [-o=<outname>] [--opt=<level>] [--cc=<cc>]

Options:
  -h --help      Show this screen.
  --version      Show version.
  -o=<outname>   Specialize output filename.
  --opt=<level>  Optimize level.
  --cc=<cc>      Select C Compiler.
"""

proc parseCC*(val: Value): CCKind =
  if val:
    case $val
    of "gcc":
      return ccGCC
    of "tcc":
      return ccTCC
    else:
      quit "unsupported C Compiler: $#" % $val
  else:
    return ccGCC

proc main() =
  let args = docopt(doc, version = "Flori 0.1.0")
  if args["c"]:
    let filepath = $args["<name>"]
    let outname = if args["-o"]:
                    $args["-o"]
                  else:
                    filepath.splitFile.name
    let optlevel = if args["--opt"]:
                     parseInt($args["--opt"])
                   else:
                     0
    if optlevel < 0 or 3 < optlevel:
      quit "optlevel should be 0 <= level <= 3."
    let cc = parseCC(args["--cc"])
    compileFlori(ccoptions(cc, filepath, outname, optlevel))

main()
