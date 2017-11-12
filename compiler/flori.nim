
import docopt
import os
import strutils
import compile

let doc = """
Flori programming language.

Usage:
  flori c <name> [-o=<outname>] [--opt=<level>]

Options:
  -h --help      Show this screen.
  --version      Sho version
  -o=<outname>   Specialize output filename.
  --opt=<level>  Optimize level.
"""

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
    if optlevel > 3:
      quit "optlevel should be lesseq than 3."
    compileFlori(filepath, outname, optlevel)

main()
