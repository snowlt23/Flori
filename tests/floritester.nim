
import os
import osproc
import unittest
import strutils
import terminal

import compiler/fexpr_core
import basictester

proc checkOutput*(filename: string, expectoutput: string) =
  let cout = execProcess("compiler/flori c -otests/bin/$# $#" % [filename.splitFile().name, filename])
  if cout != "":
    echo cout
  let name = splitFile(filename).name
  try:
    let output = execProcess("tests/bin" / name)
    if output == expectoutput:
      styledEcho(fgGreen, "[Success] ", resetStyle, filename)
    else:
      styledEcho(fgRed, "[Error] ", resetStyle, "$#: expect \"$#\", but got \"$#\"" % [filename, expectoutput, output])
  except OSError:
    styledEcho(fgRed, "[Error] ", resetStyle, getCurrentExceptionMsg())
proc parseExpectOutput*(filename: string): string =
  let tops = parseToplevel(filename, readFile(filename))
  for t in tops:
    if t.kind == fexprSeq and t.len == 2 and $t[0] == "expect" and t[1].kind == fexprList and t[1].len == 1 and t[1][0].kind == fexprStrLit:
      return ($t[1][0].strval).replace("\\n", "\n")
  return nil
proc execFloriTest*(filename: string) =
  if not existsFile(filename):
    styledEcho(fgRed, "[FileNotFound] ", resetStyle, filename)
    return
  let expectoutput = parseExpectOutput(filename)
  if expectoutput == nil:
    styledEcho(fgRed, "[ExpectNotFound] ", resetStyle, filename)
    return
  checkOutput(filename, expectoutput)
    
proc setupFloriCompiler*() =
  if existsDir("tests/bin"):
    removeDir("tests/bin")
  createDir("tests/bin")
  stdout.write("building flori compiler...")
  discard execProcess("nim c -d:release compiler/flori.nim")
  echo " [done]"

proc main() =
  setupFloriCompiler()
  for florifile in walkDirRec("tests/floritests"):
    initLinmem(defaultLinmemSpace)
    execFloriTest(florifile)
  
main()
