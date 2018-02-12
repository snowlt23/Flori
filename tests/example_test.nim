
import os
import osproc
import unittest
import strutils
import terminal

proc exe*(s: string): string =
  when defined(windows):
    s & ".exe"
  else:
    s

proc checkio*(filename: string, expectoutput: string) =
  let cout = execProcess("compiler/flori c --cc=tcc -otests/bin/$# $#" % [filename.splitFile().name.exe, filename])
  if cout != "":
    echo cout
  let name = splitFile(filename).name
  try:
    check execProcess("tests/bin" / name) == expectoutput
  except OSError:
    styledEcho(fgRed, "[Error] ", resetStyle, getCurrentExceptionMsg())
    fail

proc setup*() =
  if existsDir("tests/bin"):
    removeDir("tests/bin")
  createDir("tests/bin")
  stdout.write("building flori compiler...")
  discard execProcess("nim c compiler/flori.nim")
  echo " [done]"
  
suite "example":
  setup()
  test "fib":
    checkio "examples/fib.flori", "39088169\n"
  test "tak":
    checkio "examples/tak.flori", "12\n"
  test "destructor":
    checkio "examples/destructor.flori", "1\n2\nMyInt destroyed!\nMyInt destroyed!\n"
