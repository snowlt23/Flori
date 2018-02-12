
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
  let cout = execProcess("compiler/flori c --cc=tcc -obin/$# $#" % [filename.splitFile().name.exe, filename])
  if cout != "":
    echo cout
  let name = splitFile(filename).name
  try:
    check execProcess("bin" / name) == expectoutput
  except EOS:
    styledEcho(fgRed, "[Error] ", resetStyle, getCurrentExceptionMsg())
    fail

proc setup*() =
  stdout.write("remove all executable...")
  for f in walkPattern("bin/*.exe"):
    removeFile(f)
  echo " [done]"
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
    checkio "examples/destructor.flori", "1\n2\nMyInt destroyed!\n"
