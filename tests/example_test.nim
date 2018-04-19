
import os
import osproc
import unittest
import strutils
import terminal

proc checkio*(filename: string, expectoutput: string) =
  let cout = execProcess("compiler/flori c -otests/bin/$# $#" % [filename.splitFile().name, filename])
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
  test "formacro":
    checkio "examples/formacro.flori", "12345678910\n"
  test "destructor":
    checkio "examples/destructor.flori", "MyInt(1, 2) destroyed!\n3\n4\nMyInt(3, 4) destroyed!\n"
  test "string":
    checkio "examples/string.flori", "Hello Yukari! and Maki!\nHello Yukari! and Maki! and Akari!\n9\n9\n"
  test "array":
    checkio "examples/array.flori", ""
  test "const":
    checkio "examples/const.flori", "9\n"
  test "syntax macro":
    checkio "examples/syntax_macro.flori", "FList!\nFArray!\nFList!\n"
  test "float literal":
    checkio "examples/floatlit.flori", "2.000000\n"
  test "field track":
    checkio "examples/field_track.flori", "start!\nMyFieldType(1, 2) destroyed!\nend!\nMyFieldType(5, 6) destroyed!\nMyFieldType(3, 4) destroyed!\nMyType destroyed!\n"
  test "ref check":
    checkio "examples/ref_check.flori", "start!\nMyFieldType(1, 2) destroyed!\nend!\nMyFieldType(5, 6) destroyed!\nMyFieldType(3, 4) destroyed!\nMyType destroyed!\n"
  test "template expand":
    checkio "examples/template.flori", "9\n"
  test "dynamic type: unique":
    checkio "examples/dynunique.flori", "start!\nend!\nMyFile destroyed!\nDynVec destroyed!\n"
  test "dynamic type: share":
    checkio "examples/dynshare.flori", "start!\nMyData destroyed!\nMyData destroyed!\nDynVec destroyed!\nend!\nMyData destroyed!\n"
  test "vec example":
    checkio "examples/vec_example.flori", "vec[MyInt:1, MyInt:2, MyInt:3]\nd:MyInt:1\nd:MyInt:2\nd:MyInt:3\n"
