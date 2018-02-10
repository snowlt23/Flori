
import os
import osproc
import unittest
import strutils

proc checkio*(filename: string, expectoutput: string) =
  discard execProcess("compiler/flori c --cc=tcc $#" % filename)
  let name = splitFile(filename).name
  check execProcess(name) == expectoutput

suite "example":
  echo "building flori compiler..."
  discard execProcess("nim c compiler/flori.nim")
  echo "done."
  test "fib":
    checkio "examples/fib.flori", "39088169\n"
  test "tak":
    checkio "examples/tak.flori", "12\n"
  test "destructor":
    checkio "examples/destructor.flori", "1\n2\nMyInt destroyed!\nMyInt destroyed!\n"
