
import os
import osproc
import unittest

proc checkio*(filename: string, expectoutput: string) =
  discard execProcess("compiler/flori c " & filename)
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
