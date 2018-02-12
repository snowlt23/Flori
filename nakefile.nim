
import nake
import os

proc exe*(s: string): string =
  if defined(windows):
    s & ".exe"
  else:
    s

task "build", "build compiler and generate ffi c files":
  if not existsDir("bin"):
    createDir("bin")
  discard execShellCmd("nim c compiler/flori.nim")
  copyFile("compiler/flori".exe, "bin/flori".exe)

task "test", "":
  discard execShellCmd("nim c -r tests/tester.nim")
