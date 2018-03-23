
import nake
import os

proc exe*(s: string): string =
  if defined(windows):
    s & ".exe"
  else:
    s

task "build", "build compiler":
  if not existsDir("bin"):
    createDir("bin")
  discard execShellCmd("nim c compiler/flori.nim")
  copyFile("compiler/flori".exe, "bin/flori".exe)
  discard execShellCmd("nim c --cpu:i386 --passC:\"-m32\" --passL:\"-m32\" compiler/flori.nim")
  copyFile("compiler/flori".exe, "bin/flori32".exe)

task "test", "":
  discard execShellCmd("nim c -r tests/tester.nim")
