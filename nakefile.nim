
import nake
import os

proc exe*(s: string): string =
  if defined(windows):
    s & ".exe"
  else:
    s

task "debug64", "build 64bit compiler":
  if not existsDir("bin"):
    createDir("bin")
  discard execShellCmd("nim c compiler/flori.nim")
  copyFile("compiler/flori".exe, "bin/flori".exe)
  
task "build64", "build 64bit compiler":
  if not existsDir("bin"):
    createDir("bin")
  discard execShellCmd("nim c -d:release compiler/flori.nim")
  copyFile("compiler/flori".exe, "bin/flori".exe)
task "build32", "build 32bit compiler":
  if not existsDir("bin"):
    createDir("bin")
  discard execShellCmd("nim c -d:release --cpu:i386 --passC:\"-m32\" --passL:\"-m32\" compiler/flori.nim")
  copyFile("compiler/flori".exe, "bin/flori32".exe)

task "build", "build compiler":
  runTask "build64"
  runTask "build32"
  
task "test", "":
  discard execShellCmd("nim c -r tests/tester.nim")
