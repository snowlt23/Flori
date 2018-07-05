
import nake
import os

proc exe*(s: string): string =
  if defined(windows):
    s & ".exe"
  else:
    s

task "debug-build", "build compiler":
  if not existsDir("bin"):
    createDir("bin")
  discard execShellCmd("nim c --cpu:i386 compiler/flori")
  copyFile("compiler/flori".exe, "bin/flori".exe)
task "build", "build compiler":
  if not existsDir("bin"):
    createDir("bin")
  discard execShellCmd("nim c -d:release --cpu:i386 compiler/flori")
  copyFile("compiler/flori".exe, "bin/flori".exe)

task "release", "packaging build binaries to zip":
  runTask "build"
  discard execShellCmd("7z a flori-v.zip bin core std ffi examples")

task "test", "":
  discard execShellCmd("nim c -r --cpu:i386 tests/basictester.nim")
