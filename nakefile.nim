
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

task "buildrepl", "build 64bit repl":
  if not existsDir("bin"):
    createDir("bin")
  discard execShellCmd("nim c -d:release -d:replError compiler/florirepl.nim")
  copyFile("compiler/florirepl".exe, "bin/florirepl".exe)
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

task "core-img", "build core img":
  removeFile("core.fimg")
  discard execShellCmd("flori c --imagedump core.fimg core/root.flori")
  removeFile("root".exe)

task "build", "build compiler":
  #runTask "buildrepl"
  #runTask "build32"
  runTask "build64"
  runTask "core-img"

task "release", "packaging build binaries to zip":
  runTask "build"
  discard execShellCmd("7z a flori-v.zip bin core std ffi examples")
  
task "test", "":
  discard execShellCmd("nim c -r tests/floritester.nim")
