
import fexpr, parser, types
import scope, semantic
import ccodegen
import macroffi

import os
import strutils, sequtils
import dynlib
import tables

export ccodegen.codegenMangling

const cachedir* = "floricache"

proc compileWithGCC*(pass: CCodegenContext, dir: string, options: string, files = "") =
  discard execShellCmd "gcc $# $# $#" % [options, pass.cfilenames(dir).join(" "), files]
proc compileWithTCC*(pass: CCodegenContext, dir: string, options: string, files = "") =
  discard execShellCmd "tcc $# $# $#" % [options, pass.cfilenames(dir).join(" "), files]

proc exe*(s: string): string =
  when defined(windows):
    s & ".exe"
  else:
    s
proc dll*(s: string): string =
  when defined(windows):
    s & ".dll"
  else:
    s & ".so"

const macrolib* = cachedir / "flori_macrolib".dll

proc compileMacroLibrary*(semctx: SemanticContext, scope: Scope) =
  semctx.modules[name("current_module")] = scope
  defer: semctx.modules.del(name("current_module"))
  let genctx = newCCodegenContext(macrogen = true)
  genctx.codegen(semctx)
  genctx.writeModules(cachedir)
  genctx.compileWithTCC(cachedir, "-shared -rdynamic -o$# -Iffi/ -Lbin/ -lflori" % [macrolib])

proc setupFFI*(handle: LibHandle) =
  template ffi(name, prc) =
    cast[ptr pointer](handle.checkedSymAddr(name))[] = prc
  ffi "flori_new_fident", ffiNewFIdent
  ffi "flori_new_fblock", ffiNewFBlock
  ffi "flori_parse_fexpr", ffiParseFExpr
  ffi "flori_print_fexpr", ffiPrintFExpr

proc reloadMacroLibrary*(semctx: SemanticContext, scope: Scope) =
  if semctx.macrolib != nil:
    unloadLib(semctx.macrolib)
  semctx.compileMacroLibrary(scope)
  semctx.macrolib = loadLib(macrolib)
  if semctx.macrolib.isNil:
    raise newException(IOError, "couldn't load flori macro library.")
  semctx.macrolib.setupFFI()
  for mp in semctx.macroprocs:
    mp.call = cast[proc (f: FExpr): FExpr {.cdecl.}](semctx.macrolib.checkedSymAddr(mp.importname))
