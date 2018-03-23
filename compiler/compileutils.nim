
import parser, types, fexpr, scope
import ccodegen
import macroffi

import os
import strutils, sequtils
import dynlib
import tables

export ccodegen.codegenMangling

const cachedir* = "floricache"

proc compileWithGCC*(pass: CCodegenContext, dir: string, options: string, files = "") =
  discard execShellCmd "gcc $# $# $#" % [pass.cfilenames(dir).join(" "), files, options]
proc compileWithTCC*(pass: CCodegenContext, dir: string, options: string, files = "") =
  discard execShellCmd "tcc $# $# $#" % [pass.cfilenames(dir).join(" "), files, options]

proc dll*(s: string): string =
  when defined(windows):
    s & ".dll"
  else:
    s & ".so"

const macrolib* = cachedir / "flori_macrolib".dll

proc compileMacroLibrary*(semctx: SemanticContext, scope: Scope) =
  # semctx.modules[name("current_module")] = scope
  # defer: semctx.modules.del(name("current_module"))
  let genctx = newCCodegenContext(macrogen = true)
  genctx.codegen(semctx)
  genctx.writeModules(cachedir)
  genctx.compileWithTCC(cachedir, "-shared -rdynamic -o$# -I$# $#" % [macrolib, getAppDir() / ".." / "ffi", semctx.ccoptions])

proc setupFFI*(handle: LibHandle) =
  template ffi(name, prc) =
    cast[ptr pointer](handle.checkedSymAddr(name))[] = prc
  ffi "flori_new_fident", ffiNewFIdent
  ffi "flori_new_fseq", ffiNewFSeq
  ffi "flori_new_farray", ffiNewFArray
  ffi "flori_new_flist", ffiNewFList
  ffi "flori_new_fblock", ffiNewFBlock
  ffi "flori_parse_fexpr", ffiParseFExpr
  ffi "flori_print_fexpr", ffiPrintFExpr
  ffi "flori_length", ffiLength
  ffi "flori_push", ffiPush
  ffi "flori_kind", ffiKind
  ffi "flori_expect", ffiExpect
  ffi "flori_error", ffiError
  ffi "flori_access", ffiAccess
  ffi "flori_set", ffiSet
  ffi "flori_to_cs", ffiToCS
  ffi "flori_gensym", ffiGensym

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
  let florimain = cast[proc () {.cdecl.}](semctx.macrolib.checkedSymAddr("flori_main"))
  florimain()
