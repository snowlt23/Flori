
import parser, types, fexpr, scope, metadata
import ccodegen, effectpass, elimpass
import macroffi

import os
import strutils, sequtils
import dynlib
import tables

export ccodegen.codegenMangling

const cachedir* = "floricache"

proc compileWithGCC*(pass: CCodegenContext, dir: string, options: string, files = "") =
  discard execShellCmd "gcc $#/flori_compiled.c $# $#" % [dir, files, options]
proc compileWithTCC*(pass: CCodegenContext, dir: string, options: string, files = "") =
  discard execShellCmd "tcc $#/flori_compiled.c $# $#" % [dir, files, options]

proc dll*(s: string): string =
  when defined(windows):
    s & ".dll"
  else:
    s & ".so"
proc exe*(s: string): string =
  when defined(windows):
    s & ".exe"
  else:
    s

const macrolib* = cachedir / "flori_macrolib".dll

proc compileMacroLibrary*(semctx: SemanticContext, scope: Scope) =
  # for top in semctx.globaltoplevels.mitems:
  #   top.internalScope.resetElim(top)
  for top in semctx.globaltoplevels.mitems:
    top.internalScope.processElimPass(top)
  let genctx = newCCodegenContext(macrogen = true)
  if not existsDir(cachedir):
    createDir(cachedir)
  let src = genctx.codegenSingle(semctx)
  writeFile(cachedir / "flori_compiled.c", src)
  genctx.compileWithTCC(cachedir, "-shared -rdynamic -o$# -I$# $#" % [macrolib, getAppDir() / ".." / "ffi", semctx.moptions])

proc setupFFI*(handle: LibHandle) =
  template ffi(name, prc) =
    cast[ptr pointer](handle.checkedSymAddr(name))[] = prc
  ffi "flori_new_fident", ffiNewFIdent
  ffi "flori_new_fseq", ffiNewFSeq
  ffi "flori_new_farray", ffiNewFArray
  ffi "flori_new_flist", ffiNewFList
  ffi "flori_new_fblock", ffiNewFBlock
  ffi "flori_new_fintlit", ffiNewFIntLit
  ffi "flori_new_fstrlit", ffiNewFStrLit
  ffi "flori_parse_fexpr", ffiParseFExpr
  ffi "flori_print_fexpr", ffiPrintFExpr
  ffi "flori_quoted", ffiQuoted
  ffi "flori_length", ffiLength
  ffi "flori_push", ffiPush
  ffi "flori_kind", ffiKind
  ffi "flori_expect", ffiExpect
  ffi "flori_error", ffiError
  ffi "flori_access", ffiAccess
  ffi "flori_set", ffiSet
  ffi "flori_to_cs", ffiToCS
  ffi "flori_strval", ffiStrval
  ffi "flori_gensym", ffiGensym
  ffi "flori_get_type", ffiGetType
  ffi "flori_scopelevel", ffiScopeLevel
  ffi "flori_debug_marking", ffiDebugMarking

proc reloadMacroLibrary*(semctx: SemanticContext, scope: Scope) =
  if semctx.macrolib != nil:
    let floridest = cast[proc () {.cdecl.}](semctx.macrolib.symAddr("ct_flori_destruct"))
    if not floridest.isNil:
      floridest()
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
