
import linmem, image, fexpr, parser, scope
import passutils

import strutils, sequtils
import options
import tables

proc ffiSpan*(): Span =
  Span(filename: istring("expanded"), line: 0, linepos: 0, pos: 0)

proc ffiNewFIdent*(ident: cstring): FExpr {.cdecl.} =
  fident(ffiSpan(), istring($ident))
proc ffiNewFSeq*(): FExpr {.cdecl.} =
  fseq(ffiSpan())
proc ffiNewFArray*(): FExpr {.cdecl.} =
  farray(ffiSpan())
proc ffiNewFList*(): FExpr {.cdecl.} =
  flist(ffiSpan())
proc ffiNewFBlock*(): FExpr {.cdecl.} =
  fblock(ffiSpan())
proc ffiNewFIntLit*(x: int): FExpr {.cdecl.} =
  fintlit(ffiSpan(), x)
proc ffiNewFStrLit*(cstr: cstring): FExpr {.cdecl.} =
  fstrlit(ffiSpan(), istring($cstr))
proc ffiParseFExpr*(filename: cstring, line: int, linepos: int, cstr: cstring): FExpr {.cdecl.} =
  var ctx = newParserContext($filename, $cstr)
  ctx.line = line
  ctx.linepos = linepos
  return ctx.parseFExpr()
proc ffiPrintFExpr*(fexpr: FExpr) {.cdecl.} =
  stdout.write($fexpr)
proc ffiQuoted*(fexpr: FExpr): FExpr {.cdecl.} =
  fexpr.quoted
proc ffiLength*(fexpr: FExpr): int {.cdecl.} =
  fexpr.len
proc ffiPush*(fexpr: FExpr, son: FExpr) {.cdecl.} =
  fexpr.addSon(son)
proc ffiKind*(fexpr: FExpr): FExprKind {.cdecl.} =
  fexpr.kind
proc ffiExpect*(fexpr: FExpr, son: FExprKind) {.cdecl.} =
  if fexpr.kind != son:
    fexpr.error("expect fexpr kind: $#, but: $#" % [$son, $fexpr.kind])
proc ffiError*(fexpr: FExpr, msg: cstring) {.cdecl.} =
  fexpr.error($fexpr & " : " & $msg)
proc ffiAccess*(fexpr: FExpr, i: int): FExpr {.cdecl.} =
  fexpr[i]
proc ffiSet*(fexpr: FExpr, i: int, value: FExpr) {.cdecl.} =
  fexpr[i] = value
proc ffiToCS*(fexpr: FExpr): cstring {.cdecl.} =
  cstring($fexpr)
proc ffiIntval*(fexpr: FExpr): int64 {.cdecl.} =
  fexpr.intval
proc ffiStrval*(fexpr: FExpr): cstring {.cdecl.} =
  cstring($fexpr.strval)
proc ffiGensym*(): FExpr {.cdecl.} =
  return fident(ffiSpan(), istring(gCtx.genTmpName()))
proc ffiGetType*(fexpr: FExpr): FExpr {.cdecl.} =
  if not fexpr.hasTyp:
    fexpr.error("fexpr hasn't type.")
  return fsymbol(ffiSpan(), fexpr.metadata.typ)
proc ffiGetSrcExpr*(fexpr: FExpr): cstring {.cdecl.} =
  cstring(fexpr.getSrcExpr)
