
import types, fexpr, parser, metadata, scope, ctrc
import passutils

import strutils
import options
import tables

var gCtx*: SemanticContext

proc ffiSpan*(): Span =
  Span(filename: "expanded", line: 0, linepos: 0, pos: 0)

proc ffiNewFIdent*(ident: cstring): FExpr {.cdecl.} =
  fident(ffiSpan(), name($ident))
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
  fstrlit(ffiSpan(), $cstr)
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
proc ffiStrval*(fexpr: FExpr): cstring {.cdecl.} =
  cstring($fexpr.strval)
proc ffiGensym*(): FExpr {.cdecl.} =
  return fident(internalSpan, gCtx.genTmpName())

proc ffiDebugCTRC*(fexpr: FExpr) =
  if fexpr.hasCTRC:
    var s = newSeq[string]()
    for key, value in fexpr.ctrc.fieldbody:
      s.add("$#:{cnt:$#}" % [$key, $value.ctrc.cnt])
    echo "CTRC(cnt: $#, fieldbody: { $# })" % [$fexpr.ctrc.cnt, s.join(", ")]
  else:
    echo "noCTRC()"
