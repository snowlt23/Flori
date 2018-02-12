
import types, fexpr, parser
import strutils

proc ffiSpan*(): Span =
  Span(filename: "expanded", line: 0, linepos: 0, pos: 0)

proc ffiNewFIdent*(ident: cstring): FExpr {.cdecl.} =
  fident(ffiSpan(), name($ident))
proc ffiNewFSeq*(): FExpr {.cdecl.} =
  fseq(ffiSpan())
proc ffiNewFList*(): FExpr {.cdecl.} =
  flist(ffiSpan())
proc ffiNewFBlock*(): FExpr {.cdecl.} =
  fblock(ffiSpan())
proc ffiParseFExpr*(cstr: cstring): FExpr {.cdecl.} =
  var ctx = newParserContext("macrolib", $cstr)
  return ctx.parseFExpr()
proc ffiPrintFExpr*(fexpr: FExpr) {.cdecl.} =
  stdout.write($fexpr)
proc ffiAddSon*(fexpr: FExpr, son: FExpr) {.cdecl.} =
  fexpr.addSon(son)
proc ffiExpect*(fexpr: FExpr, son: FExprKind) {.cdecl.} =
  if fexpr.kind != son:
    fexpr.error("expect fexpr kind: $#, but: $#" % [$son, $fexpr.kind])
proc ffiAccess*(fexpr: FExpr, i: int): FExpr {.cdecl.} =
  fexpr[i]
