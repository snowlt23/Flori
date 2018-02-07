
import types, fexpr, parser

proc ffiSpan*(): Span =
  Span(filename: "macrolib", line: 0, linepos: 0, pos: 0)

proc ffiNewFIdent*(ident: cstring): FExpr {.cdecl.} =
  fident(ffiSpan(), name($ident))
proc ffiNewFBlock*(): FExpr {.cdecl.} =
  fblock(ffiSpan())
proc ffiParseFExpr*(cstr: cstring): FExpr {.cdecl.} =
  var ctx = newParserContext("macrolib", $cstr)
  return ctx.parseFExpr()
proc ffiPrintFExpr*(fexpr: FExpr) {.cdecl.} =
  stdout.write($fexpr)
