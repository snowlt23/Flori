
import types, fexpr, parser

proc ffiSpan*(): Span =
  Span(filename: "macrolib", line: 0, linepos: 0, pos: 0)

proc ffiNewFIdent*(ident: cstring): FExpr {.cdecl.} =
  fident(ffiSpan(), name($ident))
proc ffiNewFBlock*(): FExpr {.cdecl.} =
  fblock(ffiSpan())
