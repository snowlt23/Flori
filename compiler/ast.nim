
type
  Span* = object
    filename*: string
    line*: int
    linepos*: int
    pos*: int
    internal*: tuple[filename: string, line: int]
  FExprKind* = enum
    fexprIdent
    fexprIntLit
    fexprStrLit
    fexprLabel
    fexprArray
    fexprList
    fexprBlock
    fexprCall
  FExpr* = ref object
    span*: Span
    case kind*: FExprKind
    of fexprIdent:
      ident*: string
    of fexprIntLit:
      intval*: int64
    of fexprStrLit:
      strval*: string
    of fexprLabel:
      labelname*: string
      labelexpr*: FExpr
    of fexprArray, fexprList:
      exprs: seq[FExpr]
    of fexprBlockCall:
    of fexprCall:

const fexprAtoms* = {fexprIdent..fexprStrLit}
