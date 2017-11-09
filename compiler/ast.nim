
import strutils, sequtils

type
  FExprError* = object of Exception
  Span* = object
    filename*: string
    line*: int
    linepos*: int
    pos*: int
    internal*: tuple[filename: string, line: int]
  FExprKind* = enum
    fexprIdent
    fexprSpecial
    fexprIntLit
    fexprStrLit
    fexprSeq
    fexprArray
    fexprList
    fexprBlock
    fexprCall
  FExpr* = ref object
    span*: Span
    case kind*: FExprKind
    of fexprIdent:
      ident*: string
    of fexprSpecial:
      special*: string
    of fexprIntLit:
      intval*: int64
    of fexprStrLit:
      strval*: string
    of fexprSeq, fexprArray, fexprList, fexprBlock, fexprCall:
      sons*: seq[FExpr]

const fexprAtoms* = {fexprIdent..fexprStrLit}
const fexprContainer* = {fexprSeq..fexprCall}

proc `$`*(fexpr: FExpr): string

proc error*(fexpr: FExpr, msg: string) =
  raise newException(FExprError, "$#($#:$#): " % [fexpr.span.filename, $fexpr.span.line, $fexpr.span.linepos] & msg)

template internalSpan*(): Span =
  const internalname = instantiationInfo().filename
  const internalline = instantiationInfo().line
  Span(line: 0, linepos: 0, internal: (internalname, internalline))

proc fident*(span: Span, id: string): FExpr =
  FExpr(span: span, kind: fexprIdent, ident: id)
proc fspecial*(span: Span, sp: string): FExpr =
  FExpr(span: span, kind: fexprSpecial, special: sp)
proc fintlit*(span: Span, x: int64): FExpr =
  FExpr(span: span, kind: fexprIntLit, intval: x)
proc fstrlit*(span: Span, s: string): FExpr =
  FExpr(span: span, kind: fexprStrLit, strval: s)
proc fseq*(span: Span, sons = newSeq[FExpr]()): FExpr =
  FExpr(span: span, kind: fexprSeq, sons: sons)
proc farray*(span: Span, sons = newSeq[FExpr]()): FExpr =
  FExpr(span: span, kind: fexprArray, sons: sons)
proc flist*(span: Span, sons = newSeq[FExpr]()): FExpr =
  FExpr(span: span, kind: fexprList, sons: sons)
proc fblock*(span: Span, sons = newSeq[FExpr]()): FExpr =
  FExpr(span: span, kind: fexprBlock, sons: sons)
proc fcall*(span: Span, call: FExpr, args = newSeq[FExpr]()): FExpr =
  FExpr(span: span, kind: fexprCall, sons: @[call] & args)

proc addSon*(fexpr: FExpr, f: FExpr) =
  if fexpr.kind notin fexprContainer:
    fexpr.error("$# has not sons" % $fexpr.kind)
  fexpr.sons.add(f)
proc `[]`*(fexpr: FExpr, i: int): FExpr =
  if fexpr.kind notin fexprContainer:
    fexpr.error("$# has not sons" % $fexpr.kind)
  return fexpr.sons[i]
proc `[]`*(fexpr: FExpr, sl: Slice[int]): FExpr =
  if fexpr.kind notin fexprContainer:
    fexpr.error("$# has not sons" % $fexpr.kind)
  result = fseq(fexpr.span)
  for i in sl:
    result.addSon(fexpr.sons[i])
proc `[]=`*(fexpr: FExpr, i: int, f: FExpr) =
  if fexpr.kind notin fexprContainer:
    fexpr.error("$# has not sons" % $fexpr.kind)
  fexpr.sons[i] = f

iterator items*(fexpr: FExpr): FExpr =
  if fexpr.kind notin fexprContainer:
    fexpr.error("$# has not sons" % $fexpr.kind)
  for son in fexpr.sons:
    yield(son)
iterator pairs*(fexpr: FExpr): (int, FExpr) =
  if fexpr.kind notin fexprContainer:
    fexpr.error("$# has not sons" % $fexpr.kind)
  for i, son in fexpr.sons:
    yield(i, son)

proc len*(fexpr: FExpr): int =
  if fexpr.kind in fexprContainer:
    return fexpr.sons.len
  else:
    return 0

proc `$`*(fexpr: FExpr): string =
  case fexpr.kind
  of fexprIdent:
    fexpr.ident
  of fexprSpecial:
    fexpr.special
  of fexprIntLit:
    $fexpr.intval
  of fexprStrLit:
    "\"" & fexpr.strval & "\""
  of fexprSeq:
    fexpr.sons.mapIt($it).join(" ")
  of fexprArray:
    "[" & fexpr.sons.mapIt($it).join(", ") & "]"
  of fexprList:
    "(" & fexpr.sons.mapIt($it).join(", ") & ")"
  of fexprBlock:
    "{" & "\n" & fexpr.sons.mapIt($it).join("\n") & "\n" & "}"
  of fexprCall:
    $fexpr.sons[0] & "(" & fexpr.sons[1..^1].mapIt($it).join(", ") & ")"
