
import scope
import strutils, sequtils
import options
import types
export types.Span
export types.FExprKind
export types.FExpr

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
  FExpr(span: span, typ: none(Symbol), kind: fexprIdent, ident: id)
proc fsymbol*(span: Span, sym: Symbol): FExpr =
  FExpr(span: span, typ: none(Symbol), kind: fexprSymbol, symbol: sym)
proc fprefix*(span: Span, s: string): FExpr =
  FExpr(span: span, typ: none(Symbol), kind: fexprPrefix, prefix: s)
proc finfix*(span: Span, s: string): FExpr =
  FExpr(span: span, typ: none(Symbol), kind: fexprInfix, infix: s)
proc fquote*(span: Span, f: FExpr): FExpr =
  FExpr(span: span, typ: none(Symbol), kind: fexprQuote, quoted: f)
proc fintlit*(span: Span, x: int64): FExpr =
  FExpr(span: span, typ: none(Symbol), kind: fexprIntLit, intval: x)
proc fstrlit*(span: Span, s: string): FExpr =
  FExpr(span: span, typ: none(Symbol), kind: fexprStrLit, strval: s)
proc fseq*(span: Span, sons = newSeq[FExpr]()): FExpr =
  FExpr(span: span, typ: none(Symbol), kind: fexprSeq, sons: sons)
proc farray*(span: Span, sons = newSeq[FExpr]()): FExpr =
  FExpr(span: span, typ: none(Symbol), kind: fexprArray, sons: sons)
proc flist*(span: Span, sons = newSeq[FExpr]()): FExpr =
  FExpr(span: span, typ: none(Symbol), kind: fexprList, sons: sons)
proc fblock*(span: Span, sons = newSeq[FExpr]()): FExpr =
  FExpr(span: span, typ: none(Symbol), kind: fexprBlock, sons: sons)
proc fcall*(span: Span, call: FExpr, args = newSeq[FExpr]()): FExpr =
  FExpr(span: span, typ: none(Symbol), kind: fexprCall, sons: @[call] & args)

proc addSon*(fexpr: FExpr, f: FExpr) =
  if fexpr.kind notin fexprContainer:
    fexpr.error("$# has not sons" % $fexpr.kind)
  fexpr.sons.add(f)
proc `[]`*(fexpr: FExpr, i: int): FExpr =
  if fexpr.kind notin fexprContainer:
    fexpr.error("$# has not sons" % $fexpr.kind)
  return fexpr.sons[i]
proc `[]`*(fexpr: FExpr, sl: HSlice[int, BackwardsIndex]): FExpr =
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

proc genIndent*(indent: int): string =
  repeat(' ', indent)

proc toString*(fexpr: FExpr, indent: int): string =
  case fexpr.kind
  of fexprIdent:
    fexpr.ident
  of fexprSymbol:
    $fexpr.symbol
  of fexprPrefix:
    fexpr.prefix
  of fexprInfix:
    fexpr.infix
  of fexprQuote:
    "`" & fexpr.quoted.toString(indent)
  of fexprIntLit:
    $fexpr.intval
  of fexprStrLit:
    "\"" & fexpr.strval & "\""
  of fexprSeq:
    fexpr.sons.mapIt(it.toString(indent)).join(" ")
  of fexprArray:
    "[" & fexpr.sons.mapIt(it.toString(indent)).join(", ") & "]"
  of fexprList:
    "(" & fexpr.sons.mapIt(it.toString(indent)).join(", ") & ")"
  of fexprBlock:
    "{" & "\n" & genIndent(indent + 2) & fexpr.sons.mapIt(it.toString(indent + 2)).join("\n" & genIndent(indent + 2)) & "\n" & "}"
  of fexprCall:
    $fexpr.sons[0] & "(" & fexpr.sons[1..^1].mapIt($it).join(", ") & ")"

proc `$`*(fexpr: FExpr): string = fexpr.toString(0)
