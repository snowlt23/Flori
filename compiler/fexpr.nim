
import strutils, sequtils
import options
import tables
import terminal

import types
export types.Span
export types.FExprKind
export types.FExpr

const fexprAtoms* = {fexprIdent..fexprStrLit}
const fexprNames* = {fexprIdent, fexprPrefix, fexprInfix}
const fexprContainer* = {fexprSeq, fexprArray, fexprList, fexprBlock}

proc `$`*(fexpr: FExpr): string

proc hint*(fexpr: FExpr, msg: string) =
  let h = " $#($#:$#): " % [fexpr.span.filename, $fexpr.span.line, $fexpr.span.linepos] & msg
  styledEcho(fgGreen, "[Hint] ", resetStyle, h)
proc error*(fexpr: FExpr, msg: string) =
  let e = "$#($#:$#): " % [fexpr.span.filename, $fexpr.span.line, $fexpr.span.linepos] & msg
  styledEcho(fgRed, "[Error] ", resetStyle, e)
  when defined(release) or defined(noExceptionError):
    quit()
  else:
    raise newException(FExprError, e)

template internalSpan*(): Span =
  const internalname = instantiationInfo().filename
  const internalline = instantiationInfo().line
  Span(line: 0, linepos: 0, internal: (internalname, internalline))

proc fident*(span: Span, name: Name): FExpr =
  FExpr(span: span, typ: none(Symbol), metadata: initTable[string, Metadata](), kind: fexprIdent, idname: name)
proc fprefix*(span: Span, name: Name): FExpr =
  FExpr(span: span, typ: none(Symbol), metadata: initTable[string, Metadata](), kind: fexprPrefix, idname: name)
proc finfix*(span: Span, name: Name): FExpr =
  FExpr(span: span, typ: none(Symbol), metadata: initTable[string, Metadata](), kind: fexprInfix, idname: name)
proc fquote*(span: Span, q: FExpr): FExpr =
  FExpr(span: span, typ: none(Symbol), metadata: initTable[string, Metadata](), kind: fexprQuote, quoted: q)
proc fsymbol*(span: Span, sym: Symbol): FExpr =
  FExpr(span: span, typ: none(Symbol), metadata: initTable[string, Metadata](), kind: fexprSymbol, symbol: sym)
proc fintlit*(span: Span, x: int64): FExpr =
  FExpr(span: span, typ: none(Symbol), metadata: initTable[string, Metadata](), kind: fexprIntLit, intval: x)
proc fstrlit*(span: Span, s: string): FExpr =
  FExpr(span: span, typ: none(Symbol), metadata: initTable[string, Metadata](), kind: fexprStrLit, strval: s)
proc fseq*(span: Span, sons = newSeq[FExpr]()): FExpr =
  FExpr(span: span, typ: none(Symbol), metadata: initTable[string, Metadata](), kind: fexprSeq, sons: sons)
proc farray*(span: Span, sons = newSeq[FExpr]()): FExpr =
  FExpr(span: span, typ: none(Symbol), metadata: initTable[string, Metadata](), kind: fexprArray, sons: sons)
proc flist*(span: Span, sons = newSeq[FExpr]()): FExpr =
  FExpr(span: span, typ: none(Symbol), metadata: initTable[string, Metadata](), kind: fexprList, sons: sons)
proc fblock*(span: Span, sons = newSeq[FExpr]()): FExpr =
  FExpr(span: span, typ: none(Symbol), metadata: initTable[string, Metadata](), kind: fexprBlock, sons: sons)

iterator items*(fexpr: FExpr): FExpr =
  case fexpr.kind
  of fexprContainer:
    for e in fexpr.sons:
      yield(e)
  else:
    fexpr.error("$# shouldn't be use as container" % $fexpr.kind)
iterator mitems*(fexpr: FExpr): var FExpr =
  case fexpr.kind
  of fexprContainer:
    for e in fexpr.sons.mitems:
      yield(e)
  else:
    fexpr.error("$# shouldn't be use as container" % $fexpr.kind)

proc len*(fexpr: FExpr): int =
  case fexpr.kind
  of fexprContainer:
    return fexpr.sons.len
  else:
    return 0

proc addSon*(fexpr: FExpr, f: FExpr) =
  if fexpr.kind notin fexprContainer:
    fexpr.error("$# isn't farray" % $fexpr.kind)
  fexpr.sons.add(f)
proc `[]`*(fexpr: FExpr, i: int): var FExpr =
  case fexpr.kind
  of fexprContainer:
    return fexpr.sons[i]
  else:
    fexpr.error("$# isn't container" % $fexpr.kind)
proc `[]`*(fexpr: FExpr, i: BackwardsIndex): var FExpr =
  fexpr[fexpr.len-int(i)]
proc `[]`*(fexpr: FExpr, sl: Slice[int]): FExpr =
  case fexpr.kind
  of fexprSeq:
    result = fseq(fexpr[sl.a].span, fexpr.sons[sl])
  of fexprArray:
    result = farray(fexpr[sl.a].span, fexpr.sons[sl])
  of fexprList:
    result = flist(fexpr[sl.a].span, fexpr.sons[sl])
  of fexprBlock:
    result = fblock(fexpr[sl.a].span, fexpr.sons[sl])
  else:
    fexpr.error("$# isn't container, couldn't use as slice." % $fexpr.kind)
proc `[]`*(fexpr: FExpr, sl: HSlice[int, BackwardsIndex]): FExpr =
  fexpr[sl.a..fexpr.len-int(sl.b)]
proc `[]=`*(fexpr: FExpr, i: int, f: FExpr) =
  case fexpr.kind
  of fexprContainer:
    fexpr.sons[i] = f
  else:
    fexpr.error("$# isn't container" % $fexpr.kind)

proc genIndent*(indent: int): string =
  repeat(' ', indent)

proc toString*(fexpr: FExpr, indent: int): string =
  case fexpr.kind
  of fexprIdent, fexprPrefix, fexprInfix:
    $fexpr.idname
  of fexprQuote:
    "`" & fexpr.quoted.toString(indent)
  of fexprSymbol:
    $fexpr.symbol
  of fexprIntLit:
    $fexpr.intval
  of fexprStrLit:
    "\"" & fexpr.strval & "\""
  of fexprSeq:
    if fexpr.len == 2 and fexpr[1].kind in {fexprList, fexprArray}:
      fexpr.sons[0].toString(indent) & fexpr.sons[1..^1].mapIt(it.toString(indent)).join(" ")
    else:
      fexpr.sons.mapIt(it.toString(indent)).join(" ")
  of fexprArray:
    "[" & fexpr.sons.mapIt(it.toString(indent)).join(", ") & "]"
  of fexprList:
    "(" & fexpr.sons.mapIt(it.toString(indent)).join(", ") & ")"
  of fexprBlock:
    "{" & "\n" & genIndent(indent + 2) & fexpr.sons.mapIt(it.toString(indent + 2)).join("\n" & genIndent(indent + 2)) & "\n" & "}"

proc `$`*(fexpr: FExpr): string = fexpr.toString(0)
