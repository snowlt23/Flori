
import strutils, sequtils
import options
import tables
import terminal
import deques
import algorithm

import types

const fexprAtoms* = {fexprIdent..fexprStrLit}
const fexprNames* = {fexprIdent, fexprPrefix, fexprInfix}
const fexprContainer* = {fexprSeq, fexprArray, fexprList, fexprBlock}

proc `$`*(fexpr: FExpr): string

proc hint*(span: Span, msg: string) =
  let h = " $#($#:$#): " % [span.filename, $span.line, $span.linepos] & msg
  styledEcho(fgGreen, "[Hint] ", resetStyle, h)
proc error*(span: Span, msg: string) =
  for expand in gCtx.expands:
    let e = "$#($#:$#): expand by" % [expand.filename, $expand.line, $expand.linepos]
    styledEcho(fgGreen, "[Expand] ", resetStyle, e)
  let e = "$#($#:$#): " % [span.filename, $span.line, $span.linepos] & msg
  styledEcho(fgRed, "[Error] ", resetStyle, e)

  when defined(release) or defined(noExceptionError):
    quit()
  else:
    raise newException(FExprError, e)

template assert*(fexpr: FExpr, b: typed) =
  when not defined(release):
    if not b:
      fexpr.error("internal error.")

proc hint*(fexpr: FExpr, msg: string) = fexpr.span.hint(msg)
proc error*(fexpr: FExpr, msg: string) = fexpr.span.error(msg)

template internalSpan*(): Span =
  const internalname = instantiationInfo().filename
  const internalline = instantiationInfo().line
  Span(line: 0, linepos: 0, internal: (internalname, internalline))

proc fident*(span: Span, name: Name): FExpr =
  FExpr(span: span, metadata: initTable[string, Metadata](), kind: fexprIdent, idname: name)
proc fprefix*(span: Span, name: Name): FExpr =
  FExpr(span: span, metadata: initTable[string, Metadata](), kind: fexprPrefix, idname: name)
proc finfix*(span: Span, name: Name, p: int, isleft: bool): FExpr =
  FExpr(span: span, metadata: initTable[string, Metadata](), kind: fexprInfix, idname: name, priority: p, isleft: isleft)
proc fquote*(span: Span, q: FExpr): FExpr =
  FExpr(span: span, metadata: initTable[string, Metadata](), kind: fexprQuote, quoted: q)
proc fsymbol*(span: Span, sym: Symbol): FExpr =
  FExpr(span: span, metadata: initTable[string, Metadata](), kind: fexprSymbol, symbol: sym)
proc fintlit*(span: Span, x: int64): FExpr =
  FExpr(span: span, metadata: initTable[string, Metadata](), kind: fexprIntLit, intval: x)
proc ffloatlit*(span: Span, x: float): FExpr =
  FExpr(span: span, metadata: initTable[string, Metadata](), kind: fexprFloatLit, floatval: x)
proc fstrlit*(span: Span, s: string): FExpr =
  FExpr(span: span, metadata: initTable[string, Metadata](), kind: fexprStrLit, strval: s)
proc fseq*(span: Span, sons = newSeq[FExpr]()): FExpr =
  FExpr(span: span, metadata: initTable[string, Metadata](), kind: fexprSeq, sons: sons)
proc farray*(span: Span, sons = newSeq[FExpr]()): FExpr =
  FExpr(span: span, metadata: initTable[string, Metadata](), kind: fexprArray, sons: sons)
proc flist*(span: Span, sons = newSeq[FExpr]()): FExpr =
  FExpr(span: span, metadata: initTable[string, Metadata](), kind: fexprList, sons: sons)
proc fblock*(span: Span, sons = newSeq[FExpr]()): FExpr =
  FExpr(span: span, metadata: initTable[string, Metadata](), kind: fexprBlock, sons: sons)
proc fcontainer*(span: Span, kind: FExprKind, sons = newSeq[FExpr]()): FExpr =
  new result
  result.span = span
  result.metadata = initTable[string, Metadata]()
  result.kind = kind
  result.sons = sons

iterator items*(fexpr: FExpr): FExpr =
  case fexpr.kind
  of fexprContainer:
    for e in fexpr.sons:
      yield(e)
  else:
    yield(fexpr)
iterator mitems*(fexpr: FExpr): var FExpr =
  case fexpr.kind
  of fexprContainer:
    for e in fexpr.sons.mitems:
      yield(e)
  else:
    fexpr.error("this fexpr isn't container type.")
iterator pairs*(fexpr: FExpr): (int, FExpr) =
  case fexpr.kind
  of fexprContainer:
    for i, e in fexpr.sons:
      yield(i, e)
  else:
    yield(0, fexpr)
iterator mpairs*(fexpr: FExpr): (int, var FExpr) =
  case fexpr.kind
  of fexprContainer:
    for i, e in fexpr.sons.mpairs:
      yield(i, e)
  else:
    fexpr.error("this fexpr isn't container type.")

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
proc delSon*(fexpr: FExpr, i: int) =
  fexpr.sons.del(i)
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
proc `[]=`*(fexpr: FExpr, i: BackwardsIndex, f: FExpr) =
  fexpr[fexpr.len-int(i)] = f

proc isNormalFuncCall*(fexpr: FExpr): bool =
  fexpr.kind == fexprSeq and fexpr.len == 2 and fexpr[1].kind == fexprList
proc isGenericsFuncCall*(fexpr: FExpr): bool =
  fexpr.kind == fexprSeq and fexpr.len == 3 and fexpr[1].kind == fexprArray and fexpr[2].kind == fexprList
proc isInfixFuncCall*(fexpr: FExpr): bool =
  fexpr.kind == fexprSeq and fexpr.len == 3 and (fexpr[0].kind == fexprInfix or (fexpr[0].kind == fexprSymbol and fexpr[0].symbol.kind == symbolInfix))
proc isFuncCall*(fexpr: FExpr): bool =
  fexpr.isNormalFuncCall or fexpr.isGenericsFuncCall or fexpr.isInfixFuncCall

proc genIndent*(indent: int): string =
  repeat(' ', indent)

proc toString*(fexpr: FExpr, indent: int, desc: bool): string =
  case fexpr.kind
  of fexprIdent, fexprPrefix, fexprInfix:
    $fexpr.idname
  of fexprQuote:
    "`" & fexpr.quoted.toString(indent, desc)
  of fexprSymbol:
    if desc:
      $fexpr.symbol
    else:
      $fexpr.symbol.name
  of fexprIntLit:
    $fexpr.intval
  of fexprFloatLit:
    $fexpr.floatval
  of fexprStrLit:
    "\"" & fexpr.strval & "\""
  of fexprSeq:
    if fexpr.isNormalFuncCall:
      fexpr[0].toString(indent, desc) & fexpr[1].toString(indent, desc)
    elif fexpr.isGenericsFuncCall:
      fexpr[0].toString(indent, desc) & fexpr[1].toString(indent, desc) & fexpr[2].toString(indent, desc)
    elif fexpr.isInfixFuncCall:
      if ($fexpr[0])[0] == '.':
        fexpr[1].toString(indent, desc) & fexpr.sons[0].toString(indent, desc) & fexpr[2].toString(indent, desc)
      else:
        fexpr[1].toString(indent, desc) & " " & fexpr.sons[0].toString(indent, desc) & " " & fexpr[2].toString(indent, desc)
    else:
      fexpr.sons.mapIt(it.toString(indent, desc)).join(" ")
  of fexprArray:
    "[" & fexpr.sons.mapIt(it.toString(indent, desc)).join(", ") & "]"
  of fexprList:
    "(" & fexpr.sons.mapIt(it.toString(indent, desc)).join(", ") & ")"
  of fexprBlock:
    if fexpr.len == 0:
      "{}"
    else:
      "{" & "\n" & genIndent(indent + 2) & fexpr.sons.mapIt(it.toString(indent + 2, desc)).join("\n" & genIndent(indent + 2)) & "\n" & genIndent(indent) & "}"

proc `$`*(fexpr: FExpr): string = fexpr.toString(0, false)
proc desc*(fexpr: FExpr): string = fexpr.toString(0, true)
    
proc name*(fexpr: FExpr): Name =
  case fexpr.kind
  of fexprNames:
    return name($fexpr)
  of fexprQuote:
    return name(fexpr.quoted)
  of fexprSymbol:
    return fexpr.symbol.name
  else:
    fexpr.error("$# is not name." % $fexpr)



proc isParametricTypeExpr*(fexpr: FExpr, pos: int): bool =
  if fexpr.kind != fexprSeq: return false
  if fexpr.len <= pos+1: return false
  return (fexpr[pos].kind == fexprIdent or fexpr[pos].kind == fexprQuote or fexpr[pos].kind == fexprSymbol) and fexpr[pos+1].kind == fexprArray
proc isPragmaPrefix*(fexpr: FExpr): bool =
  fexpr.kind == fexprPrefix and $fexpr == "$"
