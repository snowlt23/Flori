
import strutils, sequtils
import options
import tables
import terminal
import deques
import algorithm

import linmem, image, symbol

const fexprAtoms* = {fexprIdent..fexprStrLit}
const fexprNames* = {fexprIdent, fexprPrefix, fexprInfix}
const fexprAllNames* = {fexprIdent, fexprPrefix, fexprInfix, fexprQuote, fexprSymbol}
const fexprContainer* = {fexprSeq, fexprArray, fexprList, fexprBlock}

proc `$`*(fexpr: FExpr): string

proc hint*(span: Span, msg: string) =
  let h = " $#($#:$#): " % [$span.filename, $span.line, $span.linepos] & msg
  styledEcho(fgGreen, "[Hint] ", resetStyle, h)
proc error*(span: Span, msg: string) =
  for expand in gCtx.expands:
    let e = "$#($#:$#): expand by" % [$expand.filename, $expand.line, $expand.linepos]
    styledEcho(fgGreen, "[Expand] ", resetStyle, e)
  let e = "$#($#:$#): " % [$span.filename, $span.line, $span.linepos] & msg
  styledEcho(fgRed, "[Error] ", resetStyle, e)

  when defined(replError):
    raise newException(FExprError, e)
  elif defined(release) or defined(noExceptionError):
    quit()
  else:
    raise newException(FExprError, e)

template assert*(fexpr: FExpr, b: typed) =
  if not b:
    fexpr.error("internal error.")

proc hint*(fexpr: FExpr, msg: string) = fexpr.span.hint(msg)
proc error*(fexpr: FExpr, msg: string) = fexpr.span.error(msg)

template internalSpan*(): Span =
  const internalname = instantiationInfo().filename
  const internalline = instantiationInfo().line
  Span(filename: istring(internalname), line: internalline, linepos: 0, pos: 0, isinternal: true)

proc fident*(span: Span, name: IString): FExpr =
  genFExpr(FExprObj(span: span, metadata: newMetadataStore(), kind: fexprIdent, idname: name))
proc fprefix*(span: Span, name: IString): FExpr =
  genFExpr(FExprObj(span: span, metadata: newMetadataStore(), kind: fexprPrefix, idname: name))
proc finfix*(span: Span, name: IString, p: int, isleft: bool): FExpr =
  genFExpr(FExprObj(span: span, metadata: newMetadataStore(), kind: fexprInfix, idname: name, priority: p, isleft: isleft))
proc fquote*(span: Span, q: FExpr): FExpr =
  genFExpr(FExprObj(span: span, metadata: newMetadataStore(), kind: fexprQuote, quoted: q))
proc fsymbol*(span: Span, sym: Symbol): FExpr =
  genFExpr(FExprObj(span: span, metadata: newMetadataStore(), kind: fexprSymbol, symbol: sym))
proc fintlit*(span: Span, x: int64): FExpr =
  genFExpr(FExprObj(span: span, metadata: newMetadataStore(), kind: fexprIntLit, intval: x))
proc ffloatlit*(span: Span, x: float): FExpr =
  genFExpr(FExprObj(span: span, metadata: newMetadataStore(), kind: fexprFloatLit, floatval: x))
proc fstrlit*(span: Span, s: IString): FExpr =
  genFExpr(FExprObj(span: span, metadata: newMetadataStore(), kind: fexprStrLit, strval: s))
proc fseq*(span: Span, sons: IList[FExpr]): FExpr =
  genFExpr(FExprObj(span: span, metadata: newMetadataStore(), kind: fexprSeq, sons: sons))
proc fseq*(span: Span, sons = newSeq[FExpr]()): FExpr =
  fseq(span, ilist(sons))
proc farray*(span: Span, sons: IList[FExpr]): FExpr =
  genFExpr(FExprObj(span: span, metadata: newMetadataStore(), kind: fexprArray, sons: sons))
proc farray*(span: Span, sons = newSeq[FExpr]()): FExpr =
  farray(span, ilist(sons))
proc flist*(span: Span, sons: IList[FExpr]): FExpr =
  genFExpr(FExprObj(span: span, metadata: newMetadataStore(), kind: fexprList, sons: sons))
proc flist*(span: Span, sons = newSeq[FExpr]()): FExpr =
  flist(span, ilist(sons))
proc fblock*(span: Span, sons: IList[FExpr]): FExpr =
  genFExpr(FExprObj(span: span, metadata: newMetadataStore(), kind: fexprBlock, sons: sons))
proc fblock*(span: Span, sons = newSeq[FExpr]()): FExpr =
  fblock(span, ilist(sons))
proc fcontainer*(span: Span, kind: FExprKind, sons: IList[FExpr]): FExpr =
  result = genFExpr(FExprObj())
  result.span = span
  result.metadata = newMetadataStore()
  result.kind = kind
  result.sons = sons

proc addSon*(f: FExpr, son: FExpr) =
  f.obj.sons.last = ilist(son, ilistNil[FExpr]())

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

proc `[]`*(fexpr: FExpr, i: int): var FExpr =
  case fexpr.kind
  of fexprContainer:
    return fexpr.sons[i]
  else:
    fexpr.error("$# isn't container" % $fexpr.kind)
proc `[]`*(fexpr: FExpr, i: BackwardsIndex): var FExpr =
  fexpr[fexpr.len-int(i)]
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
    toString(fexpr.symbol, desc)
  of fexprIntLit:
    $fexpr.intval
  of fexprFloatLit:
    $fexpr.floatval
  of fexprStrLit:
    "\"" & $fexpr.strval & "\""
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

proc getSrcExpr*(fexpr: FExpr): string =
  if fexpr.src.isSome:
    $fexpr.src.get
  else:
    $fexpr
    
proc isParametricTypeExpr*(fexpr: FExpr, pos: int): bool =
  if fexpr.kind != fexprSeq: return false
  if fexpr.len <= pos+1: return false
  return (fexpr[pos].kind == fexprIdent or fexpr[pos].kind == fexprQuote or fexpr[pos].kind == fexprSymbol) and fexpr[pos+1].kind == fexprArray
proc isPragmaPrefix*(fexpr: FExpr): bool =
  fexpr.kind == fexprPrefix and $fexpr == "$"

proc copy*(fexpr: FExpr): FExpr =
  if fexpr.kind in fexprContainer:
    result = fcontainer(fexpr.span, fexpr.kind, ilistNil[FExpr]())
    result.metadata = fexpr.metadata
    for son in fexpr:
      result.addSon(son.copy)
  elif fexpr.kind == fexprSymbol:
    result = fsymbol(fexpr.span, fexpr.symbol)
    result.metadata = fexpr.metadata
  else:
    result = fexpr

proc name*(fexpr: FExpr): string =
  if fexpr.kind == fexprQuote:
    name(fexpr.quoted)
  else:
    $fexpr

