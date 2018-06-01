
import strutils, sequtils
import options
import tables
import terminal
import deques
import algorithm

import image, symbol

const fexprAtoms* = {fexprIdent..fexprStrLit}
const fexprNames* = {fexprIdent, fexprPrefix, fexprInfix, fexprSymbol}
const fexprContainer* = {fexprSeq, fexprArray, fexprList, fexprBlock}

proc `$`*(fexpr: FExpr): string

proc hint*(span: Span, msg: string) =
  let h = " $#($#:$#): " % [span.filename, $span.line, $span.linepos] & msg
  styledEcho(fgGreen, "[Hint] ", resetStyle, h)
proc error*(span: Span, msg: string) =
  for expand in gCtx.expands:
    let e = "$#($#:$#): expand by" % [expand.filename, $expand.line, $expand.linepos]
    styledEcho(fgGreen, "[Expand] ", resetStyle, e)
  var e = ""
  if span.isinternal:
    e = "internal.$#($#): " % [span.filename, $span.line] & msg
  else:
    e = "$#($#:$#): " % [span.filename, $span.line, $span.linepos] & msg
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

proc hint*(fexpr: FExprObj, msg: string) = fexpr.span.hint(msg)
proc error*(fexpr: FExprObj, msg: string) = fexpr.span.error(msg)

template internalSpan*(): Span =
  Span(filename: instantiationInfo().filename, line: instantiationInfo().line, isinternal: true)

proc fident*(span: Span, name: string): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprIdent, idname: fstring(name)))
proc fprefix*(span: Span, name: string): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprPrefix, idname: fstring(name)))
proc finfix*(span: Span, name: string, p: int, isleft: bool): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprInfix, idname: fstring(name), priority: p, isleft: isleft))
proc fquote*(span: Span, q: FExpr): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprQuote, quoted: q))
proc fsymbol*(span: Span, sym: Symbol): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprSymbol, symbol: sym))
proc fintlit*(span: Span, x: int64): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprIntLit, intval: x))
proc ffloatlit*(span: Span, x: float): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprFloatLit, floatval: x))
proc fstrlit*(span: Span, s: string): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprStrLit, strval: fstring(s)))
proc fseq*(span: Span, sons = newSeq[FExpr]()): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprSeq, sons: sons))
proc farray*(span: Span, sons = newSeq[FExpr]()): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprArray, sons: sons))
proc flist*(span: Span, sons = newSeq[FExpr]()): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprList, sons: sons))
proc fblock*(span: Span, sons = newSeq[FExpr]()): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprBlock, sons: sons))
proc fcontainer*(span: Span, kind: FExprKind, sons = newSeq[FExpr]()): FExpr =
  var f: FExprObj
  f.span = span
  f.kind = kind
  f.sons = sons
  genFExpr(f)

proc kind*(fexpr: FExpr): FExprKind = fexpr.obj.kind
proc symbol*(fexpr: FExpr): Symbol = fexpr.obj.symbol
proc span*(fexpr: FExpr): Span = fexpr.obj.span
proc sons*(fexpr: FExpr): var seq[FExpr] = fexpr.obj.sons

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

proc len*(fexpr: FExprObj): int =
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
proc `[]`*(fexpr: var FExprObj, i: int): var FExpr =
  case fexpr.kind
  of fexprContainer:
    return fexpr.sons[i]
  else:
    fexpr.error("$# isn't container" % $fexpr.kind)
proc `[]`*(fexpr: var FExprObj, i: BackwardsIndex): var FExpr =
  fexpr[fexpr.len-int(i)]
proc `[]`*(fexpr: var FExprObj, sl: Slice[int]): FExpr =
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
proc `[]`*(fexpr: var FExprObj, sl: HSlice[int, BackwardsIndex]): FExpr =
  fexpr[sl.a..fexpr.len-int(sl.b)]
proc `[]=`*(fexpr: var FExprObj, i: int, f: FExpr) =
  case fexpr.kind
  of fexprContainer:
    fexpr.sons[i] = f
  else:
    fexpr.error("$# isn't container" % $fexpr.kind)
proc `[]=`*(fexpr: var FExprObj, i: BackwardsIndex, f: FExpr) =
  fexpr[fexpr.len-int(i)] = f

proc isNormalFuncCall*(fexpr: var FExprObj): bool =
  fexpr.kind == fexprSeq and fexpr.len == 2 and fexpr[1].kind == fexprList
proc isGenericsFuncCall*(fexpr: var FExprObj): bool =
  fexpr.kind == fexprSeq and fexpr.len == 3 and fexpr[1].kind == fexprArray and fexpr[2].kind == fexprList
proc isInfixFuncCall*(fexpr: var FExprObj): bool =
  fexpr.kind == fexprSeq and fexpr.len == 3 and (fexpr[0].kind == fexprInfix or (fexpr[0].kind == fexprSymbol and fexpr[0].symbol.kind == symbolInfix))
proc isFuncCall*(fexpr: var FExprObj): bool =
  fexpr.isNormalFuncCall or fexpr.isGenericsFuncCall or fexpr.isInfixFuncCall

proc genIndent*(indent: int): string =
  repeat(' ', indent)

proc toString*(fexpr: var FExprObj, indent: int, desc: bool): string =
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
      fexpr.sons.mapIt(it.obj.toString(indent, desc)).join(" ")
  of fexprArray:
    "[" & fexpr.sons.mapIt(it.obj.toString(indent, desc)).join(", ") & "]"
  of fexprList:
    "(" & fexpr.sons.mapIt(it.obj.toString(indent, desc)).join(", ") & ")"
  of fexprBlock:
    if fexpr.len == 0:
      "{}"
    else:
      "{" & "\n" & genIndent(indent + 2) & fexpr.sons.mapIt(it.obj.toString(indent + 2, desc)).join("\n" & genIndent(indent + 2)) & "\n" & genIndent(indent) & "}"

proc `$`*(fexpr: var FExprObj): string = fexpr.toString(0, false)
proc `$`*(fexpr: FExpr): string = fexpr.obj.toString(0, false)
proc desc*(fexpr: FExpr): string = fexpr.obj.toString(0, true)

proc getSrcExpr*(fexpr: FExpr): string =
  if fexpr.obj.src.isSome:
    $fexpr.obj.src.get
  else:
    $fexpr
    
proc name*(fexpr: FExpr): string =
  case fexpr.kind
  of fexprNames:
    return $fexpr
  of fexprQuote:
    return name(fexpr.obj.quoted)
  else:
    fexpr.error("$# is not name." % $fexpr)



proc isParametricTypeExpr*(fexpr: FExpr, pos: int): bool =
  if fexpr.kind != fexprSeq: return false
  if fexpr.len <= pos+1: return false
  return (fexpr.obj[pos].kind == fexprIdent or fexpr.obj[pos].kind == fexprQuote or fexpr.obj[pos].kind == fexprSymbol) and fexpr.obj[pos+1].kind == fexprArray
proc isPragmaPrefix*(fexpr: FExpr): bool =
  fexpr.kind == fexprPrefix and $fexpr == "$"
