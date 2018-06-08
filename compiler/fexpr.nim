
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
  let h = " $#($#:$#): " % [$span.filename, $span.line, $span.linepos] & msg
  styledEcho(fgGreen, "[Hint] ", resetStyle, h)
proc error*(span: Span, msg: string) =
  for expand in gCtx.expands:
    let e = "$#($#:$#): expand by" % [$expand.filename, $expand.line, $expand.linepos]
    styledEcho(fgGreen, "[Expand] ", resetStyle, e)
  var e = ""
  if span.isinternal:
    e = "internal.$#($#): " % [$span.filename, $span.line] & msg
  else:
    e = "$#($#:$#): " % [$span.filename, $span.line, $span.linepos] & msg
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
proc hint*(fexpr: FExpr, msg: string) = fexpr.obj.span.hint(msg)
proc error*(fexpr: FExprObj, msg: string) = fexpr.span.error(msg)
proc error*(fexpr: FExpr, msg: string) = fexpr.obj.span.error(msg)

template internalSpan*(): Span =
  Span(filename: istring(instantiationInfo().filename), line: instantiationInfo().line, isinternal: true)

proc fident*(span: Span, name: string): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprIdent, idname: istring(name)))
proc fprefix*(span: Span, name: string): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprPrefix, idname: istring(name)))
proc finfix*(span: Span, name: string, p: int, isleft: bool): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprInfix, idname: istring(name), priority: p, isleft: isleft))
proc fquote*(span: Span, q: FExpr): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprQuote, quoted: q))
proc fsymbol*(span: Span, sym: Symbol): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprSymbol, symbol: sym))
proc fintlit*(span: Span, x: int64): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprIntLit, intval: x))
proc ffloatlit*(span: Span, x: float): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprFloatLit, floatval: x))
proc fstrlit*(span: Span, s: string): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprStrLit, strval: istring(s)))
proc fseq*(span: Span, sons: IArray[FExpr]): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprSeq, sons: sons))
proc farray*(span: Span, sons: IArray[FExpr]): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprArray, sons: sons))
proc flist*(span: Span, sons: IArray[FExpr]): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprList, sons: sons))
proc fblock*(span: Span, sons: IArray[FExpr]): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprBlock, sons: sons))
proc fcontainer*(span: Span, kind: FExprKind, sons: IArray[FExpr]): FExpr =
  var f: FExprObj
  f.span = span
  f.kind = kind
  f.sons = sons
  genFExpr(f)

proc kind*(fexpr: FExpr): FExprKind = fexpr.obj.kind
proc idname*(fexpr: FExpr): IString = fexpr.obj.idname
proc priority*(fexpr: FExpr): int = fexpr.obj.priority
proc isleft*(fexpr: FExpr): bool = fexpr.obj.isleft
proc intval*(fexpr: FExpr): int64 = fexpr.obj.intval
proc strval*(fexpr: FExpr): IString = fexpr.obj.strval
proc symbol*(fexpr: FExpr): Symbol = fexpr.obj.symbol
proc span*(fexpr: FExpr): Span = fexpr.obj.span
proc sons*(fexpr: FExpr): var IArray[FExpr] = fexpr.obj.sons
proc src*(fexpr: FExpr): Option[IString] = fexpr.obj.src
proc `src=`*(fexpr: FExpr, opt: Option[IString]) = fexpr.obj.src = opt
proc typ*(fexpr: FExpr): Option[Symbol] = fexpr.obj.typ
proc `typ=`*(fexpr: FExpr, opt: Option[Symbol]) = fexpr.obj.typ = opt
proc len*(fexpr: FExpr): int = fexpr.obj.sons.len
proc scope*(fexpr: FExpr): Option[FScope] = fexpr.obj.scope
proc `scope=`*(fexpr: FExpr, opt: Option[FScope]) = fexpr.obj.scope = opt

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

proc `[]`*(fexpr: var FExprObj, i: int): var FExpr =
  case fexpr.kind
  of fexprContainer:
    return fexpr.sons.mget(i)
  else:
    fexpr.error("$# isn't container" % $fexpr.kind)
proc `[]`*(fexpr: var FExprObj, i: BackwardsIndex): var FExpr =
  fexpr[fexpr.len-int(i)]
proc `[]`*(fexpr: FExpr, i: int): var FExpr =
  fexpr.obj[i]
proc `[]`*(fexpr: FExpr, i: BackwardsIndex): var FExpr =
  fexpr.obj[fexpr.len-int(i)]
proc `[]=`*(fexpr: var FExprObj, i: int, f: FExpr) =
  case fexpr.kind
  of fexprContainer:
    fexpr.sons[i] = f
  else:
    fexpr.error("$# isn't container" % $fexpr.kind)
proc `[]=`*(fexpr: var FExprObj, i: BackwardsIndex, f: FExpr) =
  fexpr[fexpr.len-int(i)] = f
proc `[]=`*(fexpr: FExpr, i: int, f: FExpr) =
  fexpr.obj[i] = f
proc `[]=`*(fexpr: FExpr, i: BackwardsIndex, f: FExpr) =
  fexpr.obj[fexpr.len-int(i)] = f

proc isNormalFuncCall*(fexpr: var FExprObj): bool =
  fexpr.kind == fexprSeq and fexpr.len == 2 and fexpr[1].kind == fexprList
proc isGenericsFuncCall*(fexpr: var FExprObj): bool =
  fexpr.kind == fexprSeq and fexpr.len == 3 and fexpr[1].kind == fexprArray and fexpr[2].kind == fexprList
proc isInfixFuncCall*(fexpr: var FExprObj): bool =
  fexpr.kind == fexprSeq and fexpr.len == 3 and (fexpr[0].kind == fexprInfix or (fexpr[0].kind == fexprSymbol and fexpr[0].symbol.kind == symbolInfix))
proc isFuncCall*(fexpr: var FExprObj): bool =
  fexpr.isNormalFuncCall or fexpr.isGenericsFuncCall or fexpr.isInfixFuncCall

proc isNormalFuncCall*(fexpr: FExpr): bool = fexpr.obj.isNormalFuncCall
proc isGenericsFuncCall*(fexpr: FExpr): bool = fexpr.obj.isGenericsFuncCall
proc isInfixFuncCall*(fexpr: FExpr): bool = fexpr.obj.isInfixFuncCall
proc isFuncCall*(fexpr: FExpr): bool = fexpr.obj.isFuncCall

proc genIndent*(indent: int): string =
  repeat(' ', indent)

proc toString*(fexpr: FExpr, indent: int, desc: bool, typ: bool): string
  
proc toString*(fexpr: var FExprObj, indent: int, desc: bool, typ: bool): string =
  case fexpr.kind
  of fexprIdent, fexprPrefix, fexprInfix:
    result = $fexpr.idname
  of fexprQuote:
    result = "`" & fexpr.quoted.toString(indent, desc, typ)
  of fexprSymbol:
    result = toString(fexpr.symbol, desc)
  of fexprIntLit:
    result = $fexpr.intval
  of fexprFloatLit:
    result = $fexpr.floatval
  of fexprStrLit:
    result = "\"" & $fexpr.strval & "\""
  of fexprSeq:
    if fexpr.isNormalFuncCall:
      result = fexpr[0].toString(indent, desc, typ) & fexpr[1].toString(indent, desc, typ)
    elif fexpr.isGenericsFuncCall:
      result = fexpr[0].toString(indent, desc, typ) & fexpr[1].toString(indent, desc, typ) & fexpr[2].toString(indent, desc, typ)
    elif fexpr.isInfixFuncCall:
      if ($fexpr[0])[0] == '.':
        result = fexpr[1].toString(indent, desc, typ) & fexpr[0].toString(indent, desc, typ) & fexpr[2].toString(indent, desc, typ)
      else:
        result = fexpr[1].toString(indent, desc, typ) & " " & fexpr[0].toString(indent, desc, typ) & " " & fexpr[2].toString(indent, desc, typ)
    else:
      result = fexpr.sons.mapIt(it.obj.toString(indent, desc, typ)).join(" ")
    if typ:
      if fexpr.typ.isSome:
        result &= " => " & $fexpr.typ.get
      else:
        result &= " => untyped"
  of fexprArray:
    result = "[" & fexpr.sons.mapIt(it.obj.toString(indent, desc, typ)).join(", ") & "]"
  of fexprList:
    result = "(" & fexpr.sons.mapIt(it.obj.toString(indent, desc, typ)).join(", ") & ")"
  of fexprBlock:
    if fexpr.len == 0:
      result = "{}"
    else:
      result = "{" & "\n" & genIndent(indent + 2) & fexpr.sons.mapIt(it.obj.toString(indent + 2, desc, typ)).join("\n" & genIndent(indent + 2)) & "\n" & genIndent(indent) & "}"
proc toString*(fexpr: FExpr, indent: int, desc: bool, typ: bool): string = fexpr.obj.toString(indent, desc, typ)

proc `$`*(fexpr: var FExprObj): string = fexpr.toString(0, false, false)
proc `$`*(fexpr: FExpr): string = $fexpr.obj
proc desc*(fexpr: var FExprObj): string = fexpr.toString(0, true, false)
proc desc*(fexpr: FExpr): string = fexpr.obj.desc()
proc debug*(fexpr: var FExprObj): string = fexpr.toString(0, true, true)
proc debug*(fexpr: FExpr): string = fexpr.obj.debug()

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


proc isSpecTypes*(types: FExpr): bool =
  for t in types:
    if t.kind != fexprSymbol: return false
    if not t.symbol.isSpecSymbol: return false
  return true

proc isParametricTypeExpr*(fexpr: FExpr, pos: int): bool =
  if fexpr.kind != fexprSeq: return false
  if fexpr.len <= pos+1: return false
  return (fexpr.obj[pos].kind == fexprIdent or fexpr.obj[pos].kind == fexprQuote or fexpr.obj[pos].kind == fexprSymbol) and fexpr.obj[pos+1].kind == fexprArray
proc isPragmaPrefix*(fexpr: FExpr): bool =
  fexpr.kind == fexprPrefix and $fexpr == "$"

proc copy*(fexpr: FExpr): FExpr =
  if fexpr.kind in fexprContainer:
    var sons = newSeq[FExpr]()
    for son in fexpr:
      sons.add(son.copy)
    result = fcontainer(fexpr.span, fexpr.kind, iarray(sons))
    result.scope = fexpr.scope
    result.src = fexpr.src
    result.typ = fexpr.typ
  else:
    result = fexpr
