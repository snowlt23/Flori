
import strutils, sequtils
import options
import tables

import types
export types.Span
export types.FExprKind
export types.FExpr

const fexprAtoms* = {fexprIdent..fexprStrLit}
const fexprContainer* = {fexprList..fexprMap}

proc `$`*(fexpr: FExpr): string
proc reverse*(fexpr: FExpr): FExpr

proc error*(fexpr: FExpr, msg: string) =
  raise newException(FExprError, "$#($#:$#): " % [fexpr.span.filename, $fexpr.span.line, $fexpr.span.linepos] & msg)

template internalSpan*(): Span =
  const internalname = instantiationInfo().filename
  const internalline = instantiationInfo().line
  Span(line: 0, linepos: 0, internal: (internalname, internalline))

proc fident*(span: Span, id: string): FExpr =
  FExpr(span: span, typ: none(Symbol), metadata: initTable[string, Metadata](), reader: none(string), kind: fexprIdent, ident: id)
proc fattr*(span: Span, attr: string): FExpr =
  FExpr(span: span, typ: none(Symbol), metadata: initTable[string, Metadata](), reader: none(string), kind: fexprAttr, attr: attr)
proc fsymbol*(span: Span, sym: Symbol): FExpr =
  FExpr(span: span, typ: none(Symbol), metadata: initTable[string, Metadata](), reader: none(string), kind: fexprSymbol, symbol: sym)
proc fintlit*(span: Span, x: int64): FExpr =
  FExpr(span: span, typ: none(Symbol), metadata: initTable[string, Metadata](), reader: none(string), kind: fexprIntLit, intval: x)
proc fstrlit*(span: Span, s: string): FExpr =
  FExpr(span: span, typ: none(Symbol), metadata: initTable[string, Metadata](), reader: none(string), kind: fexprStrLit, strval: s)
proc fnil*(span: Span): FExpr =
  FExpr(span: span, typ: none(Symbol), metadata: initTable[string, Metadata](), reader: none(string), kind: fexprNil)
proc fcons*(span: Span, car, cdr: FExpr): FExpr =
  FExpr(span: span, typ: none(Symbol), metadata: initTable[string, Metadata](), reader: none(string), kind: fexprList, car: car, cdr: cdr)
proc flist*(span: Span, lst: varargs[FExpr]): FExpr =
  var cur = fnil(span)
  for val in lst:
    cur = fcons(span, val, cur)
  return cur.reverse()
proc farray*(span: Span, sons = newSeq[FExpr]()): FExpr =
  FExpr(span: span, typ: none(Symbol), metadata: initTable[string, Metadata](), reader: none(string), kind: fexprArray, sons: sons)
proc fmap*(span: Span): FExpr =
  FExpr(span: span, typ: none(Symbol), metadata: initTable[string, Metadata](), reader: none(string), kind: fexprMap, tbl: initOrderedTable[Name, FExpr]())

iterator items*(fexpr: FExpr): FExpr =
  case fexpr.kind
  of fexprList:
    var cur = fexpr
    while true:
      yield(cur.car)
      if cur.cdr.kind == fexprNil:
        break
      cur = cur.cdr
  of fexprArray:
    for son in fexpr.sons:
      yield(son)
  of fexprMap:
    for val in fexpr.tbl.values:
      yield(val)
  else:
    fexpr.error("$# shouldn't be use as container" % $fexpr.kind)
    
iterator mitems*(fexpr: FExpr): var FExpr =
  case fexpr.kind
  of fexprList:
    var cur = fexpr
    while true:
      yield(cur.car)
      if cur.cdr.kind == fexprNil:
        break
      cur = cur.cdr
  of fexprArray:
    for son in fexpr.sons.mitems:
      yield(son)
  of fexprMap:
    for val in fexpr.tbl.mvalues:
      yield(val)
  else:
    fexpr.error("$# shouldn't be use as container" % $fexpr.kind)

proc len*(fexpr: FExpr): int =
  case fexpr.kind
  of fexprList:
    var cnt = 0
    for val in fexpr:
      cnt.inc
    return cnt
  of fexprArray:
    return fexpr.sons.len
  of fexprMap:
    return fexpr.tbl.len*2
  else:
    return 0

proc addSon*(fexpr: FExpr, f: FExpr) =
  if fexpr.kind != fexprArray:
    fexpr.error("$# isn't farray" % $fexpr.kind)
  fexpr.sons.add(f)
proc `[]`*(fexpr: FExpr, i: int): var FExpr =
  case fexpr.kind
  of fexprList:
    var cnt = 0
    for val in fexpr.mitems:
      if i == cnt:
        return val
      cnt.inc
  of fexprArray:
    return fexpr.sons[i]
  else:
    fexpr.error("$# isn't container" % $fexpr.kind)
proc `[]`*(fexpr: FExpr, i: BackwardsIndex): var FExpr =
  fexpr[fexpr.len-int(i)]
proc `[]`*(fexpr: FExpr, sl: Slice[int]): FExpr =
  case fexpr.kind
  of fexprList:
    result = fnil(fexpr.span)
    for i in sl:
      result = fcons(fexpr.span, fexpr[i], result)
    result = result.reverse()
  of fexprArray:
    result = farray(fexpr.span)
    for i in sl:
      result.addSon(fexpr.sons[i])
  else:
    fexpr.error("$# isn't container" % $fexpr.kind)
proc `[]`*(fexpr: FExpr, sl: HSlice[int, BackwardsIndex]): FExpr =
  fexpr[sl.a..fexpr.len-int(sl.b)]
proc `[]=`*(fexpr: FExpr, i: int, f: FExpr) =
  case fexpr.kind
  of fexprList:
    var cnt = 0
    var cur = fexpr
    while true:
      if cnt == i:
        cur.car = f
        break
      if cur.cdr.kind == fexprNil:
        fexpr.error("index out of bounds")
      cnt.inc
      cur = cur.cdr
  of fexprArray:
    fexpr.sons[i] = f
  else:
    fexpr.error("$# isn't container" % $fexpr.kind)

proc `[]`*(fexpr: FExpr, key: Name): FExpr =
  if fexpr.kind != fexprMap:
    fexpr.error("$# isn't fmap" % $fexpr.kind)
  if not fexpr.tbl.hasKey(key):
    fexpr.error("$# fmap hasn't $# key." % [$fexpr, $key])
  return fexpr.tbl[key]
proc `[]=`*(fexpr: FExpr, key: Name, value: FExpr) =
  if fexpr.kind != fexprMap:
    fexpr.error("$# isn't fmap")
  fexpr.tbl[key] = value

iterator map*(fexpr: FExpr): (Name, FExpr) =
  case fexpr.kind
  of fexprMap:
    for key, val in fexpr.tbl:
      yield(key, val)
  else:
    fexpr.error("$# shouldn't be use as map" % $fexpr.kind)

proc reverse*(fexpr: FExpr): FExpr =
  if fexpr.kind != fexprList:
    fexpr.error("$# isn't flist" % $fexpr)
  var cur = fnil(fexpr.span)
  for val in fexpr:
    cur = fcons(cur.span, val, cur)
  return cur

proc genIndent*(indent: int): string =
  repeat(' ', indent)

proc toString*(fexpr: FExpr, indent: int): string =
  case fexpr.kind
  of fexprIdent:
    fexpr.ident
  of fexprAttr:
    ":" & fexpr.attr
  of fexprSymbol:
    $fexpr.symbol
  of fexprIntLit:
    $fexpr.intval
  of fexprStrLit:
    "\"" & fexpr.strval & "\""
  of fexprNil:
    "nil"
  of fexprList:
    if fexpr.reader.isSome:
      fexpr.reader.get & $fexpr.cdr.car
    else:
      "(" & toSeq(fexpr.items).mapIt($it).join(" ") & ")"
  of fexprArray:
    "[" & fexpr.sons.mapIt(it.toString(indent)).join(" ") & "]"
  of fexprMap:
    var s = newSeq[string]()
    for key, val in fexpr.tbl:
      s.add(":" & $key & " " & val.toString(indent))
    "{" & s.join(" ") & "}"

proc `$`*(fexpr: FExpr): string = fexpr.toString(0)
