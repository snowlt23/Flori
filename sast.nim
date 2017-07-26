
import strutils
import options

type
  SExprError* = object of Exception
  Span* = object
    line*: int
    linepos*: int
    pos*: int
  SExprKind* = enum
    sexprNil
    sexprList
    sexprIdent
    sexprAttr
    sexprInt
    sexprString
  SExpr* = ref object
    span*: Span
    case kind*: SExprKind
    of sexprNil:
      discard
    of sexprList:
      first*: SExpr
      rest*: SExpr
    of sexprIdent:
      id*: string
    of sexprAttr:
      attr*: string
    of sexprInt:
      intval*: int64
    of sexprString:
      strval*: string

template ast*(s: Span, sexpr: typed): untyped =
  let tmp = sexpr
  tmp.span = s
  tmp
proc newSNil*(): SExpr =
  new result
  result.kind = sexprNil
proc newSList*(first, rest: SExpr): SExpr =
  new result
  result.kind = sexprList
  result.first = first
  result.rest = rest
proc newSIdent*(id: string): SExpr =
  new result
  result.kind = sexprIdent
  result.id = id
proc newSAttr*(attr: string): SExpr =
  new result
  result.kind = sexprAttr
  result.attr = attr
proc newSInt*(x: int64): SExpr =
  new result
  result.kind = sexprInt
  result.intval = x
proc newSInt*(s: string): SExpr =
  newSInt(parseInt(s))
proc newSString*(s: string): SExpr =
  new result
  result.kind = sexprString
  result.strval = s

iterator items*(list: SExpr): SExpr =
  if list.kind != sexprList:
    raise newException(SExprError, "reverse: sexpr is not list")
  var curexpr = list
  while true:
    yield(curexpr.first)
    if curexpr.rest.kind == sexprNil:
      break
    curexpr = curexpr.rest
iterator pairs*(list: SExpr): tuple[i: int, e: SExpr] =
  if list.kind != sexprList:
    raise newException(SExprError, "reverse: sexpr is not list")
  var curexpr = list
  var i = 0
  while true:
    yield(i, curexpr.first)
    if curexpr.rest.kind == sexprNil:
      break
    curexpr = curexpr.rest
    i.inc
iterator list*(list: SExpr): SExpr =
  if list.kind != sexprList:
    raise newException(SExprError, "reverse: sexpr is not list")
  var curexpr = list
  while true:
    yield(curexpr)
    if curexpr.rest.kind == sexprNil:
      break
    curexpr = curexpr.rest

proc len*(list: SExpr): int =
  result = 0
  var curexpr = list
  while true:
    result.inc
    if curexpr.rest.kind == sexprNil:
      break
    curexpr = curexpr.rest
proc reverse*(list: SExpr): SExpr =
  if list.kind != sexprList:
    raise newException(SExprError, "reverse: sexpr is not list")
  var newexpr = newSNil()
  var curexpr = list
  while true:
    newexpr = newSList(curexpr.first, newexpr)
    if curexpr.rest.kind == sexprNil:
      break
    curexpr = curexpr.rest
  return ast(list.span, newexpr)
proc last*(list: SExpr): SExpr =
  var curexpr = list
  while true:
    if curexpr.rest.kind == sexprNil:
      return curexpr.first
    curexpr = curexpr.rest
proc `last=`*(list: SExpr, val: SExpr) =
  var curexpr = list
  while true:
    if curexpr.rest.kind == sexprNil:
      curexpr.rest = val
      break
    curexpr = curexpr.rest
proc getAttr*(list: SExpr, attr: string): Option[SExpr] =
  var curexpr = list
  while true:
    if curexpr.first.kind == sexprAttr and curexpr.first.attr == attr:
      return some(curexpr.rest.first)
    if curexpr.rest.kind == sexprNil:
      return none(SExpr)
    curexpr = curexpr.rest
proc hasAttr*(list: SExpr, attr: string): bool =
  var curexpr = list
  while true:
    if curexpr.first.kind == sexprAttr and curexpr.first.attr == attr:
      return true
    if curexpr.rest.kind == sexprNil:
      return false
    curexpr = curexpr.rest

proc debug*(sexpr: SExpr): string =
  case sexpr.kind
  of sexprNil:
    "nil[$#:$#]" % [$sexpr.span.line, $sexpr.span.linepos]
  of sexprList:
    var curexpr = sexpr
    var strings = newSeq[string]()
    while true:
      strings.add(curexpr.first.debug())
      if curexpr.rest.kind == sexprNil:
        strings.add(curexpr.rest.debug())
        break
      curexpr = curexpr.rest
    "list[$#:$#]($#)" % [$sexpr.span.line, $sexpr.span.linepos, strings.join(", ")]
  of sexprIdent:
    "ident[$#:$#]($#)" % [$sexpr.span.line, $sexpr.span.linepos, sexpr.id]
  of sexprAttr:
    "attr[$#:$#($#)" % [$sexpr.span.line, $sexpr.span.linepos, sexpr.attr]
  of sexprInt:
    "int[$#:$#]($#)" % [$sexpr.span.line, $sexpr.span.linepos, $sexpr.intval]
  of sexprString:
    "string[$#:$#]($#)" % [$sexpr.span.line, $sexpr.span.linepos, $sexpr.strval]

proc `$`*(sexpr: SExpr): string =
  case sexpr.kind
  of sexprNil:
    "nil"
  of sexprList:
    var curexpr = sexpr
    var strings = newSeq[string]()
    while true:
      strings.add($curexpr.first)
      if curexpr.rest.kind == sexprNil:
        break
      curexpr = curexpr.rest
    "(" & strings.join(" ") & ")"
  of sexprIdent:
    sexpr.id
  of sexprAttr:
    ":" & sexpr.attr
  of sexprInt:
    $sexpr.intval
  of sexprString:
    "\"" & $sexpr.strval & "\""
