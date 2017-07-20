
import strutils
import options

type
  SExprError* = object of Exception
  Span* = object
    line*: int
    linepos*: int
    pos*: int
  Symbol* = object
    hash*: string
  SExprKind* = enum
    sexprNil
    sexprList
    sexprIdent
    sexprInt
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
    of sexprInt:
      intval*: int64

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
proc newSInt*(x: int64): SExpr =
  new result
  result.kind = sexprInt
  result.intval = x
proc newSInt*(s: string): SExpr =
  newSInt(parseInt(s))

proc `$`*(symbol: Symbol): string =
  symbol.hash

iterator items*(list: SExpr): SExpr =
  if list.kind != sexprList:
    raise newException(SExprError, "reverse: sexpr is not list")
  var curexpr = list
  while true:
    yield(curexpr.first)
    if curexpr.rest.kind == sexprNil:
      break
    curexpr = curexpr.rest
iterator list*(list: SExpr): SExpr =
  if list.kind != sexprList:
    raise newException(SExprError, "reverse: sexpr is not list")
  var curexpr = list
  while true:
    yield(curexpr)
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
  return newexpr
proc `last=`*(list: SExpr, val: SExpr) =
  var curexpr = list
  while true:
    if curexpr.rest.kind == sexprNil:
      curexpr.rest = val
      break
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
  of sexprInt:
    "int[$#:$#]($#)" % [$sexpr.span.line, $sexpr.span.linepos, $sexpr.intval]

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
  of sexprInt:
    $sexpr.intval
