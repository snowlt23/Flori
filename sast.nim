
import strutils
import options

type
  SExprError* = object of Exception
  Span* = object
    line*: int
    linepos*: int
    pos*: int
    internal*: tuple[filename: string, line: int]
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

template internalSpan*(): Span =
  const internalname = instantiationInfo().filename
  const internalline = instantiationInfo().line
  Span(line: 0, linepos: 0, internal: (internalname, internalline))

proc newSNil*(span: Span): SExpr =
  new result
  result.span = span
  result.kind = sexprNil
proc newSList*(span: Span, first, rest: SExpr): SExpr =
  new result
  result.span = span
  result.kind = sexprList
  result.first = first
  result.rest = rest
proc newSIdent*(span: Span, id: string): SExpr =
  new result
  result.span = span
  result.kind = sexprIdent
  result.id = id
proc newSAttr*(span: Span, attr: string): SExpr =
  new result
  result.span = span
  result.kind = sexprAttr
  result.attr = attr
proc newSInt*(span: Span, x: int64): SExpr =
  new result
  result.span = span
  result.kind = sexprInt
  result.intval = x
proc newSInt*(span: Span, s: string): SExpr =
  newSInt(span, parseInt(s))
proc newSString*(span: Span, s: string): SExpr =
  new result
  result.span = span
  result.kind = sexprString
  result.strval = s

template each*(list: SExpr, name: untyped, body: untyped) =
  var curexpr = list
  template next() =
    curexpr = curexpr.rest
  while true:
    let name {.inject.} = curexpr
    body
    if curexpr.rest.kind == sexprNil:
      break
    next()

iterator items*(list: SExpr): SExpr =
  if list.kind != sexprList:
    raise newException(SExprError, "reverse: sexpr is not list")
  list.each(e):
    yield(e.first)
iterator pairs*(list: SExpr): tuple[i: int, e: SExpr] =
  if list.kind != sexprList:
    raise newException(SExprError, "reverse: sexpr is not list")
  var i = 0
  list.each(e):
    yield(i, e.first)
    i.inc

proc len*(list: SExpr): int =
  result = 0
  list.each(e):
    result.inc
proc reverse*(list: SExpr): SExpr =
  if list.kind != sexprList:
    raise newException(SExprError, "reverse: sexpr is not list")
  var newexpr = newSNil(list.span)
  list.each(e):
    newexpr = newSList(e.span, e.first, newexpr)
  return newexpr
proc last*(list: SExpr): SExpr =
  if list.kind != sexprList:
    raise newException(SExprError, "last: sexpr is not list")
  list.each(e):
    if e.rest.kind == sexprNil:
      return e.first
proc `last=`*(list: SExpr, val: SExpr) =
  if list.kind != sexprList:
    raise newException(SExprError, "last=: sexpr is not list")
  list.each(e):
    if e.rest.kind == sexprNil:
      e.rest = val
      break
proc getAttr*(list: SExpr, attr: string): Option[SExpr] =
  list.each(e):
    if e.first.kind == sexprAttr and e.first.attr == attr:
      return some(e.rest.first)
  return none(SExpr)
proc hasAttr*(list: SExpr, attr: string): bool =
  list.each(e):
    if e.first.kind == sexprAttr and e.first.attr == attr:
      return true
  return false

proc debug*(sexpr: SExpr): string =
  case sexpr.kind
  of sexprNil:
    "nil[$#:$#]" % [$sexpr.span.line, $sexpr.span.linepos]
  of sexprList:
    var strings = newSeq[string]()
    sexpr.each(e):
      strings.add(e.first.debug())
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
    var strings = newSeq[string]()
    sexpr.each(e):
      strings.add($e.first)
    "(" & strings.join(" ") & ")"
  of sexprIdent:
    sexpr.id
  of sexprAttr:
    ":" & sexpr.attr
  of sexprInt:
    $sexpr.intval
  of sexprString:
    "\"" & $sexpr.strval & "\""
