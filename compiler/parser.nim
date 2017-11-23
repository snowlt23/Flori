
import types
import fexpr
import strutils
import options

type
  FParseError* = object of Exception
  ParserContext* = object
    filename*: string
    src*: string
    line*: int
    linepos*: int
    pos*: int

const StartList* = {'(', '[', '{'}
const EndList* = {')', ']', '}'}
const SpecialSymbols* = {'!', '$', '%', '&', '\'', '*', '+', '-', '.', '/', '<', '=', '>', '^', '$', '&', '?'}
const SeparateSymbols* = StartList + EndList + {' '}

proc parseFExpr*(context: var ParserContext): FExpr

proc newParserContext*(filename: string, src: string): ParserContext =
  result.filename = filename
  result.src = src
  result.line = 1
  result.linepos = 1
  result.pos = 0
proc curchar*(context: ParserContext): char =
  return context.src[context.pos]
proc isEOF*(context: ParserContext): bool =
  context.curchar == 0x1a.char or context.curchar == '\0'
proc isNewline*(context: ParserContext): bool =
  let lf = 0x0a.char
  let cr = 0x0d.char
  if context.curchar == lf:
    return true
  elif context.src[context.pos] == cr and context.src[context.pos+1] == lf:
    return true
  else:
    return false
proc newlineLen*(context: ParserContext): int =
  let lf = 0x0a.char
  if context.curchar == lf:
    return 1
  else:
    return 2

proc isSeparateSymbol*(context: ParserContext): bool =
  if context.curchar in SeparateSymbols or context.isNewline:
    return true
  else:
    return false
proc inc*(context: var ParserContext) =
  context.linepos += 1
  context.pos += 1
proc skipSpaces*(context: var ParserContext) =
  while true:
    if context.curchar == ' ':
      context.inc
    elif context.isNewLine:
      context.line += 1
      context.linepos = 1
      context.pos += context.newlineLen()
    elif context.curchar == ';':
      context.inc
      while true:
        if context.isNewline:
          context.line += 1
          context.linepos = 1
          context.pos += context.newlineLen()
          break
        context.inc
    else:
      break
proc span*(context: ParserContext): Span =
  result.filename = context.filename
  result.line = context.line
  result.linepos = context.linepos
  result.pos = context.pos
proc error*(context: ParserContext, msg: string) =
  raise newException(FParseError, "$#($#:$#) " % [context.filename, $context.line, $context.linepos] & msg)

proc parseFExpr*(context: var ParserContext): FExpr =
  context.skipSpaces()
  if context.isEOF:
    return nil

  if context.curchar == '^': # type reader
    context.inc
    let span = context.span
    result = flist(context.span, fident(span, "type"), context.parseFExpr())
    result.reader = some("^")
  elif context.curchar == '&': # mut reader
    context.inc
    let span = context.span
    result = flist(context.span, fident(span, "mut"), context.parseFExpr())
    result.reader = some("&")
  elif context.curchar == '$': # pragma reader
    context.inc
    let span = context.span
    result = flist(span, fident(span, "pragma"), context.parseFExpr())
    result.reader = some("$")
  elif context.curchar == '(': # List
    var lst = fnil(context.span)
    context.inc
    while true:
      context.skipSpaces()
      if context.curchar == ')':
        context.inc
        break
      lst = fcons(lst.span, context.parseFExpr(), lst)
    return lst.reverse()
  elif context.curchar == '[': # Array
    var arr = farray(context.span)
    context.inc
    while true:
      context.skipSpaces()
      if context.curchar == ']':
        context.inc
        break
      arr.addSon(context.parseFExpr())
    return arr
  elif context.curchar == '{': # Map
    var map = fmap(context.span)
    context.inc
    while true:
      context.skipSpaces()
      if context.curchar == '}':
        context.inc
        break
      let key = context.parseFExpr()
      if key.kind != fexprAttr:
        key.error("fmap key should be fattr, actually $#" % $key)
      map[name(key.attr)] = context.parseFExpr()
    return map
  elif context.curchar == ':': # attr
    context.inc
    var attr = ""
    let span = context.span()
    while true:
      if context.isSeparateSymbol or context.curchar == ' ':
        break
      attr.add(context.curchar)
      context.inc
    return fattr(span, attr)
  elif ('a' <= context.curchar and context.curchar <= 'z') or
       ('A' <= context.curchar and context.curchar <= 'Z') or
       context.curchar in SpecialSymbols: # ident
    var ident = ""
    let span = context.span()
    while true:
      if context.isSeparateSymbol or context.curchar == ' ':
        break
      ident.add(context.curchar)
      context.inc
    return fident(span, ident)
  elif '0' <= context.curchar and context.curchar <= '9': # intlit
    let span = context.span
    var s = ""
    while true:
      if not ('0' <= context.curchar and context.curchar <= '9'):
        break
      s &= context.curchar
      context.inc
    return fintlit(span, parseInt(s))
  elif context.curchar == '"': # strlit
    var s = ""
    let span = context.span
    context.inc
    while true:
      if context.curchar == '"':
        context.inc
        break
      s &= context.curchar
      context.inc
    return fstrlit(span, s)
  else:
    context.error("couldn't parse F expression: $#" % $context.curchar)

proc parseFExpr*(filename: string, src: string): FExpr =
  var context = newParserContext(filename, src)
  return parseFExpr(context)
proc parseToplevel*(filename: string, src: string): seq[FExpr] =
  var context = newParserContext(filename, src)
  result = @[]
  while true:
    let ret = parseFExpr(context)
    if ret == nil:
      break
    else:
      result.add(ret)
