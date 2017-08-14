
import sast
import strutils

type
  SParseError* = object of Exception
  ParserContext* = object
    filename*: string
    src*: string
    line*: int
    linepos*: int
    pos*: int

const StartList* = {'(', '['}
const EndList* = {')', ']'}
const SpecialSymbols* = {'!', '#', '$', '%', '&', '\'', '*', '+', '-', '.', '/', '<', '=', '>', '?', '^'}
const SeparateSymbols* = {'(', '[', ')', ']', ' '}

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

proc parseSExpr*(context: var ParserContext): SExpr =
  context.skipSpaces()

  if context.isEOF():
    return nil

  if context.curchar in StartList: # list
    let span = context.span()
    var list = newSNil(span)
    context.inc
    while true:
      context.skipSpaces()
      if context.curchar in EndList:
        context.inc
        break
      let res = parseSExpr(context)
      list = newSList(span, res, list)
    return list.reverse()
  elif context.curchar == '@': # @ annotation syntax
    context.inc
    let annot = parseSExpr(context)
    let body = parseSExpr(context)
    var ret = annot
    if annot.kind == sexprList:
      ret.last = newSList(body.span, body, newSNil(body.span))
    else:
      ret = newSList(annot.span, annot, newSList(body.span, body, newSNil(body.span)))
    return ret
  elif context.curchar == '&': # & Ref type syntax
    let span = context.span
    context.inc
    let typ = parseSExpr(context)
    return newSList(span, newSIdent(span, "Ref"), newSList(span, typ, newSNil(span)))
  elif ('a' <= context.curchar and context.curchar <= 'z') or
       ('A' <= context.curchar and context.curchar <= 'Z') or
       context.curchar in SpecialSymbols: # ident
    var name = ""
    let span = context.span()
    while true:
      if context.isSeparateSymbol:
        break
      name &= context.curchar
      context.inc
    return newSIdent(span, name)
  elif ':' == context.curchar: # attr
    var name = ""
    let span = context.span()
    context.inc
    while true:
      if context.isSeparateSymbol:
        break
      name &= context.curchar
      context.inc
    return newSAttr(span, name)
  elif '0' <= context.curchar and context.curchar <= '9': # digit
    let span = context.span
    var s = ""
    while true:
      if not ('0' <= context.curchar and context.curchar <= '9'):
        break
      s &= context.curchar
      context.inc
    return newSInt(span, s)
  elif context.curchar == '"': # string
    var s = ""
    let span = context.span
    context.inc
    while true:
      if context.curchar == '"':
        context.inc
        break
      s &= context.curchar
      context.inc
    return newSString(span, s)
  else:
    raise newException(SParseError, "($#:$#) couldn't parse s expression" % [$context.line, $context.linepos])

proc parseSExpr*(filename: string, src: string): SExpr =
  var context = newParserContext(filename, src)
  return parseSExpr(context)
proc parseToplevel*(filename: string, src: string): seq[SExpr] =
  var context = newParserContext(filename, src)
  result = @[]
  while true:
    let ret = parseSExpr(context)
    if ret == nil:
      break
    else:
      result.add(ret)
