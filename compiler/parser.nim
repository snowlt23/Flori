
import sast
import strutils

type
  FParseError* = object of Exception
  ParserContext* = object
    filename*: string
    src*: string
    line*: int
    linepos*: int
    pos*: int

const StartList* = {'(', '['}
const EndList* = {')', ']'}
const SpecialSymbols* = {'!', '#', '$', '%', '&', '\'', '*', '+', '-', '.', '/', '<', '=', '>', '?', '^'}
const SeparateSymbols* = {'(', '[', ')', ']', '{', '}', ',', ' '}

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

# TODO:
proc parseFExpr*(context: var ParserContext): SExpr = discard

proc parseSExpr*(filename: string, src: string): SExpr =
  var context = newParserContext(filename, src)
  return parseFExpr(context)
proc parseToplevel*(filename: string, src: string): seq[SExpr] =
  var context = newParserContext(filename, src)
  result = @[]
  while true:
    let ret = parseFExpr(context)
    if ret == nil:
      break
    else:
      result.add(ret)
