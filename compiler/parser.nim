
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

const StartList* = {'(', '['}
const EndList* = {')', ']', '}', ','}
const PrefixSymbols* = {'$', '&', '?'}
const InfixSymbols* = {'!', '$', '%', '&', '\'', '*', '+', '-', '.', '/', '<', '=', '>', '^', ':'}
const SeparateSymbols* = {'(', '[', ')', ']', '{', '}', ','} + PrefixSymbols + InfixSymbols

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
proc isEndSymbol*(context: ParserContext): bool =
  if context.curchar in EndList or context.isNewline:
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
    elif context.curchar == '#':
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

proc parseFExprElem*(context: var ParserContext): FExpr =
  if context.curchar == '(':
    var lst = flist(context.span)
    context.inc
    context.skipSpaces()
    if context.curchar != ')':
      lst.addSon(context.parseFExpr())
    while true:
      context.skipSpaces()
      if context.curchar != ',' and context.curchar == ')':
        context.inc
        break
      if context.curchar != ',':
        context.error("require separate comma.")
      context.inc
      lst.addSon(context.parseFExpr())
    return lst
  elif context.curchar == '[': # Array
    var arr = farray(context.span)
    context.inc
    context.skipSpaces()
    if context.curchar != ')':
      arr.addSon(context.parseFExpr())
    while true:
      context.skipSpaces()
      if context.curchar != ',' and context.curchar == ']':
        context.inc
        break
      if context.curchar != ',':
        context.error("require separate comma.")
      context.inc
      arr.addSon(context.parseFExpr())
    return arr
  elif context.curchar == '{': # Block
    var blk = fblock(context.span)
    context.inc
    context.skipSpaces()
    if context.curchar != '}':
      blk.addSon(context.parseFExpr())
    while true:
      context.skipSpaces()
      if context.curchar == '}':
        context.inc
        break
      blk.addSon(context.parseFExpr())
    return blk
  elif ('a' <= context.curchar and context.curchar <= 'z') or
       ('A' <= context.curchar and context.curchar <= 'Z'): # ident
    var ident = ""
    let span = context.span()
    while true:
      if context.isSeparateSymbol or context.curchar == ' ':
        break
      ident.add(context.curchar)
      context.inc
    return fident(span, ident)
  elif context.curchar in PrefixSymbols: # special ident
    var ident = ""
    let span = context.span()
    while true:
      if context.curchar notin PrefixSymbols + InfixSymbols:
        break
      ident.add(context.curchar)
      context.inc
    return fprefix(span, ident)
  elif context.curchar in InfixSymbols: # special ident
    var ident = ""
    let span = context.span()
    while true:
      if context.curchar notin PrefixSymbols + InfixSymbols:
        break
      ident.add(context.curchar)
      context.inc
    return finfix(span, ident)
  elif context.curchar == '`': # quote
    let span = context.span()
    context.inc
    return fquote(span, context.parseFExprElem())
  elif '0' <= context.curchar and context.curchar <= '9': # digit
    let span = context.span
    var s = ""
    while true:
      if not ('0' <= context.curchar and context.curchar <= '9'):
        break
      s &= context.curchar
      context.inc
    return fintlit(span, parseInt(s))
  elif context.curchar == '"':
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
    context.error("couldn't parse F expression")

proc rewriteToCall*(fexpr: FExpr): FExpr =
  if fexpr.kind == fexprSeq and fexpr.len == 1:
    return rewriteToCall(fexpr[0])
  elif fexpr.len == 2 and fexpr[1].kind == fexprList:
    return fcall(fexpr.span, fexpr[0], fexpr[1].sons)
  elif fexpr.kind == fexprSeq:
    let stack = fseq(fexpr.span)
    for i, son in fexpr.sons:
      if son.kind == fexprInfix:
        let left = rewriteToCall(stack)
        let right = rewriteToCall(fexpr[i+1..^1])
        return fcall(son.span, son, @[left, right])
      else:
        stack.addSon(son)
    return fexpr
  else:
    return fexpr

proc parseFExpr*(context: var ParserContext): FExpr =
  context.skipSpaces()
  if context.isEOF:
    return nil

  let sq = fseq(context.span)
  while not context.isEndSymbol:
    context.skipSpaces()
    sq.addSon(context.parseFExprElem())

  return rewriteToCall(sq)

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
