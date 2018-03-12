
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
const EndList* = {')', ']', '}', ',', ';'}
const PrefixSymbols* = {'$', '?', '@'}
const InfixSymbols* = {'.', '!', '%', '+', '-', '*', '/', '<', '=', '>', ':', '|', '&'}
const SpecialSymbols* = PrefixSymbols + InfixSymbols
const SeparateSymbols* = StartList + EndList + SpecialSymbols

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
  context.curchar == 0x1a.char or context.curchar == '\0' or context.src.len <= context.pos
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
  if context.curchar in EndList or context.isNewline or context.src.len <= context.pos:
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
      while not context.isEOF:
        if context.isNewline:
          context.line += 1
          context.linepos = 1
          context.pos += context.newlineLen()
          break
        context.inc
    else:
      break
proc span*(context: ParserContext): Span =
  new result
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
    if context.curchar == ')':
      context.inc
      return lst
    lst.addSon(context.parseFExpr())
    while not context.isEOF:
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
    if context.curchar == ']':
      context.inc
      return arr
    arr.addSon(context.parseFExpr())
    while not context.isEOF:
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
    if context.curchar == '}':
      context.inc
      return blk
    blk.addSon(context.parseFExpr())
    while not context.isEOF:
      context.skipSpaces()
      if context.curchar == '}':
        context.inc
        break
      elif context.curchar in {',', ';'}:
        context.inc
        continue
      blk.addSon(context.parseFExpr())
    return blk
  elif ('a' <= context.curchar and context.curchar <= 'z') or
       ('A' <= context.curchar and context.curchar <= 'Z'): # ident
    let span = context.span()
    var idents = newSeq[string]()
    var curstr = ""
    while not context.isEOF:
      if context.curchar == '/':
        context.inc
        idents.add(curstr)
        curstr = ""
      elif context.isSeparateSymbol or context.curchar == ' ':
        break
      curstr.add(context.curchar)
      context.inc
    if curstr != "":
      idents.add(curstr)
    return fident(span, name(idents))
  elif context.curchar in PrefixSymbols: # special ident
    var ident = ""
    let span = context.span()
    while not context.isEOF:
      if context.curchar notin SpecialSymbols:
        break
      ident.add(context.curchar)
      context.inc
    return fprefix(span, name(ident))
  elif context.curchar in InfixSymbols: # special ident
    var ident = ""
    let span = context.span()
    while not context.isEOF:
      if context.curchar notin SpecialSymbols:
        break
      ident.add(context.curchar)
      context.inc
    return finfix(span, name(ident))
  elif context.curchar == '`': # quote
    let span = context.span()
    context.inc
    return fquote(span, context.parseFExprElem())
  elif '0' <= context.curchar and context.curchar <= '9': # intlit
    let span = context.span
    var s = ""
    while not context.isEOF:
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
      elif context.curchar == '\\':
        s.add(context.curchar)
        context.inc
        s.add(context.curchar)
        context.inc
        continue
      elif context.curchar == 0x0d.char:
        context.inc
        continue
      s &= context.curchar
      context.inc
    return fstrlit(span, s)
  else:
    context.error("couldn't parse F expression")

proc rewriteToCall*(fexpr: FExpr): FExpr =
  if fexpr.kind == fexprSeq and fexpr.len == 1:
    return rewriteToCall(fexpr[0])
  elif fexpr.kind == fexprSeq:
    let stack = fseq(fexpr.span)
    var i = 0
    while i < fexpr.len:
      if fexpr[i].kind == fexprInfix:
        let left = rewriteToCall(stack)
        let right = rewriteToCall(fexpr[i+1..^1])
        return fseq(fexpr[i].span, @[fexpr[i], left, right])
      else:
        stack.addSon(fexpr[i])
        i.inc
    if stack.len == 1:
      return stack[0]
    else:
      return stack
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

proc expandQuote*(f: var FExpr, i: var int, span: Span, args: openArray[FExpr]) =
  f.span = span
  if f.kind in fexprContainer:
    for son in f.mitems:
      expandQuote(son, i, span, args)
  elif f.kind == fexprQuote and $f.quoted == "embed":
    f = args[i]
    i += 1
      
proc quoteFExpr*(span: Span, src: string, args: openArray[FExpr]): FExpr =
  result = parseFExpr("internal", src)
  var i = 0
  expandQuote(result, i, span, args)
