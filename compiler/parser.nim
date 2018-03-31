
import types
import fexpr
import strutils
import options
import algorithm

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
  if context.curchar == lf or context.curchar == cr:
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
  
proc getPriority*(ident: string): (int, bool) =
  if ident[0] in {'<', '>'}:
    (7, true)
  elif ident.len >= 2 and ident[0] in {'!', '='} and ident[^1] in {'='}:
    (8, true)
  elif ident[^1] in {'=', ':'}:
    (15, false)
  elif ident[0] in {'&', '|'}:
    (12, true)
  elif ident[0] in {'+', '-', '!'}:
    (5, true)
  elif ident[0] in {'*', '/', '%'}:
    (4, true)
  elif ident[0] in {'.'}:
    (1, true)
  else:
    echo "internal message: unknown infix priority `", ident
    (16, true)

proc parseFExprElem*(context: var ParserContext): FExpr =
  if context.curchar == '#': # comment
    context.inc
    while not context.isEOF:
      if context.isNewline:
        break
      context.inc
    return nil
  elif context.curchar == '(':
    var lst = flist(context.span)
    context.inc
    context.skipSpaces()
    if context.curchar == ')':
      context.inc
      return lst
    let son = context.parseFExpr()
    if not son.isNil:
      lst.addSon(son)
    while not context.isEOF:
      context.skipSpaces()
      if context.curchar != ',' and context.curchar == ')':
        context.inc
        break
      if context.curchar != ',':
        context.error("require separate comma.")
      context.inc
      let son = context.parseFExpr()
      if not son.isNil:
        lst.addSon(son)
    return lst
  elif context.curchar == '[': # Array
    var arr = farray(context.span)
    context.inc
    context.skipSpaces()
    if context.curchar == ']':
      context.inc
      return arr
    let son = context.parseFExpr()
    if not son.isNil:
      arr.addSon(son)
    while not context.isEOF:
      context.skipSpaces()
      if context.curchar != ',' and context.curchar == ']':
        context.inc
        break
      if context.curchar != ',':
        context.error("require separate comma.")
      context.inc
      let son = context.parseFExpr()
      if not son.isNil:
        arr.addSon(son)
    return arr
  elif context.curchar == '{': # Block
    var blk = fblock(context.span)
    context.inc
    context.skipSpaces()
    if context.curchar == '}':
      context.inc
      return blk
    let son = context.parseFExpr()
    if not son.isNil:
      blk.addSon(son)
    while not context.isEOF:
      context.skipSpaces()
      if context.curchar == '}':
        context.inc
        break
      elif context.curchar == ';':
        context.inc
        continue
      elif context.isEndSymbol:
        context.error("unmatch container symbol: $#" % $context.curchar)
      let son = context.parseFExpr()
      if not son.isNil:
        blk.addSon(son)
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
    let (pr, isleft) = getPriority(ident)
    return finfix(span, name(ident), pr, isleft)
  elif context.curchar == '`': # quote
    let span = context.span()
    context.inc
    return fquote(span, context.parseFExprElem())
  elif '0' <= context.curchar and context.curchar <= '9': # digit
    let span = context.span
    var s = ""
    var isfloat = false
    while not context.isEOF:
      if context.curchar == '.':
        isfloat = true
      elif not ('0' <= context.curchar and context.curchar <= '9'):
        break
      s &= context.curchar
      context.inc
    if isfloat:
      return ffloatlit(span, parseFloat(s))
    else:
      return fintlit(span, parseInt(s))
  elif context.curchar == '"': # strlit
    var s = ""
    let span = context.span
    context.inc
    while true:
      let lf = 0x0a.char
      if context.curchar == lf:
        context.line += 1
        context.linepos = 1
        
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
    context.error("couldn't parse F expression: $#" % $context.curchar)

proc polandToCall*(stack: seq[FExpr], pos: var int): FExpr =
  if stack[pos].kind == fexprInfix:
    let infix = stack[pos]
    pos += 1
    let right = polandToCall(stack, pos)
    let left = polandToCall(stack, pos)
    result = fseq(infix.span, @[infix, left, right])
  else:
    result = stack[pos]
    pos += 1

proc isInfixSeq*(f: FExpr): bool =
  for son in f:
    if son.kind == fexprInfix:
      return true

proc rewriteToCall*(fexpr: FExpr): FExpr =
  if fexpr.kind == fexprSeq and fexpr.len == 1:
    return rewriteToCall(fexpr[0])
  elif fexpr.kind == fexprSeq:
    if not fexpr.isInfixSeq:
      return fexpr
      
    var stack = newSeq[FExpr]()
    var infixstack = newSeq[FExpr]()
    var i = 0
    while i < fexpr.len:
      let son = fexpr[i]
      if son.kind == fexprInfix:
        while true:
          if infixstack.len == 0 or (son.priority < infixstack[^1].priority) or ((not son.isleft) and son.priority <= infixstack[^1].priority):
            infixstack.add(son)
            break
          stack.add(infixstack[^1])
          infixstack.del(high(infixstack))
        i += 1
      else:
        let f = fseq(fexpr[i].span)
        while i < fexpr.len and fexpr[i].kind != fexprInfix:
          f.addSon(fexpr[i])
          i += 1
        if f.len == 1:
          stack.add(f[0])
        else:
          stack.add(f)
    for infix in infixstack.reversed():
      stack.add(infix)
    var pos = 0
    return polandToCall(stack.reversed(), pos)
  else:
    return fexpr

proc parseFExpr*(context: var ParserContext): FExpr =
  context.skipSpaces()
  if context.isEOF:
    return nil

  let sq = fseq(context.span)
  while not context.isEndSymbol:
    context.skipSpaces()
    let elem = context.parseFExprElem()
    if not elem.isNil:
      sq.addSon(elem)

  if sq.len == 0:
    return nil
  else:
    return rewriteToCall(sq)

proc parseFExpr*(filename: string, src: string): FExpr =
  var context = newParserContext(filename, src)
  return parseFExpr(context)
proc parseToplevel*(filename: string, src: string): seq[FExpr] =
  var context = newParserContext(filename, src)
  result = @[]
  while not context.isEOF:
    let ret = parseFExpr(context)
    if not ret.isNil:
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
