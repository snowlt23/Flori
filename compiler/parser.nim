
import image, fexpr
import strutils, options, algorithm

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

proc parseFExpr*(context: var ParserContext): Option[FExpr]

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
  result.filename = istring(context.filename)
  result.line = context.line
  result.linepos = context.linepos
  result.pos = context.pos
proc error*(context: ParserContext, msg: string) =
  raise newException(FParseError, "$#($#:$#) " % [context.filename, $context.line, $context.linepos] & msg)
proc getRangedSrc*(context: ParserContext, s: int, e: int): string =
  context.src[s..e]
  
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
  elif ident[0] in {'=', ':'}:
    (15, true)
  else:
    echo "internal message: unknown infix priority `", ident
    (16, true)

proc parseFExprElem*(context: var ParserContext): Option[FExpr] =
  if context.curchar == '#': # comment
    context.inc
    while not context.isEOF:
      if context.isNewline:
        break
      context.inc
    result = none(FExpr)
  elif context.curchar == '(':
    let span = context.span
    var arr = newSeq[FExpr]()
    context.inc
    context.skipSpaces()
    if context.curchar == ')':
      context.inc
      return some(flist(span, iarray[FExpr]()))
    let son = context.parseFExpr()
    if son.isSome:
      arr.add(son.get)
    while not context.isEOF:
      context.skipSpaces()
      if context.curchar != ',' and context.curchar == ')':
        context.inc
        break
      if context.curchar != ',':
        context.error("require separate comma.")
      context.inc
      let son = context.parseFExpr()
      if son.isSome:
        arr.add(son.get)
    result = some(flist(span, iarray(arr)))
    result.get.src = some(istring(context.getRangedSrc(span.pos, context.pos-1)))
  elif context.curchar == '[': # Array
    let span = context.span
    var arr = newSeq[FExpr]()
    context.inc
    context.skipSpaces()
    if context.curchar == ']':
      context.inc
      return some(farray(span, iarray[FExpr]()))
    let son = context.parseFExpr()
    if son.isSome:
      arr.add(son.get)
    while not context.isEOF:
      context.skipSpaces()
      if context.curchar != ',' and context.curchar == ']':
        context.inc
        break
      if context.curchar != ',':
        context.error("require separate comma.")
      context.inc
      let son = context.parseFExpr()
      if son.isSome:
        arr.add(son.get)
    result = some(farray(span, iarray(arr)))
    result.get.src = some(istring(context.getRangedSrc(span.pos, context.pos-1)))
  elif context.curchar == '{': # Block
    let span = context.span
    var arr = newSeq[FExpr]()
    context.inc
    context.skipSpaces()
    if context.curchar == '}':
      context.inc
      return some(fblock(span, iarray[FExpr]()))
    let son = context.parseFExpr()
    if son.isSome:
      arr.add(son.get)
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
      if son.isSome:
        arr.add(son.get)
    result = some(fblock(span, iarray(arr)))
    result.get.src = some(istring(context.getRangedSrc(span.pos, context.pos-1)))
  elif context.curchar in PrefixSymbols: # special ident
    var ident = ""
    let span = context.span()
    while not context.isEOF:
      if context.curchar notin SpecialSymbols:
        break
      ident.add(context.curchar)
      context.inc
    result = some(fprefix(span, ident))
  elif context.curchar in InfixSymbols: # special ident
    var ident = ""
    let span = context.span()
    while not context.isEOF:
      if context.curchar notin SpecialSymbols:
        break
      ident.add(context.curchar)
      context.inc
    let (pr, isleft) = getPriority(ident)
    result = some(finfix(span, ident, pr, isleft))
  elif context.curchar == '`': # quote
    let span = context.span()
    context.inc
    result = some(fquote(span, context.parseFExprElem().get))
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
      result = some(ffloatlit(span, parseFloat(s)))
    else:
      result = some(fintlit(span, parseInt(s)))
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
    result = some(fstrlit(span, s))
  else:
    let span = context.span()
    var ident = ""
    while not context.isEOF:
      if context.isSeparateSymbol or context.curchar == ' ' or context.curchar == '"':
        break
      ident.add(context.curchar)
      context.inc
    result = some(fident(span, ident))
  # else:
  #   context.error("couldn't parse F expression: $#" % $context.curchar)

proc polandToCall*(stack: seq[FExpr], pos: var int): FExpr =
  if stack.len <= pos:
    result = stack[pos-1]
  elif stack[pos].kind == fexprInfix:
    let infix = stack[pos]
    pos += 1
    let right = polandToCall(stack, pos)
    let left = polandToCall(stack, pos)
    result = fseq(infix.span, iarray([infix, left, right]))
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
        var arr = newSeq[FExpr]()
        while i < fexpr.len and fexpr[i].kind != fexprInfix:
          arr.add(fexpr[i])
          i += 1
        if arr.len == 1:
          stack.add(arr[0])
        else:
          let f = fseq(fexpr.span, iarray(arr))
          stack.add(f)
    for infix in infixstack.reversed():
      stack.add(infix)
    var pos = 0
    let ret = polandToCall(stack.reversed(), pos)
    ret.src = fexpr.src
    return ret
  else:
    return fexpr

proc parseFExpr*(context: var ParserContext): Option[FExpr] =
  context.skipSpaces()
  if context.isEOF:
    return none(FExpr)

  let span = context.span
  var arr = newSeq[FExpr]()
  while not context.isEndSymbol:
    context.skipSpaces()
    let elem = context.parseFExprElem()
    if elem.isSome:
      arr.add(elem.get)

  if arr.len == 0:
    return none(FExpr)
  else:
    let sq = fseq(span, iarray(arr))
    sq.src = some(istring(context.getRangedSrc(span.pos, context.pos)))
    return some(rewriteToCall(sq))

proc parseFExpr*(filename: string, src: string): FExpr =
  var context = newParserContext(filename, src)
  return parseFExpr(context).get
proc parseToplevel*(filename: string, src: string): seq[FExpr] =
  var context = newParserContext(filename, src)
  result = @[]
  while not context.isEOF:
    if context.curchar in EndList:
      context.error("couldn't parse F expression: $#" % $context.curchar)
    let ret = parseFExpr(context)
    if ret.isSome:
      result.add(ret.get)

proc expandQuote*(f: var FExpr, i: var int, span: Span, args: openArray[FExpr]) =
  f.obj.span = span
  if f.kind in fexprContainer:
    for son in f.mitems:
      expandQuote(son, i, span, args)
  elif f.kind == fexprQuote and $f.obj.quoted == "embed":
    f = args[i]
    i += 1
      
proc quoteFExpr*(span: Span, src: string, args: openArray[FExpr]): FExpr =
  result = parseFExpr("internal", src)
  var i = 0
  expandQuote(result, i, span, args)
