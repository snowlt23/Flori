
import image
import strutils, sequtils
import options

type
  TokenKind* = enum
    tokenIdent
    tokenInfix
    tokenIntLit
    tokenStrLit
    tokenQuote
    tokenComma
    tokenLParen
    tokenRParen
    tokenLBlock
    tokenRBlock
    tokenNewline
  Token* = object
    span*: Span
    case kind*: TokenKind
    of tokenIdent:
      ident*: string
    of tokenInfix:
      infix*: string
      priority*: int
      leftconc*: bool
    of tokenIntLit:
      intval*: int64
    of tokenStrLit:
      strval*: string
    of tokenQuote: discard
    of tokenComma: discard
    of tokenLParen: discard
    of tokenRParen: discard
    of tokenLBlock: discard
    of tokenRBlock: discard
    of tokenNewline: discard
  LexerContext* = object
    src*: string
    filename*: string
    line*: int
    linepos*: int
    pos*: int
  LexerError* = object of Exception

const InfixSymbols* = {'.', '!', '%', '+', '-', '*', '/', '<', '=', '>', ':', '|', '&'}
const SeparateSymbols* = InfixSymbols + {'(', ')', ',', ';', '`', ' '}

proc `$`*(token: Token): string =
  case token.kind
  of tokenIdent:
    token.ident
  of tokenInfix:
    token.infix
  of tokenIntLit:
    $token.intval
  of tokenStrLit:
    "\"" & token.strval & "\""
  of tokenQuote:
    "`"
  of tokenComma:
    ","
  of tokenLParen:
    "("
  of tokenRParen:
    ")"
  of tokenLBlock:
    "{"
  of tokenRBlock:
    "}"
  of tokenNewline:
    "\\n"

proc newLexerContext*(filename: string, src: string): LexerContext =
  result.filename = filename
  result.src = src
  result.line = 1
  result.linepos = 1
  result.pos = 0
proc curchar*(context: LexerContext): char =
  return context.src[context.pos]
proc inc*(context: var LexerContext) =
  context.linepos += 1
  context.pos += 1
proc isEOF*(context: LexerContext): bool =
  context.curchar == 0x1a.char or context.curchar == '\0' or context.src.len <= context.pos
proc isNewline*(context: LexerContext): bool =
  let lf = 0x0a.char
  let cr = 0x0d.char
  if context.curchar == lf or context.curchar == cr:
    return true
  else:
    return false
proc newlineLen*(context: LexerContext): int =
  let lf = 0x0a.char
  if context.curchar == lf:
    return 1
  else:
    return 2
proc span*(context: LexerContext): Span =
  result.filename = istring(context.filename)
  result.line = context.line
  result.linepos = context.linepos
  result.pos = context.pos
proc error*(context: LexerContext, msg: string) =
  raise newException(LexerError, "$#($#:$#) " % [context.filename, $context.line, $context.linepos] & msg)

proc isNumber*(c: char): bool =
  '0' <= c and c <= '9'

proc isSeparateSymbol*(context: LexerContext): bool =
  if context.curchar in SeparateSymbols or context.isNewline:
    return true
  else:
    return false
proc getPriority*(ident: string): (int, bool) =
  if ident[0] in {'<', '>'}:
    (7, true)
  elif ident.len >= 2 and ident[0] in {'!', '='} and ident[^1] in {'='}:
    (8, true)
  elif ident[^1] in {'=', ':'}:
    (14, false)
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
    echo "internal message: unknown infix priority `$#`" % ident
    (16, true)

proc skipComment*(ctx: var LexerContext) =
  while not ctx.isEOF:
    if ctx.isNewline:
      ctx.line += 1
      ctx.linepos = 1
      ctx.pos += ctx.newlinelen
      return
    ctx.pos.inc
proc skipSpaces*(ctx: var LexerContext): bool =
  while not ctx.isEOF:
    if ctx.isNewline:
      ctx.line += 1
      ctx.linepos = 1
      ctx.pos += ctx.newlinelen
      return true
    elif ctx.curchar == '#':
      ctx.skipComment()
      return true
    elif ctx.curchar == ' ':
      ctx.pos.inc
    else:
      return false

proc getindent*(ctx: var LexerContext): Option[int] =
  var prevline = ctx.line
  var prevlinepos = ctx.linepos
  var prevpos = ctx.pos
  var curr = 0
  while not ctx.isEOF:
    if ctx.isNewline or ctx.curchar == '#':
      ctx.line = prevline
      ctx.linepos = prevlinepos
      ctx.pos = prevpos
      return none(int)
    elif ctx.curchar == ' ':
      ctx.inc
      curr += 1
    else:
      return some(curr)

proc lex*(ctx: var LexerContext): seq[Token] =
  result = @[]
  var innewline = true
  var curindent = 0
  while not ctx.isEOF:
    if innewline:
      let indent = ctx.getindent()
      if indent.isSome:
        if curindent != indent.get:
          if curindent < indent.get:
            result.add(Token(span: ctx.span, kind: tokenLBlock))
          elif curindent > indent.get:
            var lastnewline = none(Token)
            if result[result.high].kind == tokenNewline:
              lastnewline = some(result[result.high])
              result.del(result.high)
            for i in 0..<(curindent - indent.get) div 2:
              result.add(Token(span: ctx.span, kind: tokenRBlock))
            if lastnewline.isSome:
              result.add(lastnewline.get)
          curindent = indent.get
        innewline = false
        continue
      else:
        innewline = false
        continue

    if ctx.skipSpaces():
      innewline = true
      result.add(Token(span: ctx.span, kind: tokenNewline))
      continue

    if ctx.curchar == '`':
      result.add(Token(span: ctx.span, kind: tokenQuote))
      ctx.inc
    elif ctx.curchar == ',':
      result.add(Token(span: ctx.span, kind: tokenComma))
      ctx.inc
    elif ctx.curchar == '"':
      let span = ctx.span
      ctx.inc
      var s = ""
      while ctx.curchar != '"':
        if ctx.isEOF:
          ctx.error("unmatched `\"` strlit.")
        s.add(ctx.curchar)
        ctx.inc
      ctx.inc
      result.add(Token(span: span, kind: tokenStrLit, strval: s))
    elif ctx.curchar == '(':
      result.add(Token(span: ctx.span, kind: tokenLParen))
      ctx.inc
    elif ctx.curchar == ')':
      result.add(Token(span: ctx.span, kind: tokenRParen))
      ctx.inc
    elif ctx.curchar.isNumber:
      var num = ""
      while ctx.curchar.isNumber:
        num.add(ctx.curchar)
        ctx.inc
      result.add(Token(span: ctx.span, kind: tokenIntLit, intval: parseInt(num)))
    elif ctx.curchar in InfixSymbols:
      let span = ctx.span
      var infix = ""
      while (not ctx.isEOF) and ctx.curchar in InfixSymbols:
        infix.add(ctx.curchar)
        ctx.inc
      let (priority, leftconc) = getPriority(infix)
      result.add(Token(span: span, kind: tokenInfix, infix: infix, priority: priority, leftconc: leftconc))
    else:
      let span = ctx.span
      var ident = ""
      while (not ctx.isEOF) and (not ctx.isSeparateSymbol):
        ident.add(ctx.curchar)
        ctx.inc
      result.add(Token(span: span, kind: tokenIdent, ident: ident))

  for i in 0..<(curindent - 0) div 2:
    result.add(Token(span: ctx.span, kind: tokenRBlock))

proc lex*(filename: string, src: string): seq[Token] =
  var ctx = newLexerContext(filename, src)
  return ctx.lex()
