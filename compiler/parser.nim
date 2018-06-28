
import image, fexpr, lexer
import strutils, options, algorithm

type
  ParseError* = object of Exception
  ParserContext* = object
    tokens*: seq[Token]
    pos*: int

proc topexpr(ctx: var ParserContext): FExpr

proc newParserContext*(tokens: seq[Token]): ParserContext =
  ParserContext(tokens: tokens, pos: 0)
proc isEnd*(ctx: ParserContext): bool =
  ctx.pos >= ctx.tokens.len
proc getToken*(ctx: var ParserContext, rel = 0): Option[Token] =
  if ctx.pos+rel >= ctx.tokens.len:
    none(Token)
  else:
    some(ctx.tokens[ctx.pos+rel])
proc nextToken*(ctx: var ParserContext) =
  ctx.pos += 1
proc error*(token: Token, msg: string) =
  raise newException(ParseError, "$#($#:$#) " % [$token.span.filename, $token.span.line, $token.span.linepos] & msg)
proc error*(ctx: ParserContext, msg: string) =
  if ctx.tokens.len == 0:
    raise newException(ParseError, "undef: " & msg)
  else:
    raise newException(ParseError, "$#($#:$#) " % [$ctx.tokens[^1].span.filename, $ctx.tokens[^1].span.line, $ctx.tokens[^1].span.linepos] & msg)

template defineInfixExpr(name, call, pri) =
  proc name(ctx: var ParserContext): FExpr =
    var f = ctx.call()
    while true:
      let tok = ctx.getToken()
      if tok.isNone:
        break
      if tok.get.kind == tokenInfix and tok.get.priority == pri:
        ctx.nextToken()
        f = finfix(tok.get.span, fident(tok.get.span, tok.get.infix), f, ctx.call())
      else:
        break
    return f

proc factor(ctx: var ParserContext): FExpr =
  let tok = ctx.getToken
  if tok.isNone:
    ctx.error("required more token.")
  if tok.get.kind == tokenLParen:
    ctx.nextToken()
    let f = ctx.topexpr()
    let rtok = ctx.getToken()
    if rtok.isNone or rtok.get.kind != tokenRParen:
      tok.get.error("unmatched RParen.")
    return f
  elif tok.get.kind == tokenIdent:
    ctx.nextToken()
    return fident(tok.get.span, tok.get.ident)
  elif tok.get.kind == tokenIntLit:
    ctx.nextToken()
    return fintlit(tok.get.span, tok.get.intval)
  else:
    return ctx.topexpr()

proc callexpr(ctx: var ParserContext): FExpr =
  let call = ctx.factor()
  let tok = ctx.getToken()
  if tok.isSome and tok.get.kind == tokenLParen:
    ctx.nextToken()
    var args = newSeq[FExpr]()
    if ctx.getToken().isSome and ctx.getToken().get.kind == tokenRParen:
      ctx.nextToken()
      return fcall(tok.get.span, call, args)
    while true:
      args.add(ctx.topexpr())
      let next = ctx.getToken()
      if next.isNone:
        ctx.error("required more token.")
      if next.get.kind == tokenComma:
        ctx.nextToken()
        continue
      elif next.get.kind == tokenRParen:
        ctx.nextToken()
        return fcall(tok.get.span, call, args)
      else:
        tok.get.error("exptected Comma, or RParen) by function call, but $#" % $next.get)
  else:
    return call

defineInfixExpr(infix4, callexpr, 4)
defineInfixExpr(infix5, infix4, 5)
defineInfixExpr(infix7, infix5, 7)
defineInfixExpr(infix15, infix7, 15)

proc blockexpr(ctx: var ParserContext): FExpr =
  let tok = ctx.getToken()
  if tok.isNone:
    ctx.error("required more token.")
  if tok.get.kind == tokenLBlock:
    ctx.nextToken()
    var sons = newSeq[FExpr]()
    while true:
      sons.add(ctx.infix15())
      let next = ctx.getToken()
      if next.isNone:
        ctx.error("require more token by block.")
      if next.get.kind == tokenRBlock:
        ctx.nextToken()
        return fblock(tok.get.span, sons)
  else:
    return ctx.infix15()

proc topexpr(ctx: var ParserContext): FExpr =
  ctx.blockexpr()

proc parseFExpr*(filename: string, src: string): FExpr =
  var lexer = newLexerContext(filename, src)
  var parser = newParserContext(lexer.lex())
  return parser.topexpr()
