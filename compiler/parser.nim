
import image, fexpr, lexer
import strutils, options, algorithm

type
  ParseError* = object of Exception
  ParserContext* = object
    tokens*: seq[Token]
    pos*: int

proc factor(ctx: var ParserContext): FExpr

proc newParserContext*(tokens: seq[Token]): ParserContext =
  ParserContext(tokens: tokens, pos: 0)
proc isEnd*(ctx: ParserContext): bool =
  ctx.pos >= ctx.tokens.len
proc getToken*(ctx: var ParserContext): Option[Token] =
  if ctx.isEnd:
    result = none(Token)
  else:
    result = some(ctx.tokens[ctx.pos])
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

defineInfixExpr(infix4, factor, 4)
defineInfixExpr(infix5, infix4, 5)

proc topexpr*(ctx: var ParserContext): FExpr =
  ctx.infix5()

proc factor(ctx: var ParserContext): FExpr =
  let tok = ctx.getToken
  if tok.isNone:
    ctx.error("required more token.")
  ctx.nextToken()
  if tok.get.kind == tokenLParen:
    let f = ctx.topexpr()
    let rtok = ctx.getToken()
    if rtok.isNone or rtok.get.kind != tokenRParen:
      tok.get.error("unmatched RParen.")
    return f
  elif tok.get.kind == tokenIdent:
    return fident(tok.get.span, tok.get.ident)
  elif tok.get.kind == tokenIntLit:
    return fintlit(tok.get.span, tok.get.intval)
  else:
    tok.get.error("unexpected token $#" % $tok.get)

proc parseFExpr*(filename: string, src: string): FExpr =
  var lexer = newLexerContext(filename, src)
  var parser = newParserContext(lexer.lex())
  return parser.topexpr()
