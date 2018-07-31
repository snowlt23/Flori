
import image, fexpr, lexer
import strutils, options, algorithm

type
  ParseError* = object of Exception
  MoreToken* = object of Exception
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
proc moreToken*(ctx: var ParserContext) =
  raise newException(MoreToken, "require more token.")
proc error*(token: Token, msg: string) =
  raise newException(ParseError, "$#($#:$#) " % [$token.span.filename, $token.span.line, $token.span.linepos] & msg)
proc error*(ctx: ParserContext, msg: string) =
  if ctx.tokens.len == 0:
    raise newException(ParseError, "undef: " & msg)
  else:
    raise newException(ParseError, "$#($#:$#) " % [$ctx.tokens[^1].span.filename, $ctx.tokens[^1].span.line, $ctx.tokens[^1].span.linepos] & msg)

proc isEndSymbol*(tok: Token): bool =
  tok.kind in {tokenComma, tokenRParen, tokenRBlock} or $tok == "elif" or $tok == "else"

template defineInfixExpr(name, call, pri) =
  proc name(ctx: var ParserContext): FExpr =
    if ctx.getToken().isSome and ctx.getToken().get.kind == tokenInfix and ctx.getToken().get.priority == pri:
      let tok = ctx.getToken()
      ctx.nextToken()
      return fprefix(tok.get.span, fident(tok.get.span, tok.get.infix), ctx.call())
    else:
      var f = ctx.call()
      while true:
        let tok = ctx.getToken()
        if tok.isNone:
          break
        if tok.get.kind == tokenInfix and tok.get.priority == pri:
          ctx.nextToken()
          if tok.get.leftconc:
            let right = ctx.call()
            if right.kind == fexprBlock and right.sons.len == 0:
              ctx.moreToken()
            f = finfix(tok.get.span, fident(tok.get.span, tok.get.infix), f, right)
          else:
            f = finfix(tok.get.span, fident(tok.get.span, tok.get.infix), f, ctx.topexpr())
        else:
          break
      return f

proc factor(ctx: var ParserContext): FExpr =
  let tok = ctx.getToken
  if tok.isNone:
    ctx.moreToken()
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
  elif tok.get.kind == tokenStrLit:
    ctx.nextToken()
    return fstrlit(tok.get.span, tok.get.strval)
  elif tok.get.kind == tokenNewline:
    ctx.nextToken()
    return ctx.topexpr()
  elif tok.get.isEndSymbol:
    tok.get.error("unexpected $# token." % $tok.get)
  else:
    return ctx.topexpr()

proc quoteexpr(ctx: var ParserContext): FExpr =
  let tok = ctx.getToken()
  if tok.isSome and tok.get.kind == tokenQuote:
    ctx.nextToken()
    let id = ctx.getToken()
    if id.isNone:
      ctx.moreToken()
    ctx.nextToken()
    return fquote(tok.get.span, $id.get)
  else:
    return ctx.factor()

proc callexpr(ctx: var ParserContext): FExpr =
  let call = ctx.quoteexpr()
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
        ctx.moreToken()
      if next.get.kind == tokenComma:
        ctx.nextToken()
        continue
      elif next.get.kind == tokenRParen:
        ctx.nextToken()
        return fcall(tok.get.span, call, args)
      else:
        next.get.error("exptected Comma, or RParen) by function call, but $#" % $next.get)
  else:
    return call

proc whileexpr(ctx: var ParserContext): FExpr =
  let tok = ctx.getToken()
  if tok.isSome and $tok.get == "while":
    ctx.nextToken()
    let whilebranch = ctx.topexpr()
    if whilebranch.kind != fexprInfix or $whilebranch.call != ":":
      whilebranch.error("while branch expect colon`:` token.")
    return fwhile(tok.get.span, whilebranch)
  else:
    return ctx.callexpr()

proc ifexpr(ctx: var ParserContext): FExpr =
  let tok = ctx.getToken()
  if tok.isSome and $tok.get == "if":
    ctx.nextToken()
    let ifbranch = ctx.topexpr()
    if ifbranch.kind != fexprInfix or $ifbranch.call != ":":
      ifbranch.error("if branch expect colon`:` token.")
    var elifbranches = newSeq[FExpr]()
    var elsebody = fcall(tok.get.span, fident(tok.get.span, "discard"), [])
    while true:
      let nexttok = ctx.getToken()
      if nexttok.isNone:
        ctx.moreToken()
      if nexttok.isSome and nexttok.get.kind == tokenNewline:
        ctx.nextToken()
        continue
      elif nexttok.isSome and $nexttok.get == "elif":
        ctx.nextToken()
        let elifbranch = ctx.topexpr()
        if elifbranch.kind != fexprInfix or $elifbranch.call != ":":
          elifbranch.error("elif branch expect colon`:` token.")
        elifbranches.add(elifbranch)
      elif nexttok.isSome and $nexttok.get == "else":
        ctx.nextToken()
        let colon = ctx.getToken()
        if colon.isNone or $colon.get != ":":
          nexttok.get.error("else branch expect colon`:` token.")
        ctx.nextToken()
        elsebody = ctx.topexpr()
        break
      else:
        break
    return fif(tok.get.span, ifbranch, elifbranches, elsebody)
  else:
    return ctx.whileexpr()

defineInfixExpr(infix1, ifexpr, 1)
defineInfixExpr(infix2, infix1, 2)
defineInfixExpr(infix3, infix2, 3)
defineInfixExpr(infix4, infix3, 4)
defineInfixExpr(infix5, infix4, 5)
defineInfixExpr(infix6, infix5, 6)
defineInfixExpr(infix7, infix6, 7)
defineInfixExpr(infix8, infix7, 8)
defineInfixExpr(infix12, infix8, 12)
defineInfixExpr(infix14, infix12, 14)

proc blockexpr(ctx: var ParserContext): FExpr =
  let tok = ctx.getToken()
  if tok.isNone:
    ctx.moreToken()
  if tok.get.kind == tokenLBlock:
    ctx.nextToken()
    var sons = newSeq[FExpr]()
    while true:
      let son = ctx.infix14()
      if not (son.kind == fexprBlock and son.sons.len == 0):
        sons.add(son)
      let next = ctx.getToken()
      if next.isNone:
        ctx.moreToken()
      if next.get.kind == tokenRBlock:
        ctx.nextToken()
        return fblock(tok.get.span, sons)
  else:
    var exprs = newSeq[FExpr]()
    while true:
      let tok = ctx.getToken()
      if tok.isNone:
        break
      if tok.get.kind == tokenNewline:
        ctx.nextToken()
        let next = ctx.getToken()
        if next.isNone:
          break
        if next.get.kind == tokenLBlock:
          let bexpr = ctx.blockexpr()
          for b in bexpr.sons:
            exprs.add(b)
        break
      if tok.isNone:
        break
      if tok.get.isEndSymbol:
        break
      let son = ctx.infix14()
      exprs.add(son)
      if son.kind == fexprBlock and son.sons.len == 0:
        continue

      let next = ctx.getToken()
      if next.isNone:
        break
      if next.get.kind in {tokenInfix}:
        break
    if exprs.len == 1:
      return exprs[0]
    else:
      return fblock(tok.get.span, exprs)
defineInfixExpr(infix15, blockexpr, 15)

proc topexpr(ctx: var ParserContext): FExpr =
  ctx.infix15()

proc parseFExpr*(filename: string, src: string): FExpr =
  var lexer = newLexerContext(filename, src)
  var parser = newParserContext(lexer.lex())
  return parser.topexpr()
proc parseToplevel*(filename: string, src: string): seq[FExpr] =
  result = @[]
  var lexer = newLexerContext(filename, src)
  var parser = newParserContext(lexer.lex())
  while not parser.isEnd:
    let e = parser.topexpr()
    if e.kind == fexprBlock and e.sons.len == 0:
      continue
    result.add(e)

proc embed*(fexpr: var FExpr, span: Span, i: var int, args: openArray[FExpr]) =
  if fexpr.kind == fexprQuote and $fexpr.quoted == "embed":
    fexpr = args[i]
    i.inc
  elif fexpr.kind in fexprCalls:
    fexpr.call.embed(span, i, args)
    for arg in fexpr.args.mitems:
      arg.embed(span, i, args)
  elif fexpr.kind == fexprBlock:
    for son in fexpr.sons.mitems:
      son.embed(span, i, args)

proc quoteFExpr*(span: Span, src: string, args: openArray[FExpr]): FExpr =
  result = parseFExpr("internal.flori", src)
  var i = 0
  result.embed(span, i, args)
