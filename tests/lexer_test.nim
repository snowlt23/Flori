
import unittest
import strutils, sequtils
import compiler.fcore, compiler.internalpass

initFlori()

proc lex(src: string): seq[Token] =
  lex("testmodule.flori", src)

suite "F fxpression lexer test":
  test "block test":
    let tokens = lex("""
add5 =>
  x + 5
""")
    check tokens[0].kind == tokenIdent
    check tokens[1].kind == tokenInfix
    check tokens[2].kind == tokenNewline
    check tokens[3].kind == tokenLBlock
    check tokens[^1].kind == tokenRBlock
  test "fib test":
    let tokens = lex("""
fib =>
  if n<2
    n
  else
    fib(n-1) + fib(n-2)
""")
    check tokens[0].kind == tokenIdent
    check $tokens[0] == "fib"
    check $tokens[1] == "=>"
    check tokens[2].kind == tokenNewline
    check tokens[3].kind == tokenLBlock
    check tokens[4].kind == tokenIdent
    check tokens[5].kind == tokenIdent
    check tokens[6].kind == tokenInfix
    check tokens[7].kind == tokenIntLit
    check tokens[8].kind == tokenNewline
    check tokens[9].kind == tokenLBlock
    check tokens[10].kind == tokenIdent
    check tokens[11].kind == tokenRBlock
    check tokens[12].kind == tokenNewline
