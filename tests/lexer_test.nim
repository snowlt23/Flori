
import compiler.image, compiler.lexer
import unittest
import strutils, sequtils

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
    check tokens[2].kind == tokenLBlock
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
    check tokens[0].ident == "fib"
