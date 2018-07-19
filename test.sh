unittest() {
  cp $1 tmp/`basename $1`
  gcc -Wall -o tmp/test.out tmp/`basename $1`
  ./tmp/test.out
  RETCODE=$?
  if [ $RETCODE = 0 ] ; then
    echo "[OK] unittest: $1"
  else
    echo "[ERROR] unittest: $1"
    exit 1
  fi
}

lexertest() {
  OUT=`printf "$1" | ./lexertest.out`
  OUT=`echo $OUT`
  if [ "$OUT" = "$2" ] ; then
    echo "[OK] lexer: $1"
  else
    echo "[ERROR] lexer: $1, expected $2, but got $OUT"
    exit 1
  fi
}

parsertest() {
  OUT=`printf "$1" | ./parsertest.out`
  OUT=`echo $OUT`
  if [ "$OUT" = "$2" ] ; then
    echo "[OK] parser: $1"
  else
    echo "[ERROR] parser: $1, expected $2, but got $OUT"
    exit 1
  fi
}

rettest() {
  printf "$1" | ./bin/flori > out.asm
  nasm -felf64 out.asm
  ld -o test.out out.o
  ./test.out
  RETCODE=$?
  if [ $RETCODE = $2 ] ; then
    echo "[OK] retcode: $1"
  else
    echo "[ERROR] retcode: $1, expected $2, but got $RETCODE"
    exit 1
  fi
}

unittest "test/vector_test.c"

lexertest "yukari" "TOKEN_IDENT:yukari"
lexertest "yukari maki" "TOKEN_IDENT:yukari TOKEN_IDENT:maki"
lexertest "4 + 5" "TOKEN_INTLIT:4 TOKEN_OP:+ TOKEN_INTLIT:5"
lexertest "(4 + 5)" "TOKEN_LPAREN TOKEN_INTLIT:4 TOKEN_OP:+ TOKEN_INTLIT:5 TOKEN_RPAREN"
lexertest "(4 + 5) * 2" "TOKEN_LPAREN TOKEN_INTLIT:4 TOKEN_OP:+ TOKEN_INTLIT:5 TOKEN_RPAREN TOKEN_OP:* TOKEN_INTLIT:2"
lexertest "a, b, c" "TOKEN_IDENT:a TOKEN_COMMA TOKEN_IDENT:b TOKEN_COMMA TOKEN_IDENT:c"
lexertest "main => 1" "TOKEN_IDENT:main TOKEN_OP:=> TOKEN_INTLIT:1"
lexertest "main =>\n  1" "TOKEN_IDENT:main TOKEN_OP:=> TOKEN_LBLOCK TOKEN_INTLIT:1 TOKEN_RBLOCK"
lexertest "main =>\n  1\n    2" "TOKEN_IDENT:main TOKEN_OP:=> TOKEN_LBLOCK TOKEN_INTLIT:1 TOKEN_LBLOCK TOKEN_INTLIT:2 TOKEN_RBLOCK TOKEN_RBLOCK"

parsertest "9" "FEXPR_INTLIT"
parsertest "11 + 22" "FEXPR_INFIX"
parsertest "11 - 22" "FEXPR_INFIX"
parsertest "4 * 5" "FEXPR_INFIX"
parsertest "10 / 5" "FEXPR_INFIX"
parsertest "main =>\n  1" "FEXPR_INFIX"
parsertest "num5()" "FEXPR_CALL"
parsertest "add(4, 5)" "FEXPR_CALL"
parsertest "num4() + num4()" "FEXPR_INFIX"

rettest "main => 64" 64
rettest "main => 4 + 5" 9
rettest "main => 10-5" 5
rettest "main => 4*5" 20
rettest "main => 10/5" 2
rettest "main => 4 + 5 * 2" 14
rettest "main => (4 + 5) * 2" 18
rettest "main =>\n  nine := 9\n  nine + nine" 18
rettest "num9 => 4 + 5\nmain => num9() + num9()" 18
