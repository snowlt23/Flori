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
  OUT=`echo $1 | ./lexertest.out`
  OUT=`echo $OUT`
  if [ "$OUT" = "$2" ] ; then
    echo "[OK] lexer: $1"
  else
    echo "[ERROR] lexer: $1, expected $2, but got $OUT"
    exit 1
  fi
}

parsertest() {
  OUT=`echo $1 | ./parsertest.out`
  OUT=`echo $OUT`
  if [ "$OUT" = "$2" ] ; then
    echo "[OK] parser: $1"
  else
    echo "[ERROR] parser: $1, expected $2, but got $OUT"
    exit 1
  fi
}

unittest "test/vector_test.c"

lexertest "yukari" "TOKEN_IDENT:yukari"
lexertest "yukari maki" "TOKEN_IDENT:yukari TOKEN_IDENT:maki"
lexertest "4 + 5" "TOKEN_INTLIT:4 TOKEN_OP:+ TOKEN_INTLIT:5"

parsertest "9" "FEXPR_INTLIT"
parsertest "11 + 22" "FEXPR_INFIX"
