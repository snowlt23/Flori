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

unittest "test/vector_test.c"

lexertest "yukari" "TOKEN_IDENT"
lexertest "yukari maki" "TOKEN_IDENT TOKEN_IDENT"
lexertest "4 + 5" "TOKEN_INTLIT TOKEN_OP TOKEN_INTLIT"
