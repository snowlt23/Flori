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

rettest() {
  echo $1 | ./bin/flori > out.asm
  nasm -felf64 out.asm
  ld -o test.out out.o
  ./test.out
  RETCODE=$?
  if [ $RETCODE = $2 ] ; then
    echo "[OK] retcode: $1"
  else
    echo "[ERROR] retcode: $1, expected $2, but got $RETCODE"
  fi
}

unittest "test/vector_test.c"

lexertest "yukari" "TOKEN_IDENT:yukari"
lexertest "yukari maki" "TOKEN_IDENT:yukari TOKEN_IDENT:maki"
lexertest "4 + 5" "TOKEN_INTLIT:4 TOKEN_OP:+ TOKEN_INTLIT:5"

parsertest "9" "FEXPR_INTLIT"
parsertest "11 + 22" "FEXPR_INFIX"

rettest "64" 64
rettest "4 + 5" 9
