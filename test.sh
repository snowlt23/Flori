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

unittest "test/vector_test.c"
unittest "test/token_test.c"
