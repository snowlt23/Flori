# $1=testname
unittest() {
  make $1.out
  ./$1.out
  RETCODE=$?
  if [ "$RETCODE" != "0" ] ; then
    echo "[ERROR] $1"
    exit 1
  fi
}

# $1=input $2=expect
runtest() {
  echo "$1" | ./bin/flori > tmp.s
  gcc -o a.out tmp.s
  ./a.out
  RETCODE=$?
  if [ "$2" != "$RETCODE" ] ; then
    echo "[ERROR] $1: expect $2, but got $RETCODE"
    exit 1
  fi
}

unittest "jit_test"

runtest "9" 9
