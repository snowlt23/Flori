# $1=testname
unittest() {
  M=`make $1.out`
  ./$1.out
  RETCODE=$?
  if [ "$RETCODE" != "0" ] ; then
    echo "[ERROR] $1"
    exit 1
  fi
}

# $1=input $2=expect
runtest() {
  OUT=`echo "$1" | ./bin/flori`
  if [ "$2" != "$OUT" ] ; then
    echo "[ERROR] $1: expect $2, but got $OUT"
    exit 1
  fi
}

unittest "linmem_test"
unittest "jit_test"

runtest "fn main 9" 9
runtest "fn main 4" 4
runtest "fn main 5" 5
runtest "fn main 555" 555
