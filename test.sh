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

runtest "9" 9
