#!/bin/bash

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
  PRELUDE=`cat core/prelude.flori`
  OUT=`echo "$PRELUDE; $1" | ./bin/flori`
  if [ "$2" != "$OUT" ] ; then
    echo "[ERROR] $1: expect $2, but got $OUT"
    exit 1
  fi
}

filetest() {
  OUT=`cat core/prelude.flori "$1" | ./bin/flori`
  if [ "$2" != "$OUT" ] ; then
    echo "[ERROR] $1: expect $2, but got $OUT"
    exit 1
  fi
}

unittest "linmem_test"
unittest "jit_test"

runtest "fn main() 9" 9
runtest "fn main() 4" 4
runtest "fn main() {5}" 5
runtest "fn main() 555" 555
runtest "fn main() {555}" 555
runtest "fn main() 0xa" 10
runtest "fn main() 0xFF" 255
runtest "fn main() {0xFF}" 255
runtest "fn main() {0xFF; 45}" 45
runtest "fn main() {45; X 0x58; X 0x48; X 0x89; X 0xEC; X 0x5D; X 0xC3}" 45
runtest "fn main() {return 555}" 555
runtest "fn main() {add 4 5}" 9
runtest "fn main() {set ott 123; ott}" 123
runtest "fn add5(x) {add x 5}; fn main() {add5 7}" 12
runtest "fn main() {4 + 5}" 9
runtest "fn main() { if 1 2 else 3 }" 2
runtest "fn main() { if 0 2 else 3 }" 3