#!/bin/bash

# CORELIBS="core/prelude.flori core/cstring.flori core/macro.flori core/syntax.flori core/allocator.flori core/string.flori core/asm.flori"
CORELIBS="core/prelude.flori core/cstring.flori core/macro.flori core/allocator.flori core/quote.flori core/syntax.flori core/storage.flori core/stringbuffer.flori core/module.flori core/functor.flori core/array.flori core/pointer.flori core/exception.flori"
# CORELIBS="core/prelude.flori"

read-coresrc() {
  for f in $CORELIBS ; do
    cat $f
    echo ""
  done
}

# $1=testname
unittest() {
  echo "[UNIT] $1"
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
  echo "[TEST] $1"
  PRELUDE=`read-coresrc`
  OUT=`echo "$PRELUDE; $1" | ./bin/flori`
  if [ "$2" != "$OUT" ] ; then
    echo "[ERROR] $1: expect $2, but got $OUT"
    exit 1
  fi
}

filetest() {
  echo "[TEST] $1"
  PRELUDE=`read-coresrc`
  FILE=`cat "$1"`
  OUT=`echo "$PRELUDE; $FILE" | ./bin/flori`
  if [ "$2" != "$OUT" ] ; then
    echo "[ERROR] $1: expect $2, but got $OUT"
    exit 1
  fi
}

exectest() {
  echo "[EXEC] $1"
  PRELUDE=`read-coresrc`
  FILE=`cat "$1"`
  echo "$PRELUDE; $FILE" | ./bin/flori -o fa.out
  PR=`./fa.out`
  OUT=$?
  if [ "$2" != "$PR$OUT" ] ; then
    echo "[ERROR] exec $1: expect $2, but got $PR$OUT"
    exit 1
  fi
}

httpcheck() {
  for ((i=0; i < 5; i++)) ; do
    STATUS=`curl -s $1 -o /dev/null -w '%{http_code}'`
    if [ "$STATUS" = "$2" ] ; then
      return
    fi
    sleep 1
  done
  echo "[ERROR] failed curlcheck to $1, expect $2"
  exit 1
}

unittest "linmem_test"
unittest "jit_test"
unittest "fmap_test"

runtest "fn main() { 9 }" 9
runtest "fn main() {4}" 4
runtest "fn main() {5}" 5
runtest "fn main() {555}" 555
runtest "fn main() {0xa}" 10
runtest "fn main() {0xFF}" 255
runtest "fn main() {0xFF; 45}" 45
runtest "fn main() {45; X(0x58); X(0x48); X(0x89); X(0xEC); X(0x5D); X(0xC3)}" 45
runtest "fn main() {4 + 5}" 9
runtest "fn add5(x ^int) ^int {x + 5}; fn main() {add5(7)}" 12
runtest "fn main() {return 555}" 555
runtest "fn main() { var yyy ^int yyy = 1515 yyy }" 1515
runtest "fn main() {ott := 123; ott}" 123
runtest "fn main() { if 1 4 }" 4
runtest "fn main() { if 1 2 else 3 }" 2
runtest "fn main() { if 0 2 else 3 }" 3
runtest "fn id(p ^ptr int) ^ptr int { p }; fn main() {9}" 9
runtest "fn main() {s := \"yukarisan\"; 9}" 9
runtest "fn main() {s := \"ia\"; *cast_ptr(s)}" 105
runtest "fn main() {s := \"yukayuka\"; len(s)}" 8
runtest "struct myint {a ^int; b ^int}; fn main() {5}" 5
runtest "fn main() {5 == 5}" 1
runtest "fn main() {4 == 5}" 0
runtest "fn main() {strcmp(\"abcdgogo\", \"abcdgogo\")}" 1
runtest "fn main() {strcmp(\"abcdgogo\", \"abcdgog\")}" 0
runtest "fn main() {internal_print(5454) 0}" "54540"
runtest "macro idmac(f ^fmap) ^fmap {f} fn main() {idmac(5)}" 5
runtest "macro fiv() ^fmap {fintlit(5)} fn main() {fiv()}" 5
runtest "gx := 555; fn main() {gx}" 555

filetest "examples/fib.flori" 34
filetest "examples/sizeof.flori" 24
filetest "examples/pointer.flori" 1000
filetest "examples/struct_field.flori" 9
filetest "examples/struct_ptr.flori" 13
# filetest "examples/struct_copy.flori" 9
# filetest "examples/struct_value.flori" 9
# filetest "examples/struct_result.flori" 9

#
# language library test
#

filetest "examples/sysprint.flori" "yukarisan0"
filetest "examples/while.flori" "aaaaaaaaaa0"
filetest "examples/cstring.flori" "akari0"
# filetest "examples/string.flori" 3
filetest "examples/macro.flori" "!@Hello Yukari!9"
filetest "examples/syntax.flori" "YUKARIMAKIAKARI123456789100"

#
# extend library test
#

# filetest "examples/asm.flori" "55"
filetest "examples/storage.flori" "9"

HTTP_REQ=`cat <<EOF
GET /index.html HTTP/1.1
Host: www.example.com
Connection: close
0
EOF`
filetest "examples/http.flori" "$HTTP_REQ" &
httpcheck localhost:4545 200

filetest "examples/module.flori" "9"
filetest "examples/functor.flori" "9"

filetest "examples/exception.flori" "Error5"

#
# executable test
#

exectest "examples/fib.flori" 0
exectest "examples/exitfib.flori" 34
exectest "examples/sysprint.flori" "yukarisan0"
exectest "examples/globalvar.flori" 45
# exectest "examples/memory_allocate.flori" 123
exectest "examples/while.flori" "aaaaaaaaaa0"
