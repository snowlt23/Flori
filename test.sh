#!/bin/bash

# CORELIBS="core/prelude.flori core/cstring.flori core/macro.flori core/syntax.flori core/allocator.flori core/string.flori core/asm.flori"
# CORELIBS="core/prelude.flori"
CORELIBS="core/new_prelude.flori core/cstring.flori"

read-coresrc() {
  for f in $CORELIBS ; do
    cat $f
    echo ""
  done
}

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
  PRELUDE=`read-coresrc`
  OUT=`echo "$PRELUDE; $1" | ./bin/flori`
  if [ "$2" != "$OUT" ] ; then
    echo "[ERROR] $1: expect $2, but got $OUT"
    exit 1
  fi
}

filetest() {
  PRELUDE=`read-coresrc`
  FILE=`cat "$1"`
  OUT=`echo "$PRELUDE; $FILE" | ./bin/flori`
  if [ "$2" != "$OUT" ] ; then
    echo "[ERROR] $1: expect $2, but got $OUT"
    exit 1
  fi
}

exectest() {
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
# runtest "gx := 555; fn main() {gx}" 555

filetest "examples/fib.flori" 34
filetest "examples/sizeof.flori" 24
filetest "examples/pointer.flori" 1000
filetest "examples/struct_field.flori" 9
filetest "examples/struct_ptr.flori" 13
# filetest "examples/struct_copy.flori" 9
# filetest "examples/struct_value.flori" 9
# filetest "examples/struct_result.flori" 9
filetest "examples/sysprint.flori" "yukarisan0"
filetest "examples/while.flori" "aaaaaaaaaa0"
# filetest "examples/cstring.flori" "akari0"
# filetest "examples/string.flori" 3
# filetest "examples/macro.flori" "!@DDHello Yukari!9"
# filetest "examples/asm.flori" "55"
# filetest "examples/syntax.flori" "123456789100"

# exectest "examples/fib.flori" 0
# exectest "examples/exitfib.flori" 34
# exectest "examples/sysprint.flori" "yukarisan0"
# exectest "examples/globalvar.flori" 45
# exectest "examples/memory_allocate.flori" 123
# exectest "examples/while.flori" "aaaaaaaaaa0"
