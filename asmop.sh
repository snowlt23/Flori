#!/bin/sh

echo ".intel_syntax noprefix" >> asmop_tmp.s
echo $@ >> asmop_tmp.s
gcc -c -o asmop_tmp.o asmop_tmp.s
objdump -d -M intel asmop_tmp.o | tail -n +8

rm asmop_tmp.s
rm asmop_tmp.o
