#!/bin/sh
objdump -b binary -D -m i386:x86-64 -M intel $1
