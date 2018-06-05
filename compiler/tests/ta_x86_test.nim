
import ../image, ../parser, ../fexpr
import ../codegen/tacode, ../codegen/tagen, ../codegen/taopt, ../codegen/ta_x86

import tables
import os, osproc, strutils

# let fexprs = parseToplevel("test.flori", """
# fn fib(x Int) Int {
#   if (x < 2) {
#     x
#   } else {
#     fib(x-1) + fib(x-2)
#   }
# }
# fib(30)
# """)
let fexprs = parseToplevel("test.flori", """
fn add5(x Int) Int {
  x + 5
}
fn sub5(x Int) Int {
  x - 5
}
add5(4)
sub5(14)
""")

var ctx = newTAContext()
for f in fexprs:
  discard ctx.convertFExpr(f)
# ctx = ctx.optimize()

var asmctx = newAsmContext(newSeq[uint8]())
for i, c in ctx.codes:
  if ctx.revlabels.hasKey(i):
    asmctx.addLabel(ctx.revlabels[i])
  asmctx.generateFromTACode(c)
var bin = ""
for b in asmctx.buffer:
  bin.add(cast[char](b))
writeFile("fib.bin", bin)

echo ctx
echo "=>\n"
let outp = execProcess("objdump -b binary -M intel -m i386 -D fib.bin")
echo outp.split("\n")[6..^1].join("\n")
