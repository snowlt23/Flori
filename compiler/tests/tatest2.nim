
import ../image, ../parser, ../fexpr
import ../codegen/tacode, ../codegen/tagen, ../codegen/taopt

let fexprs = parseToplevel("test.flori", """
fn fib(x Int) Int {
  if (x < 2) {
    x
  } else {
    fib(x-1) + fib(x-2)
  }
}

fib(30)
""")

var ctx = newTAContext()
for f in fexprs:
  discard ctx.convertFExpr(f)
  
for f in fexprs:
  echo f
echo "\n=>\n"

echo ctx

var optctx = ctx.optimize()
echo "Optimized => \n"
echo optctx
echo "codecount: ", ctx.countCode, " => ", optctx.countCode
