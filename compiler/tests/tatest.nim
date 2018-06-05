
import ../image, ../parser, ../fexpr
import ../codegen/tacode, ../codegen/tagen, ../codegen/taopt

let fexprs = parseToplevel("test.flori", """
fn add5(x Int) Int {
  x + 5
}

fn main() {
a := 1
if (add5(a)) {
  a = 2
  1 + 1
} elif (b) {
  2 + 2
} else {
  3 + 3
}

while(a) {
  1 + 1
  1 + 1
  1 / 2
}
4 + 5
1 - 1 + 6
}
main()
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
