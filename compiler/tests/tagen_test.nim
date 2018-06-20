
import ../fcore, ../passmacro, ../internalpass
import ../codegen/tacode, ../codegen/tagen

let prelude = """
type Bool $[internalsize 4] {}

fn `+(a IntLit, b IntLit) IntLit $[internalop "+"]
fn `-(a IntLit, b IntLit) IntLit $[internalop "-"]
fn `<(a IntLit, b IntLit) Bool $[internalop "<"]
"""

initRootScope()
var tactx = newTAContext()
let scope = newFScope("testmodule", "testmodule.flori")
scope.importFScope(internalScope.obj.name, internalScope)
proc evalTest*(src: string): seq[FExpr] =
  result = parseToplevel("testmodule.flori", src)
  for f in result.mitems:
    scope.rootPass(f)
    discard tactx.convertFExpr(f)
discard evalTest(prelude)

let fexprs = evalTest("""
fn add5(x IntLit) IntLit {
  a := x + 5
  a
}
fn sub5(x IntLit) IntLit {
  x - 5
}
add5(4)
sub5(9)
""")
  
for f in fexprs:
  echo f
echo "\n=>\n"

echo tactx

# var optctx = ctx.optimize()
# echo "Optimized => \n"
# echo optctx
# echo "codecount: ", ctx.countCode, " => ", optctx.countCode
