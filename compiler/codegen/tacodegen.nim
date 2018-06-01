
import ../types, ../parser, ../fexpr, ../metadata
import tacode

import options
import strutils

proc convertFExpr*(ctx: var TAContext, fexpr: FExpr): TAAtom =
  if fexpr.kind == fexprIntLit:
    return atomInt(fexpr.intval)
  elif fexpr.kind == fexprIdent:
    return atomVar($fexpr)
  elif fexpr.kind == fexprSymbol:
    return atomVar(desc(fexpr))
  elif fexpr.isInfixFuncCall:
    let left = ctx.convertFExpr(fexpr[1])
    let right = ctx.convertFExpr(fexpr[2])
    let tmp = ctx.tmpsym
    case $fexpr[0]
    of "+":
      ctx.add(codeOp(taAdd, tmp, left, right))
    of "-":
      ctx.add(codeOp(taSub, tmp, left, right))
    of "*":
      ctx.add(codeOp(taMul, tmp, left, right))
    of "/":
      ctx.add(codeOp(taDiv, tmp, left, right))
    return atomVar(tmp)
  elif fexpr.kind == fexprSeq and fexpr.len >= 1:
    if $fexpr[0] == "while":
      let cond = ctx.convertFExpr(fexpr[1][0])
      let whilel = ctx.tmplabel
      let bodyl = ctx.tmplabel
      let nextl = ctx.tmplabel
      ctx.addLabel(whilel)
      ctx.add(codeIf(cond, bodyl))
      ctx.add(codeGoto(nextl))
      ctx.addLabel(bodyl)
      for b in fexpr[2]:
        discard ctx.convertFExpr(b)
      ctx.add(codeGoto(whilel))
      ctx.addLabel(nextl)
    else:
      fexpr.error("unsupported internal tacodegen of: $#" % $fexpr)
  else:
    fexpr.error("unsupported tacodegen of: $#" % $fexpr)

let fexprs = parseToplevel("test.flori", """
while(a) {
  1 / 2
}
4 + 5
1 - 1 + 6
""")
var ctx = newTAContext()
for f in fexprs:
  discard ctx.convertFExpr(f)
for f in fexprs:
  echo f
echo "\n=>\n"
echo ctx
