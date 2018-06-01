
import ../image, ../parser, ../fexpr
import tacode

import options
import strutils

proc convertFExpr*(ctx: var TAContext, fexpr: FExpr): TAAtom

proc convertWhile*(ctx: var TAContext, fexpr: FExpr): TAAtom =
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
  return atomNone()

proc convertIf*(ctx: var TAContext, fexpr: FExpr): TAAtom =
  var conds = @[(ctx.tmplabel, some(fexpr[1][0]), fexpr[2])]
  var i = 3
  while i < fexpr.len:
    if $fexpr[i] == "elif": # elif clause
      conds.add((ctx.tmplabel, some(fexpr[i+1][0]), fexpr[i+2]))
      i += 3
    elif $fexpr[i] == "else": # else clause
      conds.add((ctx.tmplabel, none(FExpr), fexpr[i+1]))
      i += 2

  let retsym = ctx.tmpsym
  let nextl = ctx.tmplabel
  ctx.add(codeVar(retsym, atomNone()))
  for c in conds:
    let (label, fcond, _) = c
    if fcond.isSome:
      let cond = ctx.convertFExpr(fcond.get)
      ctx.add(codeIf(cond, label))
    else:
      ctx.add(codeGoto(label))
  ctx.add(codeGoto(nextl))
  for c in conds:
    let (label, _, fbody) = c
    ctx.addLabel(label)
    for i in 0..<fbody.len-1:
      discard ctx.convertFExpr(fbody[i])
    if fbody.len != 0:
      ctx.add(codeOp(taSet, "", atomVar(retsym), ctx.convertFExpr(fbody[^1])))
    ctx.add(codeGoto(nextl))
  ctx.addLabel(nextl)
  return atomVar(retsym)

proc convertFExpr*(ctx: var TAContext, fexpr: FExpr): TAAtom =
  if fexpr.kind == fexprIntLit:
    return atomInt(fexpr.intval)
  elif fexpr.kind == fexprIdent:
    return atomVar($fexpr)
  elif fexpr.kind == fexprSymbol:
    return atomVar(desc(fexpr))
  elif fexpr.kind == fexprBlock:
    for b in fexpr:
      discard ctx.convertFExpr(b)
  elif fexpr.obj.isInfixFuncCall and $fexpr[0] == ":=":
    let name = $fexpr[1]
    let value = ctx.convertFExpr(fexpr[2])
    ctx.add(codeVar(name, value))
    return atomNone()
  elif fexpr.obj.isInfixFuncCall:
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
    of "=":
      ctx.add(codeOp(taSet, tmp, left, right))
      return atomNone()
    return atomVar(tmp)
  elif fexpr.kind == fexprSeq and fexpr.len >= 1:
    if $fexpr[0] == "while":
      return ctx.convertWhile(fexpr)
    elif $fexpr[0] == "if":
      return ctx.convertIf(fexpr)
    else:
      fexpr.error("unsupported internal tacodegen of: $#" % $fexpr)
  else:
    fexpr.error("unsupported tacodegen of: $#" % $fexpr)

let fexprs = parseToplevel("test.flori", """
a := 1
if (a) {
  a = 2
  1 + 1
} elif (b) {
  2 + 2
} else {
  3 + 3
}

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
