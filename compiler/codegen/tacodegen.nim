
import ../image, ../parser, ../fexpr
import tacode

import options
import strutils, sequtils

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
  let size = if fexpr.typ.isSome:
               999999
             else:
               0
  ctx.add(codeVar(retsym, size, atomNone()))
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

proc convertArg*(ctx: var TAContext, id: string, i: int, s: int, e: int) =
  for atom in ctx.atoms(s, e):
    if atom.kind == atVar and atom.varname == id:
      atom = atomArg(i)
proc convertFn*(ctx: var TAContext, fexpr: FExpr): TAAtom =
  let fname = fexpr[1]
  let args = if fexpr[2].kind == fexprArray:
               fexpr[3]
             else:
               fexpr[2]
  let fbody = fexpr[^1]
  let fnlabel = desc(fname)
  ctx.addFn(codeFn(fnlabel, args.mapIt((desc(it[0]), 0)), 0))
  let s = ctx.codes.len
  for i in 0..<fbody.len-1:
    discard ctx.convertFExpr(fbody[i])
  if fbody.len != 0:
    ctx.add(codeRet(ctx.convertFExpr(fbody[^1])))
  else:
    ctx.add(codeRet(atomNone()))
  let e = ctx.codes.len-1
  for i, argname in args.mapIt(desc(it[0])):
    ctx.convertArg(argname, i, s, e)
  ctx.addLabel(ctx.tmplabel)
  return atomNone()
  
proc convertCall*(ctx: var TAContext, fexpr: FExpr): TAAtom =
  let fnlabel = desc(fexpr[0])
  var args = newSeq[TAAtom]()
  if fexpr.obj.isNormalFuncCall:
    for arg in fexpr[1]:
      args.add(ctx.convertFExpr(arg))
  elif fexpr.obj.isGenericsFuncCall:
    for arg in fexpr[2]:
      args.add(ctx.convertFExpr(arg))
  elif fexpr.obj.isInfixFuncCall:
    args.add(ctx.convertFExpr(fexpr[1]))
    args.add(ctx.convertFExpr(fexpr[2]))
  let tmp = ctx.tmpsym
  ctx.add(codeCall(tmp, fnlabel, args, false))
  return atomVar(tmp)

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
    return atomNone()
  elif fexpr.obj.isInfixFuncCall and $fexpr[0] == ":=":
    let name = $fexpr[1]
    let size = if fexpr[2].typ.isSome:
                 999999
               else:
                 0
    let value = ctx.convertFExpr(fexpr[2])
    ctx.add(codeVar(name, size, value))
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
    of ">":
      ctx.add(codeOp(taGreater, tmp, left, right))
    of "<":
      ctx.add(codeOp(taLess, tmp, left, right))
    of "=":
      ctx.add(codeOp(taSet, tmp, left, right))
      return atomNone()
    else:
      return ctx.convertCall(fexpr)
    return atomVar(tmp)
  elif fexpr.obj.isFuncCall:
    return ctx.convertCall(fexpr)
  elif fexpr.kind == fexprSeq and fexpr.len >= 1:
    if $fexpr[0] == "while":
      return ctx.convertWhile(fexpr)
    elif $fexpr[0] == "if":
      return ctx.convertIf(fexpr)
    elif $fexpr[0] == "fn":
      return ctx.convertFn(fexpr)
    else:
      fexpr.error("unsupported internal tacodegen of: $#" % $fexpr)
  else:
    fexpr.error("unsupported tacodegen of: $#" % $fexpr)
