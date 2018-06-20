
import ../fcore
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
  ctx.add(initTACodeAIf(cond, bodyl))
  ctx.add(initTACodeGoto(nextl))
  ctx.addLabel(bodyl)
  for b in fexpr[2]:
    discard ctx.convertFExpr(b)
  ctx.add(initTACodeGoto(whilel))
  ctx.addLabel(nextl)
  return initTAAtomNone()

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
               fexpr.typ.get.fexpr.typesize
             else:
               -1
  if size != -1:
    ctx.add(initTACodeAVar(retsym, size, initTAAtomNone()))
  for c in conds:
    let (label, fcond, _) = c
    if fcond.isSome:
      let cond = ctx.convertFExpr(fcond.get)
      ctx.add(initTACodeAIf(cond, label))
    else:
      ctx.add(initTACodeGoto(label))
  ctx.add(initTACodeGoto(nextl))
  for c in conds:
    let (label, _, fbody) = c
    ctx.addLabel(label)
    for i in 0..<fbody.len-1:
      discard ctx.convertFExpr(fbody[i])
    if size != -1:
      ctx.add(initTACodeSet(retsym, ctx.convertFExpr(fbody[^1])))
    else:
      discard ctx.convertFExpr(fbody[^1])
    ctx.add(initTACodeGoto(nextl))
  ctx.addLabel(nextl)
  return initTAAtomAVar(retsym)

proc convertFn*(ctx: var TAContext, fexpr: FExpr): TAAtom =
  let parsed = parseDefn(fexpr)
  let args = fexpr[parsed.argdecls]
  let fnlabel = desc(fexpr[parsed.name])
  let retsize = if parsed.ret.isSome:
                  fexpr[parsed.ret.get].symbol.fexpr.typesize
                else:
                  -1

  var fnctx = newTAContext()
  if parsed.body.isSome:
    for i in 0..<fexpr[parsed.body.get].len-1:
      discard fnctx.convertFExpr(fexpr[parsed.body.get][i])
  if parsed.body.isSome and fexpr[parsed.body.get].len != 0:
    fnctx.add(initTACodeRet(fnctx.convertFExpr(fexpr[parsed.body.get][^1])))
  else:
    fnctx.add(initTACodeRet(initTAAtomNone()))
  fnctx.addLabel(ctx.tmplabel)
  ctx.addFn(TAFn(fnname: fnlabel, args: args.mapIt((desc(it[0]), it[1].symbol.fexpr.typesize)), retsize: retsize, body: fnctx.codes))
  return initTAAtomNone()
  
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
  ctx.add(initTACodeCall(tmp, fnlabel, args, false))
  return initTAAtomAVar(tmp)

proc convertFExpr*(ctx: var TAContext, fexpr: FExpr): TAAtom =
  if fexpr.kind == fexprIntLit:
    return initTAAtomIntLit(fexpr.intval)
  elif fexpr.kind == fexprIdent:
    return initTAAtomAVar($fexpr)
  elif fexpr.kind == fexprSymbol:
    return initTAAtomAVar(desc(fexpr))
  elif fexpr.kind == fexprBlock:
    for b in fexpr:
      discard ctx.convertFExpr(b)
    return initTAAtomNone()
  elif fexpr.obj.isInfixFuncCall and $fexpr[0] == ":=":
    let name = desc(fexpr[1])
    let size = if fexpr[2].typ.isSome:
                 fexpr[2].typ.get.fexpr.typesize
               else:
                 0
    let value = ctx.convertFExpr(fexpr[2])
    ctx.add(initTACodeAVar(name, size, value))
    return initTAAtomNone()
  elif fexpr.isFuncCall and fexpr[0].kind == fexprSymbol and fexpr[0].symbol.fexpr.obj.internal.isSome:
    let op = fexpr[0].symbol.fexpr.obj.internal.get.obj.internalop
    let left = ctx.convertFExpr(fexpr[1])
    let right = ctx.convertFExpr(fexpr[2])
    let tmp = ctx.tmpsym
    case op
    of internalAdd:
      ctx.add(initTACodeAdd(tmp, left, right))
    of internalSub:
      ctx.add(initTACodeSub(tmp, left, right))
    of internalMul:
      ctx.add(initTACodeMul(tmp, left, right))
    of internalDiv:
      ctx.add(initTACodeADiv(tmp, left, right))
    of internalGreater:
      ctx.add(initTACodeGreater(tmp, left, right))
    of internalLess:
      ctx.add(initTACodeLesser(tmp, left, right))
    of internalSet:
      ctx.add(initTACodeSet($left, right))
      return initTAAtomNone()
    else:
      return ctx.convertCall(fexpr)
    return initTAAtomAVar(tmp)
  elif fexpr.obj.isFuncCall:
    return ctx.convertCall(fexpr)
  elif fexpr.kind == fexprSeq and fexpr.len >= 1:
    if $fexpr[0] == "type":
      return initTAAtomNone()
    elif $fexpr[0] == "while":
      return ctx.convertWhile(fexpr)
    elif $fexpr[0] == "if":
      return ctx.convertIf(fexpr)
    elif $fexpr[0] == "fn":
      let parsed = parseDefn(fexpr)
      if parsed.body.isSome:
        return ctx.convertFn(fexpr)
      else:
        return initTAAtomNone()
    else:
      fexpr.error("unsupported internal tacodegen of: $#" % $fexpr)
  else:
    fexpr.error("unsupported tacodegen of: $#" % $fexpr)
