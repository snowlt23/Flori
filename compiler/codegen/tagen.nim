
import ../fcore
import tacode

import options
import strutils, sequtils

proc convertFExpr*(ctx: var TAContext, fexpr: FExpr): TAAtom

# proc convertWhile*(ctx: var TAContext, fexpr: FExpr): TAAtom =
#   let whilel = ctx.tmplabel
#   let bodyl = ctx.tmplabel
#   let nextl = ctx.tmplabel
#   ctx.addLabel(whilel)
#   let cond = ctx.convertFExpr(fexpr[1][0])
#   ctx.add(initTACodeAIf(cond, bodyl))
#   ctx.add(initTACodeGoto(nextl))
#   ctx.addLabel(bodyl)
#   for b in fexpr[2]:
#     discard ctx.convertFExpr(b)
#   ctx.add(initTACodeGoto(whilel))
#   ctx.addLabel(nextl)
#   return initTAAtomNone()

proc convertIf*(ctx: var TAContext, fexpr: FExpr): TAAtom =
  let retsym = ctx.tmpsym
  let nextl = ctx.tmplabel
  let size = fexpr.gettype.typesize
  if size != 0:
    ctx.add(initTACodeAVar(retsym, size, initTAAtomNone()))

  var conds = newSeq[(string, Option[FExpr], FExpr)]()
  conds.add((ctx.tmplabel, some(fexpr.ifcond), fexpr.ifbody))
  for elifbranch in fexpr.elifbranches:
    conds.add((ctx.tmplabel, some(elifbranch.cond), elifbranch.body))
  conds.add((ctx.tmplabel, none(FExpr), fexpr.elsebody))

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
    if fbody.kind == fexprBlock:
      for i in 0..<fbody.sons.len-2:
        discard ctx.convertFExpr(fbody.sons[i])
    if size != 0:
      if fbody.kind == fexprBlock:
        ctx.add(initTACodeSet(retsym, ctx.convertFExpr(fbody.sons[fbody.sons.len-1])))
      else:
        ctx.add(initTACodeSet(retsym, ctx.convertFExpr(fbody)))
    else:
      if fbody.kind == fexprBlock:
        discard ctx.convertFExpr(fbody.sons[fbody.sons.len-1])
      else:
        discard ctx.convertFExpr(fbody)
    ctx.add(initTACodeGoto(nextl))
  ctx.addLabel(nextl)
  return initTAAtomAVar(retsym)

# proc convertSet*(ctx: var TAContext, fexpr: FExpr): TAAtom =
#   let dst = ctx.convertFExpr(fexpr[1])
#   if dst.kind != TAAtomKind.AVar:
#     error(fexpr, "unsupported expression set `= in currently")
#   ctx.add(initTACodeSet(dst.avar.name, ctx.convertFExpr(fexpr[2])))
#   return initTAAtomNone()

proc convertWord*(ctx: var TAContext, fexpr: FExpr): TAAtom =
  if fexpr.internal.obj.internalop != internalNone:
    return initTAAtomNone()

  let fnlabel = desc(fexpr.args[0])
  let retsize = fexpr.internal.obj.returntype.typesize

  var fnctx = newTAContext()
  if fexpr.args[1].kind == fexprBlock:
    for i in 0..<fexpr.args[1].sons.len-1:
      discard fnctx.convertFExpr(fexpr.args[1].sons[i])
    fnctx.add(initTACodeRet(fnctx.convertFExpr(fexpr.args[1].sons[fexpr.args[1].sons.len-1])))
  else:
    fnctx.add(initTACodeRet(fnctx.convertFExpr(fexpr.args[1])))
  var args = newSeq[(string, int)]()
  for i in 0..<fexpr.internal.obj.argtypes.get.len:
    args.add((toString(fexpr.internal.obj.argnames.get[i], desc=true), fexpr.internal.obj.argtypes.get[i].typesize))
  ctx.addFn(TAFn(fnname: fnlabel, args: args, retsize: retsize, body: fnctx.codes))
  return initTAAtomNone()

proc convertCall*(ctx: var TAContext, fexpr: FExpr): TAAtom =
  let fnlabel = desc(fexpr.call)
  var args = fexpr.args.mapIt(ctx.convertFExpr(it))
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
  elif fexpr.kind in fexprCalls and fexpr.call.kind == fexprSymbol and fexpr.call.symbol.fexpr.obj.internal.isSome and fexpr.call.symbol.fexpr.internal.obj.internalop != internalNone:
    let op = fexpr.call.symbol.fexpr.obj.internal.get.obj.internalop
    let left = ctx.convertFExpr(fexpr.args[0])
    let right = ctx.convertFExpr(fexpr.args[1])
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
  elif fexpr.kind in fexprCalls and $fexpr.call == "=>":
    return ctx.convertWord(fexpr)
  elif fexpr.kind in fexprCalls:
    return ctx.convertCall(fexpr)
  elif fexpr.kind == fexprIf:
    return ctx.convertIf(fexpr)
  else:
    fexpr.error("unsupported tacodegen of: $#" % $fexpr)
