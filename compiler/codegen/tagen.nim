
import ../fcore
import ../internalffi
import tacode

import options
import strutils, sequtils
import dynlib

proc convertFExpr*(ctx: var TAFn, fexpr: FExpr): TAAtom

proc convertWhile*(ctx: var TAFn, fexpr: FExpr): TAAtom =
  let whilel = ctx.tmplabel
  let bodyl = ctx.tmplabel
  let nextl = ctx.tmplabel
  ctx.addLabel(whilel)
  let cond = ctx.convertFExpr(fexpr.whilebranch.args[0])
  ctx.add(initTACodeAIf(cond, bodyl))
  ctx.add(initTACodeGoto(nextl))
  ctx.addLabel(bodyl)
  if fexpr.whilebranch.args[1].kind == fexprBlock:
    for b in fexpr.whilebranch.args[1].sons:
      discard ctx.convertFExpr(b)
  else:
    discard ctx.convertFExpr(fexpr.whilebranch.args[1])
  ctx.add(initTACodeGoto(whilel))
  ctx.addLabel(nextl)
  return initTAAtomNone()

proc convertIf*(ctx: var TAFn, fexpr: FExpr): TAAtom =
  let retsym = ctx.tmpsym
  let nextl = ctx.tmplabel
  let size = fexpr.gettype.typesize
  if size != 0:
    ctx.add(initTACodeAVar(retsym, size, initTAAtomNone()))

  var conds = newSeq[(string, Option[FExpr], FExpr)]()
  conds.add((ctx.tmplabel, some(fexpr.ifbranch.args[0]), fexpr.ifbranch.args[1]))
  for elifbranch in fexpr.elifbranches:
    conds.add((ctx.tmplabel, some(elifbranch.args[0]), elifbranch.args[1]))
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

proc convertSet*(ctx: var TAFn, fexpr: FExpr): TAAtom =
  let dst = ctx.convertFExpr(fexpr.args[0])
  if dst.kind != TAAtomKind.AVar:
    error(fexpr, "unsupported expression set `= in currently")
  ctx.add(initTACodeSet(dst.avar.name, ctx.convertFExpr(fexpr.args[1])))
  return initTAAtomNone()

proc convertWord*(ctx: var TAFn, fexpr: FExpr): TAAtom =
  if fexpr.internal.obj.internalop != internalNone:
    return initTAAtomNone()
  if fexpr.internal.obj.isStruct:
    return initTAAtomNone()
  if fexpr.internal.obj.cffi.isSome:
    return initTAAtomNone()
  if not toSeq(fexpr.internal.obj.argtypes.get.items).isSpecTypes:
    return initTAAtomNone()

  let fnlabel = desc(fexpr.args[0])
  let retsize = fexpr.internal.obj.returntype.typesize

  if fexpr.args[1].kind == fexprBlock:
    for i in 0..<fexpr.args[1].sons.len-1:
      discard ctx.convertFExpr(fexpr.args[1].sons[i])
    ctx.add(initTACodeRet(ctx.convertFExpr(fexpr.args[1].sons[fexpr.args[1].sons.len-1])))
  else:
    ctx.add(initTACodeRet(ctx.convertFExpr(fexpr.args[1])))
  var args = newSeq[(string, int)]()
  for i in 0..<fexpr.internal.obj.argtypes.get.len:
    args.add((toString(fexpr.internal.obj.argnames.get[i], desc=true), fexpr.internal.obj.argtypes.get[i].typesize))
  ctx.fnname = fnlabel
  ctx.args = args
  ctx.retsize = retsize
  return initTAAtomNone()

proc convertCall*(ctx: var TAFn, fexpr: FExpr): TAAtom =
  let tmp = ctx.tmpsym
  let fnlabel = desc(fexpr.call)
  if fexpr.call.symbol.fexpr.internal.obj.isStruct:
    ctx.add(initTACodeAVar(tmp, fexpr.gettype.typesize, initTAAtomNone()))
    var pos = 0
    for arg in fexpr.args:
      let at = ctx.convertFExpr(arg)
      let atmp = ctx.tmpsym
      let stmp = ctx.tmpsym
      ctx.add(initTACodeAAddr(atmp, initTAAtomAVar(tmp)))
      ctx.add(initTACodeAdd(stmp, initTAAtomAVar(atmp), initTAAtomIntLit(pos)))
      ctx.add(initTACodeDerefSet(initTAAtomAVar(stmp), at))
      pos += 4
  else:
    var args = newSeq[TAAtom]()
    for arg in fexpr.args:
      let at = ctx.convertFExpr(arg)
      let argtyp = if arg.gettype.kind == symbolLink:
                     arg.gettype.wrapped
                   else:
                     arg.gettype
      if argtyp.fexpr.internal.obj.isStruct:
        let tmp = ctx.tmpsym
        ctx.add(initTACodeAAddr(tmp, at))
        args.add(initTAAtomAVar(tmp))
      else:
        args.add(at)
    ctx.add(initTACodeCall(tmp, fnlabel, args, false))
  return initTAAtomAVar(tmp)

proc convertField*(ctx: var TAFn, fexpr: FExpr): TAAtom =
  let fieldname = $fexpr.args[1]
  var pos = 0
  let structyp = if fexpr.args[0].gettype.kind == symbolLink:
                   fexpr.args[0].gettype.wrapped
                 else:
                   fexpr.args[0].gettype
  for field in structyp.fexpr.args:
    let fname = if field.kind == fexprSymbol:
                  field
                else:
                  field.args[0]
    if $fname == fieldname:
      break
    pos += fname.gettype.typesize
  let struc = ctx.convertFExpr(fexpr.args[0])
  let fieldtyp = fexpr.args[2].symbol
  let stmp = ctx.tmpsym
  let ftmp = ctx.tmpsym
  ctx.add(initTACodeAdd(stmp, struc, initTAAtomIntLit(pos)))
  ctx.add(initTACodeDeref(ftmp, initTAAtomAVar(stmp)))
  return initTAAtomAVar(ftmp)

proc convertFExpr*(ctx: var TAFn, fexpr: FExpr): TAAtom =
  if fexpr.kind == fexprIntLit:
    return initTAAtomIntLit(fexpr.intval)
  elif fexpr.kind == fexprStrLit:
    return initTAAtomStrLit($fexpr.strval)
  elif fexpr.kind == fexprIdent:
    return initTAAtomAVar($fexpr)
  elif fexpr.kind == fexprSymbol:
    return initTAAtomAVar(desc(fexpr))
  elif fexpr.kind in fexprCalls and $fexpr.call == ":=":
    let name = desc(fexpr.args[0])
    let size = fexpr.args[1].gettype.typesize
    let value = ctx.convertFExpr(fexpr.args[1])
    ctx.add(initTACodeAVar(name, size, value))
    return initTAAtomNone()
  elif fexpr.kind in fexprCalls and $fexpr.call == "=":
    return ctx.convertSet(fexpr)
  elif fexpr.kind in fexprCalls and fexpr.call.kind == fexprSymbol and fexpr.call.symbol.fexpr.obj.internal.isSome and fexpr.call.symbol.fexpr.internal.obj.internalop != internalNone:
    let op = fexpr.call.symbol.fexpr.obj.internal.get.obj.internalop
    let tmp = ctx.tmpsym
    if op == internalAddr:
      ctx.add(initTACodeAAddr(tmp, ctx.convertFExpr(fexpr.args[0])))
      return initTAAtomAVar(tmp)
    elif op == internalDeref:
      ctx.add(initTACodeDeref(tmp, ctx.convertFExpr(fexpr.args[0])))
      return initTAAtomAVar(tmp)

    let left = ctx.convertFExpr(fexpr.args[0])
    let right = ctx.convertFExpr(fexpr.args[1])
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
  elif fexpr.kind in fexprCalls and fexpr.call.kind == fexprSymbol and
      fexpr.call.symbol.fexpr.internal.obj.cffi.isSome:
    if fexpr.call.symbol.fexpr.internal.obj.dll.isNone and not fexpr.call.symbol.fexpr.internal.obj.internalffi:
      fexpr.error("$cffi call needs $dll or $internalffi.")
    let cname = $fexpr.call.symbol.fexpr.internal.obj.cffi.get
    let caddr = if fexpr.call.symbol.fexpr.internal.obj.internalffi:
                  getInternalFFI(cname)
                else:
                  let dllname = $fexpr.call.symbol.fexpr.internal.obj.dll.get
                  let lib = loadLib(dllname)
                  lib.symAddr(cname)
    # echo cast[proc (x: int32): int32 {.cdecl.}](caddr)(9)
    if caddr.isNil:
      fexpr.error("not found \"$#\" cffi function." % [cname])
    let tmp = ctx.tmpsym
    if fexpr.call.symbol.fexpr.internal.obj.callconv == convNone:
      fexpr.call.symbol.fexpr.error("please specify ffi call convention. ($cdecl, $stdcall)")
    if fexpr.call.symbol.fexpr.internal.obj.internalffi:
      ctx.add(initTACodeFFICall(tmp, $fexpr.call, cname, none(string), some(cast[int](caddr)), fexpr.args.mapIt(ctx.convertFExpr(it)), false, fexpr.call.symbol.fexpr.internal.obj.callconv, true))
    else:
      ctx.add(initTACodeFFICall(tmp, $fexpr.call, cname, some($fexpr.call.symbol.fexpr.internal.obj.dll.get), some(cast[int](caddr)), fexpr.args.mapIt(ctx.convertFExpr(it)), false, fexpr.call.symbol.fexpr.internal.obj.callconv, false))
    return initTAAtomAVar(tmp)
  elif fexpr.kind in fexprCalls and $fexpr.call == "=>":
    return ctx.convertWord(fexpr)
  elif fexpr.kind in fexprCalls:
    case $fexpr.call
    of "$typed", "$returned":
      return initTAAtomNone()
    of "$field":
      return ctx.convertField(fexpr)
    else:
      return ctx.convertCall(fexpr)
  elif fexpr.kind == fexprIf:
    return ctx.convertIf(fexpr)
  elif fexpr.kind == fexprWhile:
    return ctx.convertWhile(fexpr)
  else:
    fexpr.error("unsupported tacodegen of: $#" % $fexpr)
