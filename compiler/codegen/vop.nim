
import macros
import strutils, sequtils, tables
import options
import ../image, ../fexpr, ../symbol

type
  VOPAtom* = string
  VOPKind* = enum
    vopLabel
    vopAVar
    vopInt
    vopAdd
    vopSub
    vopLesser
    vopIfGoto
    vopGoto
  VOP* = object
    name*: string
    case kind*: VOPKind
    of vopAVar:
      avarsize*: int
    of vopLabel:
      label*: string
    of vopInt:
      intval*: int64
    of vopAdd:
      addleft*: VOPAtom
      addright*: VOPAtom
    of vopSub:
      subleft*: VOPAtom
      subright*: VOPAtom
    of vopLesser:
      lesserleft*: VOPAtom
      lesserright*: VOPAtom
    of vopIfGoto:
      cond*: VOPAtom
      ifgotolabel*: string
    of vopGoto:
      gotolabel*: string
  VOPCtx* = object
    vops*: seq[VOP]
    tmpcnt*: int

#
# VOPCtx
#

proc initVOPCtx*(): VOPCtx = VOPCtx(vops: @[], tmpcnt: 0)

proc add*(ctx: var VOPCtx, vop: VOP) = ctx.vops.add(vop)

proc tmpsym*(ctx: var VOPCtx, prefix = ".t"): string =
  result = prefix & $ctx.tmpcnt
  ctx.tmpcnt.inc

#
# VOP
#

proc int*(name: VOPAtom, x: int64): VOP =
  VOP(name: name, kind: vopInt, intval: x)

proc avar*(name: VOPAtom, size: int): VOP =
  VOP(name: name, kind: vopAVar, avarsize: size)

proc add*(name: VOPAtom, left, right: VOPAtom): VOP =
  VOP(name: name, kind: vopAdd, addleft: left, addright: right)
proc sub*(name: VOPAtom, left, right: VOPAtom): VOP =
  VOP(name: name, kind: vopSub, subleft: left, subright: right)
proc lesser*(name: VOPAtom, left, right: VOPAtom): VOP =
  VOP(name: name, kind: vopLesser, lesserleft: left, lesserright: right)

proc ifgoto*(name: VOPAtom, c: VOPAtom, l: string): VOP =
  VOP(name: name, kind: vopIfGoto, cond: c, ifgotolabel: l)
proc goto*(name: VOPAtom, l: string): VOP =
  VOP(name: name, kind: vopGoto, gotolabel: l)

#
# VOP macros
#

template tmpsym(s: string, p = ".t"): string =
  if not tmpsymtbl.hasKey(s):
    tmpsymtbl[s] = ctx.tmpsym(p)
  tmpsymtbl[s]
template tmplabel(s: string, p = ".L"): string =
  tmpsym(s, p)

macro `<-`*(t: untyped, e: untyped): untyped =
  if e.kind == nnkIntLit:
    return quote do:
      ctx.add(int(tmpsym(astToStr(`t`)), `e`))
      template `t`: string = tmpsym(astToStr(`t`))
  var call = nnkCall.newTree(e[0])
  call.add(nnkCall.newTree(ident"tmpsym", newLit($t)))
  for i in 1..<e.len:
    call.add(e[i])
  result = quote do:
    ctx.add(`call`)
    template `t`: string = tmpsym(astToStr(`t`))

macro inst*(e: untyped): untyped =
  var call = nnkCall.newTree(e[0])
  call.add(newLit(""))
  for i in 1..<e.len:
    call.add(e[i])
  result = quote do:
    ctx.add(`call`)

template label*(t) =
  ctx.add(VOP(name: "", kind: vopLabel, label: t))

template vopret*(t) =
  return some(tmpsym(astToStr(t)))

template vopnone*() =
  return some("")

macro defineVOP*(name: untyped, body: untyped): untyped =
  let ctx = ident"ctx"
  let f = ident"f"
  result = quote do:
    proc `name`*(`ctx`: var VOPCtx, `f`: FExpr): Option[VOPAtom] =
      `body`

macro defineVOPSet*(body: untyped): untyped =
  let ctx = ident"ctx"
  let f = ident"f"
  result = quote do:
    proc vop*(`ctx`: var VOPCtx, `f`: FExpr): VOPAtom = discard
  result[6] = newStmtList()
  for b in body:
    result[6].add quote do:
      let opt = `b`(`ctx`, `f`)
      if opt.isSome:
        return opt.get
  result[6].add quote do:
    `f`.error("unmatched $# expression to VOP." % $`f`)

template pattern*(e) =
  if not e:
    return none(VOPAtom)

macro labels*(ls: varargs[untyped]): untyped =
  result = newStmtList()
  for l in ls:
    result.add quote do:
      template `l`: string = tmplabel(astToStr(`l`))

template generate*(body) =
  template vop(f: FExpr): VOPAtom = ctx.vop(f)
  var tmpsymtbl {.inject.} = initTable[string, VOPAtom]()
  body

#
# stringer
#

proc `$`*(vop: VOP): string =
  if vop.name == "":
    result = ""
  else:
    result = vop.name & " <- "
  case vop.kind
  of vopLabel:
    result &= vop.label & ":"
  of vopAVar:
    result &= "var(size: $#)" % $vop.avarsize
  of vopInt:
    result &= $vop.intval
  of vopAdd:
    result &= "add " & vop.addleft & ", " & vop.addright
  of vopSub:
    result &= "sub " & vop.subleft & ", " & vop.subright
  of vopLesser:
    result &= "lesser " & vop.lesserleft & ", " & vop.lesserright
  of vopIfGoto:
    result &= "if $# goto $#" % [vop.cond, vop.ifgotolabel]
  of vopGoto:
    result &= "goto $#" % vop.gotolabel

proc `$`*(ctx: VOPCtx): string =
  result = ""
  for vop in ctx.vops:
    if vop.kind == vopLabel:
      result &= $vop & "\n"
    else:
      result &= "  " & $vop & "\n"

#
# VOP definitions
#

proc vop*(ctx: var VOPCtx, f: FExpr): VOPAtom

proc eqInternal*(f: FExpr, op: InternalOp): bool =
  f.kind in fexprCalls and f.call.kind == fexprSymbol and f.call.symbol.fexpr.internal.obj.internalop == op

defineVOP vopIntLit:
  pattern f.kind == fexprIntLit
  generate:
    t0 <- int(f.intval)
    vopret t0

defineVOP vopInternalWord:
  pattern f.kind in fexprCalls and $f.call == "=>" and f.internal.obj.internalop != internalNone
  generate:
    vopnone

defineVOP vopIntAdd:
  pattern f.eqInternal(internalAdd)
  generate:
    t0 <- add(vop(f.args[0]), vop(f.args[1]))
    vopret t0

defineVOP vopIntSub:
  pattern f.eqInternal(internalSub)
  generate:
    t0 <- sub(vop(f.args[0]), vop(f.args[1]))
    vopret t0

defineVOP vopIntLesser:
  pattern f.eqInternal(internalLess)
  generate:
    t0 <- lesser(vop(f.args[0]), vop(f.args[1]))
    vopret t0

defineVOP vopIf:
  pattern f.kind == fexprIf
  labels TL, FL, EL
  generate:
    inst ifgoto(vop(f.ifbranch.args[0]), TL)
    inst goto(FL)
    label TL
    discard vop(f.ifbranch.args[1])
    inst goto(EL)
    label FL
    discard vop(f.elsebody)
    inst goto(EL)
    label EL
    vopret ret

defineVOPSet:
  vopIntLit
  vopInternalWord
  vopIntAdd
  vopIntSub
  vopIntLesser
  vopIf

