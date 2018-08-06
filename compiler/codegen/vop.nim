
import macros
import strutils, sequtils, tables
import options
import ../fcore

#
# VOP inst macros
#

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

template inst*(e) =
  ctx.add(e)

macro instcall*(e: untyped): untyped =
  var call = nnkCall.newTree(e[0])
  call.add(ident"ctx")
  for i in 1..<e.len:
    call.add(e[i])
  return call

template label*(l) =
  ctx.add(Label(l))

template vop*(f: FExpr): auto = ctx.vop(f)

template vopret*(t) =
  return some(t)
template vopnone*() =
  return some(Temp(""))

#
# VOP macros
#

macro defineVOP*(name: untyped, body: untyped): untyped =
  let ctx = ident"ctx"
  let f = ident"f"
  result = quote do:
    proc `name`*(`ctx`: var VOPFn, `f`: FExpr): Option[VOPAtom] =
      `body`

macro defineVOPSet*(body: untyped): untyped =
  let ctx = ident"ctx"
  let f = ident"f"
  result = quote do:
    proc vop*(`ctx`: var VOPFn, `f`: FExpr): VOPAtom = discard
  result[6] = newStmtList()
  for b in body:
    result[6].add quote do:
      let opt = `b`(`ctx`, `f`)
      if opt.isSome:
        return opt.get
  result[6].add quote do:
    `f`.error("unmatched $# expression to VOP." % $`f`)

#
# VOP attributes
#

template pattern*(e) =
  if not e:
    return none(VOPAtom)

macro temps*(ts: varargs[untyped]): untyped =
  result = newStmtList()
  for t in ts:
    result.add quote do:
      let `t` = Temp(ctx.gensym())

macro labels*(ls: varargs[untyped]): untyped =
  result = newStmtList()
  for l in ls:
    result.add quote do:
      let `l` = ctx.genlabel()

template generate*(body) =
  body
