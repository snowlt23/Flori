
import strutils, sequtils
import tables
import options

import variant
import tacode, liveness, asm_x86

defVariant X86Atom:
  Temp(name: string)
  Reg(reg: Reg32)
  IntLit(intval: int64)
  EbpRel(rel: int)
  EspRel(rel: int)

defVariant X86Code:
  Label(name: string)
  AVar(name: string, size: int, stack: bool)
  Add(left: X86Atom, right: X86Atom)
  Sub(left: X86Atom, right: X86Atom)
  Mul(right: X86Atom)
  ADiv(right: X86Atom)
  Mov(left: X86Atom, right: X86Atom)
  Push(value: X86Atom)
  Pop(value: X86Atom)
  Cmp(left: X86Atom, right: X86Atom)
  JmpGreater(label: string)
  JmpLesser(label: string)
  JmpZero(label: string)
  Jmp(label: string)
  Call(label: string)
  Ret()

type
  X86Fn* = object
    name*: string
    args*: seq[(string, int)]
    body*: seq[X86Code]
  X86Context* = object
    fns*: seq[X86Fn]
    codes*: seq[X86Code]
    tmpl*: int

const refX86Atom* = {X86AtomKind.EbpRel, X86AtomKind.EspRel}

proc initX86CodeAVar*(name: string, size: int): X86Code = initX86CodeAVar(name, size, false)

proc add*(ctx: var X86Context, code: X86Code) =
  ctx.codes.add(code)
proc addLabel*(ctx: var X86Context, labelname: string) =
  ctx.codes.add(initX86CodeLabel(labelname))
proc tmplabel*(ctx: var X86Context, prefix = "XL"): string =
  result = prefix & $ctx.tmpl
  ctx.tmpl.inc

proc toX86Atom*(atom: TAAtom): X86Atom =
  case atom.kind
  of TAAtomKind.None:
    raise newException(Exception, "cannot convert TAAtom.None to X86Atom")
  of TAAtomKind.AVar:
    return initX86AtomTemp(atom.avar.name)
  of TAAtomKind.IntLit:
    return initX86AtomIntLit(atom.intlit.intval)

proc `$`*(atom: X86Atom): string =
  case atom.kind
  of X86AtomKind.Temp:
    atom.temp.name
  of X86AtomKind.Reg:
    $atom.reg.reg
  of X86AtomKind.IntLit:
    $atom.intlit.intval
  of X86AtomKind.EbpRel:
    if atom.ebprel.rel < 0:
      "[ebp$#]" % $atom.ebprel.rel
    else:
      "[ebp+$#]" % $atom.ebprel.rel
  of X86AtomKind.EspRel:
    if atom.ebprel.rel < 0:
      "[esp$#]" % $atom.ebprel.rel
    else:
      "[esp+$#]" % $atom.ebprel.rel

proc `$`*(code: X86Code): string =
  case code.kind
  of X86CodeKind.Label:
    "$#:" % code.label.name
  of X86CodeKind.AVar:
    "var $#: <$#>" % [code.avar.name, $code.avar.size]
  of X86CodeKind.Add:
    "add $#, $#" % [$code.add.left, $code.add.right]
  of X86CodeKind.Sub:
    "sub $#, $#" % [$code.sub.left, $code.sub.right]
  of X86CodeKind.Mul:
    "mul $#" % [$code.mul.right]
  of X86CodeKind.ADiv:
    "div $#" % [$code.adiv.right]
  of X86CodeKind.Mov:
    "mov $#, $#" % [$code.mov.left, $code.mov.right]
  of X86CodeKind.Push:
    "push $#" % [$code.push.value]
  of X86CodeKind.Pop:
    "pop $#" % [$code.pop.value]
  of X86CodeKind.Cmp:
    "cmp $#, $#" % [$code.cmp.left, $code.cmp.right]
  of X86CodeKind.JmpGreater:
    "jg $#" % code.jmpgreater.label
  of X86CodeKind.JmpLesser:
    "jl $#" % code.jmplesser.label
  of X86CodeKind.JmpZero:
    "jz $#" % code.jmpzero.label
  of X86CodeKind.Jmp:
    "jmp $#" % code.jmp.label
  of X86CodeKind.Call:
    "call $#" % code.call.label
  of X86CodeKind.Ret:
    "ret"

proc `$`*(fn: X86Fn): string =
  "fn $#:\n  $#" % [fn.name, fn.body.mapIt($it).join("\n  ")]
proc `$`*(ctx: X86Context): string =
  var sq = newSeq[string]()
  for fn in ctx.fns:
    sq.add($fn)
  return sq.join("\n")

proc newX86Context*(): X86Context =
  X86Context(fns: @[], codes: @[])

proc toX86Code*(ctx: var X86Context, code: TACode) =
  case code.kind
  of TACodeKind.Label:
    ctx.add(initX86CodeLabel(code.label.name))
  of TACodeKind.Add:
    ctx.add(initX86CodeAVar(code.add.name, 4)) # FIXME:
    ctx.add(initX86CodeMov(initX86AtomTemp(code.add.name), toX86Atom(code.add.left)))
    ctx.add(initX86CodeAdd(initX86AtomTemp(code.add.name), toX86Atom(code.add.right)))
  of TACodeKind.Sub:
    ctx.add(initX86CodeAVar(code.sub.name, 4)) # FIXME:
    ctx.add(initX86CodeMov(initX86AtomTemp(code.sub.name), toX86Atom(code.sub.left)))
    ctx.add(initX86CodeSub(initX86AtomTemp(code.sub.name), toX86Atom(code.sub.right)))
  of TACodeKind.Mul:
    ctx.add(initX86CodeAVar(code.mul.name, 4)) # FIXME:
    ctx.add(initX86CodeMov(initX86AtomReg(eax), toX86Atom(code.mul.left)))
    ctx.add(initX86CodeMul(toX86Atom(code.mul.right)))
    ctx.add(initX86CodeMov(initX86AtomTemp(code.mul.name), initX86AtomReg(eax)))
  of TACodeKind.ADiv:
    ctx.add(initX86CodeAVar(code.adiv.name, 4)) # FIXME:
    ctx.add(initX86CodeMov(initX86AtomReg(eax), toX86Atom(code.adiv.left)))
    ctx.add(initX86CodeADiv(toX86Atom(code.adiv.right)))
    ctx.add(initX86CodeMov(initX86AtomTemp(code.adiv.name), initX86AtomReg(eax)))
  of TACodeKind.Greater:
    let tlabel = ctx.tmplabel
    let nlabel = ctx.tmplabel
    ctx.add(initX86CodeAVar(code.greater.name, 4))
    ctx.add(initX86CodeCmp(toX86Atom(code.greater.left), toX86Atom(code.greater.right)))
    ctx.add(initX86CodeJmpGreater(tlabel))
    ctx.add(initX86CodeMov(initX86AtomTemp(code.greater.name), initX86AtomIntLit(0)))
    ctx.add(initX86CodeJmp(nlabel))
    ctx.add(initX86CodeLabel(tlabel))
    ctx.add(initX86CodeMov(initX86AtomTemp(code.greater.name), initX86AtomIntLit(1)))
    ctx.add(initX86CodeLabel(nlabel))
  of TACodeKind.Lesser:
    let tlabel = ctx.tmplabel
    let nlabel = ctx.tmplabel
    ctx.add(initX86CodeAVar(code.lesser.name, 4)) # FIXME:
    ctx.add(initX86CodeCmp(toX86Atom(code.lesser.left), toX86Atom(code.lesser.right)))
    ctx.add(initX86CodeJmpLesser(tlabel))
    ctx.add(initX86CodeMov(initX86AtomTemp(code.lesser.name), initX86AtomIntLit(0)))
    ctx.add(initX86CodeJmp(nlabel))
    ctx.add(initX86CodeLabel(tlabel))
    ctx.add(initX86CodeMov(initX86AtomTemp(code.lesser.name), initX86AtomIntLit(1)))
    ctx.add(initX86CodeLabel(nlabel))
  of TACodeKind.Set:
    ctx.add(initX86CodeMov(initX86AtomTemp(code.set.name), toX86Atom(code.set.value)))
  of TACodeKind.Call:
    var argssize = 0
    for i in 1..code.call.args.len:
      let n = code.call.args.len - i
      ctx.add(initX86CodePush(toX86Atom(code.call.args[n])))
      argssize += 4 # FIXME:
    ctx.add(initX86CodeAVar(code.call.name, 4, true)) # FIXME:
    ctx.add(initX86CodeCall(code.call.calllabel))
    ctx.add(initX86CodeMov(initX86AtomTemp(code.call.name), initX86AtomReg(eax)))
    ctx.add(initX86CodeAdd(initX86AtomReg(esp), initX86AtomIntLit(argssize)))
  of TACodeKind.AVar:
    ctx.add(initX86CodeAVar(code.avar.name, code.avar.size))
    if code.avar.value.kind != TAAtomKind.None:
      ctx.add(initX86CodeMov(initX86AtomTemp(code.avar.name), toX86Atom(code.avar.value)))
  of TACodeKind.Goto:
    ctx.add(initX86CodeJmp(code.goto.gotolabel))
  of TACodeKind.AIf:
    ctx.add(initX86CodeCmp(toX86Atom(code.aif.cond), initX86AtomIntLit(1)))
    ctx.add(initX86CodeJmpZero(code.aif.gotolabel))
  of TACodeKind.Ret:
    ctx.add(initX86CodeMov(initX86AtomReg(eax), toX86Atom(code.ret.value)))
proc toX86Context*(ctx: TAContext): X86Context =
  result = newX86Context()
  for fn in ctx.fns:
    var fnctx = newX86Context()
    for b in fn.body:
      fnctx.toX86Code(b)
    result.fns.add(X86Fn(name: fn.fnname, args: fn.args, body: fnctx.codes))

proc collectNaiveStacklen*(fn: X86Fn): int =
  result = 0
  for code in fn.body:
    if code.kind == X86CodeKind.AVar:
      result += code.avar.size
proc collectVariables*(fn: X86Fn): seq[(string, int, bool, int)] =
  result = @[]
  for i, code in fn.body:
    if code.kind == X86CodeKind.AVar:
      result.add((code.avar.name, code.avar.size, code.avar.stack, i))

proc replaceTemp*(atom: var X86Atom, tmpname: string, by: X86Atom) =
  if atom.kind == X86AtomKind.Temp and atom.temp.name == tmpname:
    atom = by
proc replaceTemp*(code: var X86Code, tmpname: string, by: X86Atom) =
  case code.kind
  of X86CodeKind.Add:
    code.add.left.replaceTemp(tmpname, by)
    code.add.right.replaceTemp(tmpname, by)
  of X86CodeKind.Sub:
    code.sub.left.replaceTemp(tmpname, by)
    code.sub.right.replaceTemp(tmpname, by)
  of X86CodeKind.Mul:
    code.mul.right.replaceTemp(tmpname, by)
  of X86CodeKind.ADiv:
    code.adiv.right.replaceTemp(tmpname, by)
  of X86CodeKind.Mov:
    code.mov.left.replaceTemp(tmpname, by)
    code.mov.right.replaceTemp(tmpname, by)
  of X86CodeKind.Push:
    code.push.value.replaceTemp(tmpname, by)
  of X86CodeKind.Pop:
    code.pop.value.replaceTemp(tmpname, by)
  of X86CodeKind.Cmp:
    code.cmp.left.replaceTemp(tmpname, by)
    code.cmp.right.replaceTemp(tmpname, by)
  else:
    discard
proc replaceTemp*(fn: X86Fn, replaces: seq[(string, X86Atom)]): X86Fn =
  result = X86Fn(name: fn.name, args: fn.args, body: @[])
  for code in fn.body:
    var code = code
    for rep in replaces:
      let (tmpname, by) = rep
      code.replaceTemp(tmpname, by)
    if code.kind != X86CodeKind.AVar:
      result.body.add(code)

proc isDoubleRef*(a, b: X86Atom): bool =
  a.kind in refX86Atom and b.kind in refX86Atom

template expandLeftRight*(fn: var X86Fn, code: X86Code, variant: untyped, init: untyped) =
  if isDoubleRef(code.variant.left, code.variant.right):
    fn.body.add(initX86CodeMov(initX86AtomReg(eax), code.variant.right))
    fn.body.add(init(code.variant.left, initX86AtomReg(eax)))
  else:
    fn.body.add(code)
proc expandDoubleRef*(fn: var X86Fn, code: X86Code) =
  case code.kind
  of X86CodeKind.Add:
    expandLeftRight(fn, code, add, initX86CodeAdd)
  of X86CodeKind.Sub:
    expandLeftRight(fn, code, sub, initX86CodeSub)
  of X86CodeKind.Mov:
    expandLeftRight(fn, code, mov, initX86CodeMov)
  of X86CodeKind.Cmp:
    expandLeftRight(fn, code, cmp, initX86CodeCmp)
  else:
    fn.body.add(code)
proc expandDoubleRef*(fn: X86Fn): X86Fn =
  result = X86Fn(name: fn.name, args: fn.args, body: @[])
  for code in fn.body:
    result.expandDoubleRef(code)

template expandIntLit*(fn: var X86Fn, code: X86Code, variant: untyped, init: untyped) =
  if code.variant.left.kind != X86AtomKind.Reg and code.variant.right.kind == X86AtomKind.IntLit:
    fn.body.add(initX86CodeMov(initX86AtomReg(eax), code.variant.right))
    fn.body.add(init(code.variant.left, initX86AtomReg(eax)))
  else:
    fn.body.add(code)
proc expandIntLit*(fn: var X86Fn, code: X86Code) =
  case code.kind
  of X86CodeKind.Add:
    expandIntLit(fn, code, add, initX86CodeAdd)
  of X86CodeKind.Sub:
    expandIntLit(fn, code, sub, initX86CodeSub)
  of X86CodeKind.Mov:
    expandIntLit(fn, code, mov, initX86CodeMov)
  of X86CodeKind.Cmp:
    expandIntLit(fn, code, cmp, initX86CodeCmp)
  else:
    fn.body.add(code)
proc expandIntLit*(fn: X86Fn): X86Fn =
  result = X86Fn(name: fn.name, args: fn.args, body: @[])
  for code in fn.body:
    result.expandIntLit(code)

proc naiveRegalloc*(fn: X86Fn): X86Fn =
  result = X86Fn(name: fn.name, args: fn.args, body: @[])

  # prologue gen
  let stacklen = fn.collectNaiveStacklen()
  result.body.add(initX86CodePush(initX86AtomReg(ebp)))
  result.body.add(initX86CodeMov(initX86AtomReg(ebp), initX86AtomReg(esp)))
  result.body.add(initX86CodeSub(initX86AtomReg(esp), initX86AtomIntLit(stacklen)))

  var variables = fn.collectVariables()
  var allocatoms = newSeq[(string, X86Atom)]()

  # argument gen
  var curargpos = 0
  for i, arg in fn.args:
    let (argname, argsize) = arg
    allocatoms.add((argname, initX86AtomEbpRel(8 + curargpos)))
    curargpos += argsize
  # variable gen
  var curvarpos = 0
  for variable in variables:
    let (varname, varsize, _, _) = variable
    curvarpos += varsize
    allocatoms.add((varname, initX86AtomEbpRel(-curvarpos)))

  # replace temporary variables by allocatoms
  let replacedfn = fn.replaceTemp(allocatoms)
  for b in replacedfn.body:
    result.body.add(b)

  result = result.expandDoubleRef()
  result = result.expandIntLit()

proc naiveRegalloc*(ctx: X86Context): X86Context =
  result = newX86Context()
  for fn in ctx.fns:
    result.fns.add(fn.naiveRegalloc())

proc isDuplicate*(a, b: seq[string]): bool =
  for e in b:
    if e in a:
      return true
  return false

proc allocReg*(regmap: var Table[Reg32, int], liveness: Liveness, varname: string, i: int): Option[Reg32] =
  for r in DataReg32:
    if regmap[r] < liveness.lifetime[varname]:
      regmap[r] = liveness.lifetime[varname]
      return some(r)
  return none(Reg32)

proc simpleRegalloc*(fn: X86Fn, liveness: Liveness): X86Fn =
  result = X86Fn(name: fn.name, args: fn.args, body: @[])

  var variables = fn.collectVariables()
  var allocatoms = newSeq[(string, X86Atom)]()

  var curargpos = 0
  for i, arg in fn.args:
    let (argname, argsize) = arg
    allocatoms.add((argname, initX86AtomEbpRel(8 + curargpos)))
    curargpos += argsize
  var curvarpos = 0
  var regmap = initTable[Reg32, int]()
  for r in DataReg32:
    regmap[r] = -1
  for variable in variables:
    let (varname, varsize, iscall, i) = variable
    if iscall:
      curvarpos += varsize
      allocatoms.add((varname, initX86AtomEbpRel(-curvarpos)))
      continue

    let regopt = allocReg(regmap, liveness, varname, i)
    if regopt.isSome:
      allocatoms.add((varname, initX86AtomReg(regopt.get)))
    else:
      curvarpos += varsize
      allocatoms.add((varname, initX86AtomEbpRel(-curvarpos)))

  # prologue gen
  result.body.add(initX86CodePush(initX86AtomReg(ebp)))
  result.body.add(initX86CodeMov(initX86AtomReg(ebp), initX86AtomReg(esp)))
  result.body.add(initX86CodeSub(initX86AtomReg(esp), initX86AtomIntLit(curvarpos)))

  let replacedfn = fn.replaceTemp(allocatoms)
  for b in replacedfn.body:
    result.body.add(b)

  result = result.expandDoubleRef()
  result = result.expandIntLit()

proc simpleRegalloc*(ctx: X86Context, liveness: Liveness): X86Context =
  result = newX86Context()
  for fn in ctx.fns:
    result.fns.add(fn.simpleRegalloc(liveness))
