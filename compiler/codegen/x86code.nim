
import strutils, sequtils
import algorithm
import tables
import options

import variant
import ../image
import tacode, liveness, asm_x86

defVariant X86Atom:
  Temp(name: string)
  Reg(reg: Reg32)
  IntLit(intval: int64)
  StrLit(strval: string)
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
  Call(label: string, args: seq[X86Atom])
  FFICall(label: string, address: Option[int], args: seq[X86Atom], callconv: CallConvention)
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
  of TAAtomKind.StrLit:
    return initX86AtomStrLit(atom.strlit.strval)

proc `$`*(atom: X86Atom): string =
  case atom.kind
  of X86AtomKind.Temp:
    atom.temp.name
  of X86AtomKind.Reg:
    $atom.reg.reg
  of X86AtomKind.IntLit:
    $atom.intlit.intval
  of X86AtomKind.StrLit:
    "\"" & atom.strlit.strval & "\""
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
  of X86CodeKind.FFICall:
    "call $#" % $code.fficall.label
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
  of X86CodeKind.Call:
    for arg in code.call.args.mitems:
      arg.replaceTemp(tmpname, by)
  of X86CodeKind.FFICall:
    for arg in code.fficall.args.mitems:
      arg.replaceTemp(tmpname, by)
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
  else:
    fn.body.add(code)
proc expandIntLit*(fn: X86Fn): X86Fn =
  result = X86Fn(name: fn.name, args: fn.args, body: @[])
  for code in fn.body:
    result.expandIntLit(code)
template expandStrLit*(fn: var X86Fn, code: X86Code, variant: untyped, init: untyped) =
  if code.variant.left.kind != X86AtomKind.Reg and code.variant.right.kind == X86AtomKind.StrLit:
    fn.body.add(initX86CodeMov(initX86AtomReg(eax), code.variant.right))
    fn.body.add(init(code.variant.left, initX86AtomReg(eax)))
  else:
    fn.body.add(code)
proc expandStrLit*(fn: var X86Fn, code: X86Code) =
  case code.kind
  of X86CodeKind.Add:
    expandStrLit(fn, code, add, initX86CodeAdd)
  of X86CodeKind.Sub:
    expandStrLit(fn, code, sub, initX86CodeSub)
  of X86CodeKind.Mov:
    expandStrLit(fn, code, mov, initX86CodeMov)
  else:
    fn.body.add(code)
proc expandStrLit*(fn: X86Fn): X86Fn =
  result = X86Fn(name: fn.name, args: fn.args, body: @[])
  for code in fn.body:
    result.expandStrLit(code)

proc expandCallNaive*(fn: var X86Fn, code: X86Code) =
  if code.kind == X86CodeKind.Call:
    var argssize = 0
    for i in 1..code.call.args.len:
      let n = code.call.args.len - i
      fn.body.add(initX86CodePush(code.call.args[n]))
      argssize += 4
    fn.body.add(code)
    if argssize != 0:
      fn.body.add(initX86CodeAdd(initX86AtomReg(esp), initX86AtomIntLit(argssize)))
  elif code.kind == X86CodeKind.FFICall: # FIXME: call convention
    var argssize = 0
    for i in 1..code.fficall.args.len:
      let n = code.fficall.args.len - i
      fn.body.add(initX86CodePush(code.fficall.args[n]))
      argssize += 4
    fn.body.add(code)
    if argssize != 0:
      fn.body.add(initX86CodeAdd(initX86AtomReg(esp), initX86AtomIntLit(argssize)))
  else:
    fn.body.add(code)
proc expandCallNaive*(fn: X86Fn): X86Fn =
  result = X86Fn(name: fn.name, args: fn.args, body: @[])
  for code in fn.body:
    result.expandCallNaive(code)

proc expandEpilogue*(fn: var X86Fn, code: X86Code) =
  if code.kind == X86CodeKind.Ret:
    fn.body.add(initX86CodeMov(initX86AtomReg(esp), initX86AtomReg(ebp)))
    fn.body.add(initX86CodePop(initX86AtomReg(ebp)))
    fn.body.add(code)
  else:
    fn.body.add(code)
proc expandEpilogue*(fn: X86Fn): X86Fn =
  result = X86Fn(name: fn.name, args: fn.args, body: @[])
  for code in fn.body:
    result.expandEpilogue(code)

#
# Naive Regalloc
#

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
  result = result.expandCallNaive()
  result = result.expandEpilogue()

proc naiveRegalloc*(ctx: X86Context): X86Context =
  result = newX86Context()
  for fn in ctx.fns:
    result.fns.add(fn.naiveRegalloc())

#
# Simple Regalloc
#

proc simpleAllocReg*(regmap: var Table[Reg32, int], liveness: Liveness, varname: string, i: int): Option[Reg32] =
  for r in DataReg32:
    if regmap[r] < liveness.variables[varname].lifetime:
      regmap[r] = liveness.variables[varname].lifetime
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

    let regopt = simpleAllocReg(regmap, liveness, varname, i)
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
  result = result.expandCallNaive()
  result = result.expandEpilogue()

proc simpleRegalloc*(ctx: X86Context, liveness: Liveness): X86Context =
  result = newX86Context()
  for fn in ctx.fns:
    result.fns.add(fn.simpleRegalloc(liveness))

#
# Freq Regalloc
#

type FreqRegInfo* = tuple[lifetime: int, count: int, candestroy: bool, atom: X86Atom]

proc expandCallByRegmap*(fn: var X86Fn, code: X86Code, args: seq[X86Atom], argregs: seq[X86Atom], destregs: seq[Reg32], liveness: Liveness) =
  var argssize = 0
  for r in destregs:
    fn.body.add(initX86CodePush(initX86AtomReg(r)))
  for i in 1..args.len:
    let n = args.len - i
    if argregs[n].kind == X86AtomKind.Reg:
      # fn.body.add(initX86CodePush(argregs[n]))
      fn.body.add(initX86CodeMov(argregs[n], args[n]))
    elif argregs[n].kind == X86AtomKind.EbpRel:
      fn.body.add(initX86CodePush(args[n]))
      argssize += 4
    else:
      assert(false)
  fn.body.add(code)
  for r in destregs.reversed():
    fn.body.add(initX86CodePop(initX86AtomReg(r)))
  # for i in 0..<code.call.args.len:
  #   if argregs[i].kind == X86AtomKind.Reg:
  #     fn.body.add(initX86CodePop(argregs[i]))
  if argssize != 0:
    fn.body.add(initX86CodeAdd(initX86AtomReg(esp), initX86AtomIntLit(argssize)))
proc expandCallByRegmap*(fn: X86Fn, fnmap: Table[string, (seq[X86Atom], seq[Reg32])], liveness: Liveness): X86Fn =
  result = X86Fn(name: fn.name, args: fn.args, body: @[])
  for code in fn.body:
    if code.kind == X86CodeKind.Call:
      let (argregs, destregs) = fnmap[code.call.label]
      result.expandCallByRegmap(code, code.call.args, argregs, destregs, liveness)
    elif code.kind == X86CodeKind.FFICall:
      for arg in code.fficall.args.reversed():
        result.body.add(initX86CodePush(arg))
      result.body.add(code)
      if code.fficall.callconv == convCdecl:
        result.body.add(initX86CodeAdd(initX86AtomReg(esp), initX86AtomIntLit(code.fficall.args.len*4)))
    else:
      result.body.add(code)

proc freqAllocReg*(tbl: var Table[string, FreqRegInfo], regmap: var Table[Reg32, string], curargpos: var int, curvarpos: var int, liveness: Liveness, varname: string, candestroy: bool) =
  let (lifetime, count) = liveness.variables[varname]
  for r in DataReg32:
    if regmap.hasKey(r):
      let reginfo = tbl[regmap[r]]
      if reginfo.candestroy and reginfo.lifetime < lifetime:
        tbl[varname] = (lifetime, count, candestroy, initX86AtomReg(r))
        regmap[r] = varname
        return
      elif reginfo.candestroy and reginfo.count < count:
        curvarpos += 4
        tbl[regmap[r]].atom = initX86AtomEbpRel(-curvarpos)
        tbl[varname] = (lifetime, count, candestroy, initX86AtomReg(r))
        regmap[r] = varname
        return
    else:
      tbl[varname] = (lifetime, count, candestroy, initX86AtomReg(r))
      regmap[r] = varname
      return
  if candestroy:
    curvarpos += 4
    tbl[varname] = (lifetime, count, candestroy, initX86AtomEbpRel(-curvarpos))
  else:
    curargpos += 4
    tbl[varname] = (lifetime, count, candestroy, initX86AtomEbpRel(curargpos+8))

proc freqRegalloc*(fn: X86Fn, fnmap: var Table[string, (seq[X86Atom], seq[Reg32])], liveness: Liveness): X86Fn =
  result = X86Fn(name: fn.name, args: fn.args, body: @[])

  var variables = fn.collectVariables()
  var allocatoms = initTable[string, FreqRegInfo]()
  let destregs = @[ecx, edx, ebx]
  var regmap = initTable[Reg32, string]()

  var curargpos = 0
  var curvarpos = 0
  var fnregs = newSeq[X86Atom]()
  for arg in fn.args:
    let (argname, argsize) = arg
    freqAllocReg(allocatoms, regmap, curargpos, curvarpos, liveness, argname, false)
    fnregs.add(allocatoms[argname].atom)
  fnmap[fn.name] = (fnregs, destregs)

  for variable in variables:
    let (varname, varsize, iscall, i) = variable
    freqAllocReg(allocatoms, regmap, curargpos, curvarpos, liveness, varname, true)

  # prologue gen
  if curvarpos != 0:
    result.body.add(initX86CodePush(initX86AtomReg(ebp)))
    result.body.add(initX86CodeMov(initX86AtomReg(ebp), initX86AtomReg(esp)))
    result.body.add(initX86CodeSub(initX86AtomReg(esp), initX86AtomIntLit(curvarpos)))

  var replaceatoms = newSeq[(string, X86Atom)]()
  for key, value in allocatoms:
    replaceatoms.add((key, value.atom))
  let replacedfn = fn.replaceTemp(replaceatoms)
  for b in replacedfn.body:
    result.body.add(b)

  result = result.expandDoubleRef()
  result = result.expandIntLit()
  result = result.expandStrLit()
  result = result.expandCallByRegmap(fnmap, liveness)
  if curvarpos != 0:
    result = result.expandEpilogue()

proc freqRegalloc*(ctx: X86Context, liveness: Liveness): X86Context =
  result = newX86Context()
  var fnmap = initTable[string, (seq[X86Atom], seq[Reg32])]()
  for fn in ctx.fns:
    result.fns.add(fn.freqRegalloc(fnmap, liveness))
