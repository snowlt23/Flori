
import tables
import strutils, sequtils
import options

import ../image
import variant

defVariant TAAtom:
  None()
  AVar(name: string)
  IntLit(intval: int64)
  StrLit(strval: string)

defVariant TACode:
  Add(name: string, left: TAAtom, right: TAAtom)
  Sub(name: string, left: TAAtom, right: TAAtom)
  Mul(name: string, left: TAAtom, right: TAAtom)
  ADiv(name: string, left: TAAtom, right: TAAtom)
  Greater(name: string, left: TAAtom, right: TAAtom)
  Lesser(name: string, left: TAAtom, right: TAAtom)
  Set(name: string, value: TAAtom)
  AAddr(name: string, value: TAAtom)
  Deref(name: string, value: TAAtom)
  DerefSet(left: TAAtom, right: TAAtom)
  Label(name: string)
  Call(name: string, calllabel: string, args: seq[TAAtom], isPure: bool)
  FFICall(name: string, calllabel: string, ffiname: string, dll: Option[string], address: Option[int], args: seq[TAAtom], isPure: bool, callconv: CallConvention, internal: bool)
  Struct(name: string, args: seq[TAAtom], size: int)
  Field(name: string, struc: TAAtom, fieldname: string, pos: int, size: int)
  AVar(name: string, size: int, value: TAAtom)
  Goto(gotolabel: string)
  AIf(cond: TAAtom, gotolabel: string)
  Ret(value: TAAtom)

type
  TAFn* = object
    fnname*: string
    args*: seq[(string, int)]
    retsize*: int
    body*: seq[TACode]
    tmp*: int
    tmpl*: int

proc newTAFn*(name: string, args: seq[(string, int)]): TAFn =
  TAFn(fnname: name, args: args, retsize: 0, body: @[])
proc emptyTAFn*(): TAFn =
  newTAFn("", @[])
proc add*(ctx: var TAFn, code: TACode) =
  ctx.body.add(code)
proc addLabel*(ctx: var TAFn, label: string) =
  ctx.body.add(initTACodeLabel(label))
proc tmpsym*(ctx: var TAFn, prefix = "t"): string =
  result = ctx.fnname & "." & prefix & $ctx.tmp
  ctx.tmp.inc
proc tmplabel*(ctx: var TAFn, prefix = "L"): string =
  result = ctx.fnname & "." & prefix & $ctx.tmpl
  ctx.tmpl.inc

proc hasDist*(code: TACode): bool =
  case code.kind
  of TACodeKind.Add:
    return true
  of TACodeKind.Sub:
    return true
  of TACodeKind.Mul:
    return true
  of TACodeKind.ADiv:
    return true
  of TACodeKind.Greater:
    return true
  of TACodeKind.Lesser:
    return true
  of TACodeKind.Set:
    return true
  of TACodeKind.AAddr:
    return true
  of TACodeKind.Deref:
    return true
  of TACodeKind.DerefSet:
    return false
  of TACodeKind.Struct:
    return true
  of TACodeKind.Call:
    return true
  of TACodeKind.FFICall:
    return true
  of TACodeKind.AVar:
    return true
  else:
    return false

proc getname*(code: TACode): string =
  case code.kind
  of TACodeKind.Add:
    return code.add.name
  of TACodeKind.Sub:
    return code.sub.name
  of TACodeKind.Mul:
    return code.mul.name
  of TACodeKind.ADiv:
    return code.adiv.name
  of TACodeKind.Greater:
    return code.greater.name
  of TACodeKind.Lesser:
    return code.lesser.name
  of TACodeKind.Set:
    return code.set.name
  of TACodeKind.AAddr:
    return code.aaddr.name
  of TACodeKind.Deref:
    return code.deref.name
  of TACodeKind.Call:
    return code.call.name
  of TACodeKind.FFICall:
    return code.fficall.name
  of TACodeKind.Struct:
    return code.struct.name
  of TACodeKind.Field:
    return code.field.name
  of TACodeKind.AVar:
    return code.avar.name
  else:
    raise newException(Exception, "cannot getname: $#" % $code.kind)

proc getVarRefs*(code: TACode): seq[string] =
  result = @[]
  case code.kind
  of TACodeKind.Add:
    if code.add.left.kind == TAAtomKind.AVar:
      result.add(code.add.left.avar.name)
    if code.add.right.kind == TAAtomKind.AVar:
      result.add(code.add.right.avar.name)
  of TACodeKind.Sub:
    if code.sub.left.kind == TAAtomKind.AVar:
      result.add(code.sub.left.avar.name)
    if code.sub.right.kind == TAAtomKind.AVar:
      result.add(code.sub.right.avar.name)
  of TACodeKind.Mul:
    if code.mul.left.kind == TAAtomKind.AVar:
      result.add(code.mul.left.avar.name)
    if code.mul.right.kind == TAAtomKind.AVar:
      result.add(code.mul.right.avar.name)
  of TACodeKind.ADiv:
    if code.adiv.left.kind == TAAtomKind.AVar:
      result.add(code.adiv.left.avar.name)
    if code.adiv.right.kind == TAAtomKind.AVar:
      result.add(code.adiv.right.avar.name)
  of TACodeKind.Greater:
    if code.greater.left.kind == TAAtomKind.AVar:
      result.add(code.greater.left.avar.name)
    if code.greater.right.kind == TAAtomKind.AVar:
      result.add(code.greater.right.avar.name)
  of TACodeKind.Lesser:
    if code.lesser.left.kind == TAAtomKind.AVar:
      result.add(code.lesser.left.avar.name)
    if code.lesser.right.kind == TAAtomKind.AVar:
      result.add(code.lesser.right.avar.name)
  of TACodeKind.Set:
    result.add(code.set.name)
    if code.set.value.kind == TAAtomKind.AVar:
      result.add(code.set.value.avar.name)
  of TACodeKind.AAddr:
    if code.aaddr.value.kind == TAAtomKind.AVar:
      result.add(code.aaddr.value.avar.name)
  of TACodeKind.Deref:
    if code.deref.value.kind == TAAtomKind.AVar:
      result.add(code.deref.value.avar.name)
  of TACodeKind.DerefSet:
    if code.derefset.left.kind == TAAtomKind.AVar:
      result.add(code.derefset.left.avar.name)
    if code.derefset.right.kind == TAAtomKind.AVar:
      result.add(code.derefset.right.avar.name)
  of TACodeKind.Label:
    discard
  of TACodeKind.Call:
    for arg in code.call.args:
      if arg.kind == TAAtomKind.AVar:
        result.add(arg.avar.name)
  of TACodeKind.FFICall:
    for arg in code.fficall.args:
      if arg.kind == TAAtomKind.AVar:
        result.add(arg.avar.name)
  of TACodeKind.Struct:
    for arg in code.struct.args:
      if arg.kind == TAAtomKind.AVar:
        result.add(arg.avar.name)
  of TACodeKind.Field:
    if code.field.struc.kind == TAAtomKind.AVar:
      result.add(code.field.struc.avar.name)
  of TACodeKind.AVar:
    if code.avar.value.kind == TAAtomKind.AVAr:
      result.add(code.avar.value.avar.name)
  of TACodeKind.Goto:
    discard
  of TACodeKind.AIf:
    if code.aif.cond.kind == TAAtomKind.AVAr:
      result.add(code.aif.cond.avar.name)
  of TACodeKind.Ret:
    if code.ret.value.kind == TAAtomKind.AVAr:
      result.add(code.ret.value.avar.name)
proc getVarRefs*(fn: TAFn): seq[string] =
  result = @[]
  for code in fn.body:
    result &= code.getVarRefs()

proc getLabels*(fn: TAFn): Table[string, int] =
  result = initTable[string, int]()
  for i, code in fn.body:
    if code.kind == TACodeKind.Label:
      result[code.label.name] = i

proc sizerepr*(s: int): string =
  if s == -1:
    "<undefined>"
  elif s == 0:
    "<void>"
  else:
    "<$#>" % $s
proc `$`*(a: TAAtom): string =
  case a.kind
  of TAAtomKind.None:
    "none"
  of TAAtomKind.AVar:
    a.avar.name
  of TAAtomKind.IntLit:
    $a.intlit.intval
  of TAAtomKind.StrLit:
    "\"" & a.strlit.strval & "\""
proc `$`*(code: TACode): string =
  case code.kind
  of TACodeKind.Add:
    "$# = $# + $#" % [code.add.name, $code.add.left, $code.add.right]
  of TACodeKind.Sub:
    "$# = $# - $#" % [code.sub.name, $code.sub.left, $code.sub.right]
  of TACodeKind.Mul:
    "$# = $# * $#" % [code.mul.name, $code.mul.left, $code.mul.right]
  of TACodeKind.ADiv:
    "$# = $# / $#" % [code.adiv.name, $code.adiv.left, $code.adiv.right]
  of TACodeKind.Greater:
    "$# = $# > $#" % [code.greater.name, $code.greater.left, $code.greater.right]
  of TACodeKind.Lesser:
    "$# = $# < $#" % [code.lesser.name, $code.lesser.left, $code.lesser.right]
  of TACodeKind.Set:
    "$# = $#" % [code.set.name, $code.set.value]
  of TACodeKind.AAddr:
    "$# = addr($#)" % [code.aaddr.name, $code.aaddr.value]
  of TACodeKind.Deref:
    "$# = deref($#)" % [code.deref.name, $code.deref.value]
  of TACodeKind.DerefSet:
    "deref($#) = $#" % [$code.derefset.left, $code.derefset.right]
  of TACodeKind.Label:
    "$#:" % code.label.name
  of TACodeKind.Call:
    "$# = $#($#)" % [code.call.name, code.call.calllabel, code.call.args.mapIt($it).join(", ")]
  of TACodeKind.FFICall:
    "$# = $#($#)" % [code.fficall.name, $code.fficall.calllabel, code.fficall.args.mapIt($it).join(", ")]
  of TACodeKind.Struct:
    "$# = $$struct($#)" % [code.struct.name, code.struct.args.mapIt($it).join(", ")]
  of TACodeKind.Field:
    "$# = $$field($#, $#)" % [code.field.name, $code.field.struc, code.field.fieldname]
  of TACodeKind.AVar:
    "$# $# := $#" % [code.avar.name, sizerepr(code.avar.size), $code.avar.value]
  of TACodeKind.Goto:
    "goto $#" % code.goto.gotolabel
  of TACodeKind.AIf:
    "if $# goto $#" % [$code.aif.cond, code.aif.gotolabel]
  of TACodeKind.Ret:
    "ret $#\n" % $code.ret.value

proc `$`*(fn: TAFn): string =
  result = "fn $#($#) $#:\n" % [
    fn.fnname,
    fn.args.map(proc (arg: (string, int)): string =
                  let (n, s) = arg
                  "$# $#" % [n, sizerepr(s)]).join(", "),
    sizerepr(fn.retsize)
  ]
  for code in fn.body:
    if code.kind == TACodeKind.Label:
      result &= $code & "\n"
    else:
      result &= "  " & $code & "\n"

iterator atoms*(fn: var TAFn): var TAAtom =
  for code in fn.body.mitems:
    case code.kind
    of TACodeKind.Add:
      yield(code.add.left)
      yield(code.add.right)
    of TACodeKind.Sub:
      yield(code.sub.left)
      yield(code.sub.right)
    of TACodeKind.Mul:
      yield(code.mul.left)
      yield(code.mul.right)
    of TACodeKind.ADiv:
      yield(code.adiv.left)
      yield(code.adiv.right)
    of TACodeKind.Greater:
      yield(code.greater.left)
      yield(code.greater.right)
    of TACodeKind.Lesser:
      yield(code.lesser.left)
      yield(code.lesser.right)
    of TACodeKind.Set:
      yield(code.set.value)
    of TACodeKind.AAddr:
      yield(code.aaddr.value)
    of TACodeKind.Deref:
      yield(code.deref.value)
    of TACodeKind.DerefSet:
      yield(code.derefset.left)
      yield(code.derefset.right)
    of TACodeKind.Label:
      discard
    of TACodeKind.Call:
      for arg in code.call.args.mitems:
        yield(arg)
    of TACodeKind.FFICall:
      for arg in code.fficall.args.mitems:
        yield(arg)
    of TACodeKind.Struct:
      for arg in code.struct.args.mitems:
        yield(arg)
    of TACodeKind.Field:
      yield(code.field.struc)
    of TACodeKind.AVar:
      yield(code.avar.value)
    of TACodeKind.Goto:
      discard
    of TACodeKind.AIf:
      yield(code.aif.cond)
    of TACodeKind.Ret:
      yield(code.ret.value)

# let left = initTAAtomIntLit(4)
# let right = initTAAtomIntLit(5)
# let avar = initTAAtomAVar("t0")
# echo initTACodeAdd("t0", left, right)
# echo initTACodeSub("t1", avar, right)

# var ctx = newTAContext()
# ctx.add(codeVar("a", atomInt(5)))
# ctx.add(codeIf(atomVar("a"), "L0"))
# ctx.addLabel("L0")
# ctx.add(codeOp(taAdd, ctx.tmpsym, atomInt(4), atomVar("a")))
# echo ctx
