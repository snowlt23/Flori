
import tables
import strutils, sequtils
import options

import variant

defVariant TAAtom:
  None()
  AVar(name: string)
  IntLit(intval: int64)

defVariant TACode:
  Add(name: string, left: TAAtom, right: TAAtom)
  Sub(name: string, left: TAAtom, right: TAAtom)
  Mul(name: string, left: TAAtom, right: TAAtom)
  ADiv(name: string, left: TAAtom, right: TAAtom)
  Greater(name: string, left: TAAtom, right: TAAtom)
  Lesser(name: string, left: TAAtom, right: TAAtom)
  Set(name: string, value: TAAtom)
  Label(name: string)
  Call(name: string, calllabel: string, args: seq[TAAtom], isPure: bool)
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
  TAContext* = object
    fns*: seq[TAFn]
    codes*: seq[TACode]
    tmp*: int
    tmpl*: int

proc newTAContext*(): TAContext =
  TAContext(fns: @[], codes: @[])
proc add*(ctx: var TAContext, code: TACode) =
  ctx.codes.add(code)
proc addLabel*(ctx: var TAContext, name: string) =
  ctx.codes.add(initTACodeLabel(name))
proc addFn*(ctx: var TAContext, fn: TAFn) =
  ctx.fns.add(fn)
proc tmpsym*(ctx: var TAContext, prefix = "t"): string =
  result = prefix & $ctx.tmp
  ctx.tmp.inc
proc tmplabel*(ctx: var TAContext, prefix = "L"): string =
  result = prefix & $ctx.tmpl
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
  of TACodeKind.Call:
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
  of TACodeKind.Call:
    return code.call.name
  of TACodeKind.AVar:
    return code.avar.name
  else:
    raise newException(Exception, "cannot getname: $#" % $code.kind)

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
  of TACodeKind.Label:
    "$#:" % code.label.name
  of TACodeKind.Call:
    "$# = $#($#)" % [code.call.name, code.call.calllabel, code.call.args.mapIt($it).join(", ")]
  of TACodeKind.AVar:
    "$# $# := $#" % [code.avar.name, sizerepr(code.avar.size), $code.avar.value]
  of TACodeKind.Goto:
    "goto $#" % code.goto.gotolabel
  of TACodeKind.AIf:
    "if $# goto $#" % [$code.aif.cond, code.aif.gotolabel]
  of TACodeKind.Ret:
    "ret $#\n" % $code.ret.value

proc `$`*(fn: TAFn): string =
  "fn $#($#) $#:\n  $#" % [fn.fnname, fn.args.map(proc (arg: (string, int)): string =
                                                let (n, s) = arg
                                                "$# $#" % [n, sizerepr(s)]
  ).join(", "), sizerepr(fn.retsize), fn.body.mapIt($it).join("\n  ")]

proc `$`*(ctx: TAContext): string =
  result = ""
  for i, c in ctx.fns:
    result &= $c & "\n"

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
