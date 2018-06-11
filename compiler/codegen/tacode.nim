
import tables
import strutils, sequtils
import options

type
  TAAtomKind* = enum
    atNone
    atVar
    atArg
    atIntLit
  TAAtom* = object
    case kind*: TAAtomKind
    of atNone:
      discard
    of atVar:
      varname*: string
    of atArg:
      argindex*: int
    of atIntLit:
      intval*: int64
      
  TACodeKind* = enum
    taAdd
    taSub
    taMul
    taDiv
    taGreater
    taLess
    taSet
    taFn
    taCall
    taVar
    taGoto
    taIf
    taRet
  TACode* = object
    case kind*: TACodeKind
    of {taAdd..taSet}:
      dstname*: string
      left*: TAAtom
      right*: TAAtom
    of taFn:
      fnname*: string
      fnargs*: seq[(string, int)]
      retsize*: int
    of taCall:
      calldstname*: string
      fnlabel*: string
      args*: seq[TAAtom]
      isPure*: bool
    of taVar:
      varname*: string
      size*: int
      value*: TAAtom
    of taGoto:
      gotolabel*: string
    of taIf:
      cond*: TAAtom
      iflabel*: string
    of taRet:
      ret*: TAAtom
  TAContext* = object
    codes*: seq[TACode]
    fnlabels*: Table[string, int]
    labels*: Table[string, int]
    revlabels*: Table[int, string]
    nextlabel*: Option[string]
    tmp*: int
    tmpl*: int

proc newTAContext*(): TAContext =
  TAContext(codes: @[], fnlabels: initTable[string, int](), labels: initTable[string, int](), revlabels: initTable[int, string]())
proc add*(ctx: var TAContext, code: TACode) =
  ctx.nextlabel = none(string)
  ctx.codes.add(code)
proc addFn*(ctx: var TAContext, code: TACode) =
  ctx.fnlabels[code.fnname] = ctx.codes.len
  ctx.add(code)
proc addLabel*(ctx: var TAContext, labelname: string) =
  ctx.nextlabel = some(labelname)
  ctx.labels[labelname] = ctx.codes.len
  ctx.revlabels[ctx.codes.len] = labelname
proc tmpsym*(ctx: var TAContext, prefix = "t"): string =
  result = prefix & $ctx.tmp
  ctx.tmp.inc
proc tmplabel*(ctx: var TAContext, prefix = "L"): string =
  if ctx.nextlabel.isSome:
    result = ctx.nextlabel.get
    ctx.nextlabel = none(string)
  else:
    result = prefix & $ctx.tmpl
    ctx.tmpl.inc

proc atomNone*(): TAAtom = TAAtom(kind: atNone)
proc atomVar*(name: string): TAAtom = TAAtom(kind: atVar, varname: name)
proc atomArg*(i: int): TAAtom = TAAtom(kind: atArg, argindex: i)
proc atomInt*(x: int64): TAAtom = TAAtom(kind: atIntLit, intval: x)

proc codeOp*(kind: TACodeKind, dstname: string, left, right: TAAtom): TACode =
  result.kind = kind
  result.dstname = dstname
  result.left = left
  result.right = right
proc codeFn*(fnname: string, args: seq[(string, int)], retsize: int): TACode =
  TACode(kind: taFn, fnname: fnname, fnargs: args, retsize: retsize)
proc codeCall*(dstname: string, fnlabel: string, args: seq[TAAtom], pure: bool): TACode =
  TACode(kind: taCall, calldstname: dstname, fnlabel: fnlabel, args: args, isPure: pure)
proc codeVar*(varname: string, size: int, val: TAAtom): TACode =
  TACode(kind: taVar, varname: varname, size: size, value: val)
proc codeGoto*(label: string): TACode =
  TACode(kind: taGoto, gotolabel: label)
proc codeIf*(cond: TAAtom, label: string): TACode =
  TACode(kind: taIf, cond: cond, iflabel: label)
proc codeRet*(ret: TAAtom): TACode =
  TACode(kind: taRet, ret: ret)

iterator atoms*(ctx: var TAContext, s, e: int): var TAAtom =
  for i in s..e:
    case ctx.codes[i].kind
    of taAdd..taSet:
      yield(ctx.codes[i].left)
      yield(ctx.codes[i].right)
    of taCall:
      for arg in ctx.codes[i].args.mitems:
        yield(arg)
    of taVar:
      yield(ctx.codes[i].value)
    of taIf:
      yield(ctx.codes[i].cond)
    of taRet:
      yield(ctx.codes[i].ret)
    else:
      discard
iterator atoms*(ctx: var TAContext): var TAAtom =
  for atom in ctx.atoms(0, ctx.codes.len-1):
    yield(atom)

proc getname*(code: TACode): string =
  case code.kind
  of taAdd..taSet:
    return code.dstname
  of taCall:
    return code.calldstname
  of tavar:
    return code.varname
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
  of atNone:
    "none"
  of atVar:
    a.varname
  of atArg:
    "$" & $a.argindex
  of atIntLit:
    $a.intval
proc `$`*(code: TACode): string =
  case code.kind
  of taAdd:
    "$# = $# + $#" % [code.dstname, $code.left, $code.right]
  of taSub:
    "$# = $# - $#" % [code.dstname, $code.left, $code.right]
  of taMul:
    "$# = $# * $#" % [code.dstname, $code.left, $code.right]
  of taDiv:
    "$# = $# / $#" % [code.dstname, $code.left, $code.right]
  of taGreater:
    "$# = $# > $#" % [code.dstname, $code.left, $code.right]
  of taLess:
    "$# = $# < $#" % [code.dstname, $code.left, $code.right]
  of taSet:
    "$# = $#" % [$code.left, $code.right]
  of taFn:
    "fn $#($#) $#:" % [code.fnname, code.fnargs.map(proc (arg: (string, int)): string =
      let (n, s) = arg
      "$# $#" % [n, sizerepr(s)]
    ).join(", "), sizerepr(code.retsize)]
  of taCall:
    "$# = $#($#)" % [code.calldstname, code.fnlabel, code.args.mapIt($it).join(", ")]
  of taVar:
    "$# $# := $#" % [code.varname, sizerepr(code.size), $code.value]
  of taGoto:
    "goto $#" % code.gotolabel
  of taIf:
    "if $# goto $#" % [$code.cond, code.iflabel]
  of taRet:
    "ret $#\n" % $code.ret

proc `$`*(ctx: TAContext): string =
  result = ""
  for i, c in ctx.codes:
    if ctx.revlabels.hasKey(i):
      result &= ctx.revlabels[i] & ":\n"
    if c.kind == taFn:
      result &= $c & "\n"
    else:
      result &= "  " & $c & "\n"
    
# var ctx = newTAContext()
# ctx.add(codeVar("a", atomInt(5)))
# ctx.add(codeIf(atomVar("a"), "L0"))
# ctx.addLabel("L0")
# ctx.add(codeOp(taAdd, ctx.tmpsym, atomInt(4), atomVar("a")))
# echo ctx
