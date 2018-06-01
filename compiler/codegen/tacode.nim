
import tables
import strutils
import options

type
  TAAtomKind* = enum
    atNone
    atVar
    atIntLit
  TAAtom* = object
    case kind*: TAAtomKind
    of atNone:
      discard
    of atVar:
      varname*: string
    of atIntLit:
      intval*: int64
      
  TACodeKind* = enum
    taAdd
    taSub
    taMul
    taDiv
    taSet
    taVar
    taGoto
    taIf
  TACode* = object
    case kind*: TACodeKind
    of {taAdd, taSub, taMul, taDiv, taSet}:
      dstname*: string
      left*: TAAtom
      right*: TAAtom
    of taVar:
      varname*: string
      value*: TAAtom
    of taGoto:
      gotolabel*: string
    of taIf:
      cond*: TAAtom
      iflabel*: string
  TAContext* = object
    codes*: seq[TACode]
    labels*: Table[string, int]
    revlabels*: Table[int, string]
    nextlabel*: Option[string]
    tmp*: int
    tmpl*: int

proc newTAContext*(): TAContext =
  TAContext(codes: @[], labels: initTable[string, int](), revlabels: initTable[int, string]())
proc add*(ctx: var TAContext, code: TACode) =
  ctx.nextlabel = none(string)
  ctx.codes.add(code)
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
proc atomInt*(x: int64): TAAtom = TAAtom(kind: atIntLit, intval: x)

proc codeOp*(kind: TACodeKind, dstname: string, left, right: TAAtom): TACode =
  result.kind = kind
  result.dstname = dstname
  result.left = left
  result.right = right
proc codeVar*(varname: string, val: TAAtom): TACode =
  TACode(kind: taVar, varname: varname, value: val)
proc codeGoto*(label: string): TACode =
  TACode(kind: taGoto, gotolabel: label)
proc codeIf*(cond: TAAtom, label: string): TACode =
  TACode(kind: taIf, cond: cond, iflabel: label)

proc `$`*(a: TAAtom): string =
  case a.kind
  of atNone:
    "none"
  of atVar:
    a.varname
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
  of taSet:
    "$# = $#" % [$code.left, $code.right]
  of taVar:
    "$# := $#" % [code.varname, $code.value]
  of taGoto:
    "goto $#" % code.gotolabel
  of taIf:
    "if $# goto $#" % [$code.cond, code.iflabel]

proc `$`*(ctx: TAContext): string =
  result = ""
  for i, c in ctx.codes:
    if ctx.revlabels.hasKey(i):
      result &= ctx.revlabels[i] & ":\n"
    result &= "  " & $c & "\n"
    
# var ctx = newTAContext()
# ctx.add(codeVar("a", atomInt(5)))
# ctx.add(codeIf(atomVar("a"), "L0"))
# ctx.addLabel("L0")
# ctx.add(codeOp(taAdd, ctx.tmpsym, atomInt(4), atomVar("a")))
# echo ctx
