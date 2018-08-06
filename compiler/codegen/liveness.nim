
import tables
import strutils, sequtils
import tacode
import options

type
  VarLive* = tuple[lifetime: int, count: int]
  Liveness* = Table[string, VarLive]

proc isUsed*(atom: TAAtom, name: string): bool =
  atom.kind == TAAtomKind.AVar and atom.avar.name == name
proc getUseCount*(code: TACode, name: string): int =
  result = 0
  case code.kind
  of TACodeKind.Add:
    if code.add.left.isUsed(name):
      result.inc
    if code.add.right.isUsed(name):
      result.inc
  of TACodeKind.Sub:
    if code.sub.left.isUsed(name):
      result.inc
    if code.sub.right.isUsed(name):
      result.inc
  of TACodeKind.Mul:
    if code.mul.left.isUsed(name):
      result.inc
    if code.mul.right.isUsed(name):
      result.inc
  of TACodeKind.ADiv:
    if code.mul.left.isUsed(name):
      result.inc
    if code.mul.right.isUsed(name):
      result.inc
  of TACodeKind.Greater:
    if code.greater.left.isUsed(name):
      result.inc
    if code.greater.right.isUsed(name):
      result.inc
  of TACodeKind.Lesser:
    if code.lesser.left.isUsed(name):
      result.inc
    if code.lesser.right.isUsed(name):
      result.inc
  of TACodeKind.Set:
    if code.set.value.isUsed(name):
      result.inc
  of TACodeKind.Call:
    for arg in code.call.args:
      if arg.isUsed(name):
        result.inc
  of TACodeKind.FFICall:
    for arg in code.fficall.args:
      if arg.isUsed(name):
        result.inc
  of TACodeKind.AVar:
    if code.avar.value.isUsed(name):
      result.inc
  of TACodeKind.AIf:
    if code.aif.cond.isUsed(name):
      result.inc
  else:
    discard

proc findBackwardGoto*(codes: openArray[TACode], prevlabels: seq[string], start: int): Option[int] =
  result = none(int)
  for i in start+1..<codes.len:
    if codes[i].kind == TACodeKind.Goto and codes[i].goto.gotolabel in prevlabels:
      result = some(i)

proc analyzeLivenessOfVar*(liveness: var Liveness, codes: openArray[TACode], varname: string, i: int) =
  if not liveness.hasKey(varname):
    liveness[varname] = (i, 0)
  liveness[varname].lifetime = max(liveness[varname].lifetime, i)
  var prevlabels = newSeq[string]()
  for j in i+1..<codes.len:
    if codes[j].kind == TACodeKind.Label:
      prevlabels.add(codes[j].label.name)
      continue

    let count = codes[j].getUseCount(varname)
    if count != 0:
      liveness[varname].count += count
      let nextopt = findBackwardGoto(codes, prevlabels, j)
      if nextopt.isSome:
        liveness[varname].lifetime = max(liveness[varname].lifetime, nextopt.get)
      else:
        liveness[varname].lifetime = max(liveness[varname].lifetime, j)

proc analyzeLiveness*(fn: TAFn): Liveness =
  result = initTable[string, VarLive]()
  # fn args liveness
  for i, arg in fn.args:
    let (argname, _) = arg
    result.analyzeLivenessOfVar(fn.body, argname, i)

  # fn variables liveness
  for i in 0..<fn.body.len:
    if not fn.body[i].hasDist: continue
    result.analyzeLivenessOfVar(fn.body, fn.body[i].getname, i)
