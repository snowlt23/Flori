
import tables
import strutils, sequtils
import tacode
import options

type
  VarLive* = tuple[lifetime: int, count: int]
  Liveness* = object
    variables*: Table[string, VarLive]

proc isUsed*(atom: TAAtom, name: string): bool =
  atom.kind == TAAtomKind.AVar and atom.avar.name == name
proc useCount*(code: TACode, name: string): int =
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

proc findBackwardGoto*(codes: seq[TACode], prevlabels: seq[string], x: int): Option[int] =
  result = none(int)
  for i in x+1..<codes.len:
    if codes[i].kind == TACodeKind.Goto and codes[i].goto.gotolabel in prevlabels:
      result = some(i)

proc analyzeLiveness*(ctx: TAContext): Liveness =
  result = Liveness(variables: initTable[string, VarLive]())

  for fn in ctx.fns:
    # fn args liveness
    for i, arg in fn.args:
      let (argname, _) = arg
      if not result.variables.hasKey(argname):
        result.variables[argname] = (i, 0)
      result.variables[argname].lifetime = max(result.variables[argname].lifetime, i)
      var prevlabels = newSeq[string]()
      for j in i+1..<fn.body.len:
        if fn.body[j].kind == TACodeKind.Label:
          prevlabels.add(fn.body[j].label.name)
          continue
        let count = fn.body[j].useCount(argname)
        if count != 0:
          result.variables[argname].count += count
          let nextopt = findBackwardGoto(fn.body, prevlabels, j)
          if nextopt.isSome:
            result.variables[argname].lifetime = max(result.variables[argname].lifetime, nextopt.get)
          else:
            result.variables[argname].lifetime = max(result.variables[argname].lifetime, j)

    # fn variables liveness
    for i in 0..<fn.body.len:
      if not fn.body[i].hasDist: continue
      if not result.variables.hasKey(fn.body[i].getname):
        result.variables[fn.body[i].getname] = (i, 0)
      result.variables[fn.body[i].getname].lifetime = max(result.variables[fn.body[i].getname].lifetime, i)
      var prevlabels = newSeq[string]()
      for j in i+1..<fn.body.len:
        if fn.body[j].kind == TACodeKind.Label:
          prevlabels.add(fn.body[j].label.name)
          continue
        let count = fn.body[j].useCount(fn.body[i].getname)
        if count != 0:
          result.variables[fn.body[i].getname].count += count
          let nextopt = findBackwardGoto(fn.body, prevlabels, j)
          if nextopt.isSome:
            result.variables[fn.body[i].getname].lifetime = max(result.variables[fn.body[i].getname].lifetime, nextopt.get)
          else:
            result.variables[fn.body[i].getname].lifetime = max(result.variables[fn.body[i].getname].lifetime, j)
