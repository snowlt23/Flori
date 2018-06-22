
import tables
import strutils, sequtils
import tacode
import options

type
  Liveness* = object
    lifetime*: Table[string, int]

proc isUsed*(atom: TAAtom, name: string): bool =
  atom.kind == TAAtomKind.AVar and atom.avar.name == name
proc isUsed*(code: TACode, name: string): bool =
  case code.kind
  of TACodeKind.Add:
    return code.add.left.isUsed(name) or code.add.right.isUsed(name)
  of TACodeKind.Sub:
    return code.sub.left.isUsed(name) or code.sub.right.isUsed(name)
  of TACodeKind.Mul:
    return code.mul.left.isUsed(name) or code.mul.right.isUsed(name)
  of TACodeKind.ADiv:
    return code.adiv.left.isUsed(name) or code.adiv.right.isUsed(name)
  of TACodeKind.Greater:
    return code.greater.left.isUsed(name) or code.greater.right.isUsed(name)
  of TACodeKind.Lesser:
    return code.lesser.left.isUsed(name) or code.lesser.right.isUsed(name)
  of TACodeKind.Set:
    return code.set.value.isUsed(name)
  of TACodeKind.Call:
    for arg in code.call.args:
      if arg.isUsed(name):
        return true
  of TACodeKind.AVar:
    return code.avar.value.isUsed(name)
  of TACodeKind.AIf:
    return code.aif.cond.isUsed(name)
  else:
    return false

proc findBackwardGoto*(codes: seq[TACode], prevlabels: seq[string], x: int): Option[int] =
  result = none(int)
  for i in x+1..<codes.len:
    if codes[i].kind == TACodeKind.Goto and codes[i].goto.gotolabel in prevlabels:
      result = some(i)

proc analyzeLiveness*(ctx: TAContext): Liveness =
  result = Liveness(lifetime: initTable[string, int]())
  for fn in ctx.fns:
    for i in 0..<fn.body.len:
      if not fn.body[i].hasDist: continue
      if not result.lifetime.hasKey(fn.body[i].getname):
        result.lifetime[fn.body[i].getname] = i
      result.lifetime[fn.body[i].getname] = max(result.lifetime[fn.body[i].getname], i)
      var prevlabels = newSeq[string]()
      for j in i+1..<fn.body.len:
        if fn.body[j].kind == TACodeKind.Label:
          prevlabels.add(fn.body[j].label.name)
          continue
        if fn.body[j].isUsed(fn.body[i].getname):
          let nextopt = findBackwardGoto(fn.body, prevlabels, j)
          if nextopt.isSome:
            result.lifetime[fn.body[i].getname] = max(result.lifetime[fn.body[i].getname], nextopt.get)
          else:
            result.lifetime[fn.body[i].getname] = max(result.lifetime[fn.body[i].getname], j)
