
import ../image, ../parser, ../fexpr
import tacode, tagen
import tables
import options

proc replaceAlias*(ctx: var TAFn, name: string, by: string) =
  for atom in ctx.atoms:
    if atom.kind == TAAtomKind.AVar and atom.avar.name == name:
      atom.avar.name = by

proc searchRetName*(ctx: TAFn): Option[string] =
  for code in ctx.body:
    if code.kind == TACodeKind.Ret:
      if code.ret.value.kind == TAAtomKind.AVar:
        return some(code.ret.value.avar.name)
      else:
        return none(string)
  return none(string)

proc canEarlyRet*(ctx: TAFn, x: int): bool =
  let labels = ctx.getLabels()
  var alreadygotos = newSeq[string]()
  var i = x+1
  while i < ctx.body.len:
    if ctx.body[i].kind == TACodeKind.Label:
      i.inc
      continue
    if ctx.body[i].kind == TACodeKind.Ret:
      i.inc
      continue
    if ctx.body[i].kind == TACOdeKind.Goto and ctx.body[i].goto.gotolabel notin alreadygotos:
      alreadygotos.add(ctx.body[i].goto.gotolabel)
      i = labels[ctx.body[i].goto.gotolabel]
      continue
    return false
  return true

proc optElimAlias*(ctx: TAFn): TAFn =
  result = newTAFn(ctx.fnname, ctx.args)
  let retname = ctx.searchRetName()
  var returned = false
  var replaces = newSeq[(string, string)]()
  for i in 0..<ctx.body.len:
    if ctx.body[i].kind == TACodeKind.AVar and ctx.body[i].avar.value.kind == TAAtomKind.AVar:
      replaces.add((ctx.body[i].avar.name, ctx.body[i].avar.value.avar.name))
    elif ctx.body[i].kind == TACodeKind.Set and ctx.body[i].set.value.kind == TAAtomKind.AVar:
      if retname.isSome and ctx.body[i].set.name == retname.get and ctx.canEarlyRet(i):
        result.add(initTACodeRet(ctx.body[i].set.value))
        returned = true
      else:
        result.add(ctx.body[i])
        # replaces.add((ctx.body[i].set.name, ctx.body[i].set.value.avar.name))
    else:
      if ctx.body[i].kind == TACodeKind.Ret and returned: continue
      result.add(ctx.body[i])
  for r in replaces:
    let (left, right) = r
    result.replaceAlias(left, right)

proc optElimUnusedVar*(ctx: TAFn): TAFn =
  result = newTAFn(ctx.fnname, ctx.args)
  let varrefs = ctx.getVarRefs()
  for code in ctx.body:
    if code.kind == TACodeKind.AVar and code.avar.name notin varrefs: continue
    result.add(code)

proc optElimDeadLabel*(ctx: TAFn): TAFn =
  result = newTAFn(ctx.fnname, ctx.args)
  var prevlabelpos = -1
  var deadlabels = newSeq[string]()
  for i in 0..<ctx.body.len:
    if ctx.body[i].kind == TACodeKind.Label:
      if i - prevlabelpos == 0:
        deadlabels.add(ctx.body[prevlabelpos].label.name)
      prevlabelpos = i
  if ctx.body.len-1 - prevlabelpos == 0 and prevlabelpos != -1:
    deadlabels.add(ctx.body[prevlabelpos].label.name)

  for code in ctx.body:
    if code.kind == TACodeKind.Label and code.label.name in deadlabels: continue
    if code.kind == TACodeKind.Goto and code.goto.gotolabel in deadlabels: continue
    result.add(code)

proc optimize*(fn: TAFn): TAFn =
  result = fn
  result = result.optElimAlias()
  result = result.optElimDeadLabel()
  result = result.optElimUnusedVar()
