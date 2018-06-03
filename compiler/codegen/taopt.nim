
import ../image, ../parser, ../fexpr
import tacode, tacodegen
import tables
import options

proc reloc*(ctx: TAContext, reloc: var TAContext, i: int) =
  if ctx.revlabels.hasKey(i):
    if ctx.revlabels.hasKey(i):
      let label = ctx.revlabels[i]
      reloc.addLabel(label)

proc cmpDup*(a, b: TAAtom): bool =
  if a.kind != b.kind: return false
  case a.kind
  of atNone:
    return true
  of atVar:
    return a.varname == b.varname
  of atArg:
    return a.argindex == b.argindex
  of atIntLit:
    return a.intval == b.intval
proc cmpDup*(a, b: TACode): bool =
  if a.kind != b.kind: return false
  
  case a.kind
  of taAdd..taSet:
    return cmpDup(a.left, b.left) and cmpDup(a.right, b.right)
  of taVar:
    return cmpDup(a.value, b.value)
  of taCall:
    if a.fnlabel != b.fnlabel: return false
    if a.args.len != b.args.len: return false
    if not a.isPure or not b.isPure: return false
    for i in 0..<a.args.len:
      if not cmpDup(a.args[i], b.args[i]): return false
    return true
  of taGoto:
    return false
  of taIf:
    return false
  of taRet:
    return false
  of taFn:
    return false

proc hasIndex*(indexes: seq[(int, string)], i: int): Option[(int, string)] =
  for p in indexes:
    let (index, _) = p
    if index == i:
      return some(p)
  return none((int, string))
proc optLocalDupImpl*(ctx: TAContext, r: var TAContext, codeindexes: seq[int]) =
  var elimindexes = newSeq[(int, string)]()
  for i in 0..<codeindexes.len:
    let left = ctx.codes[codeindexes[i]]
    for j in 0..<codeindexes.len:
      if i >= j: continue
      let right = ctx.codes[codeindexes[j]]
      if cmpDup(left, right):
        elimindexes.add((codeindexes[j], left.getname()))
    let opt = elimindexes.hasIndex(codeindexes[i])
    if opt.isSome:
      let (index, leftname) = opt.get
      r.add(codeVar(left.getname(), 0, atomVar(leftname)))
    else:
      ctx.reloc(r, codeindexes[i])
      r.add(left)
proc optLocalDup*(ctx: TAContext): TAContext =
  result = newTAContext()
  var codeindexes = newSeq[int]()
  for i in 0..<ctx.codes.len:
    if ctx.revlabels.hasKey(i):
      ctx.optLocalDupImpl(result, codeindexes)
      codeindexes = @[]
    codeindexes.add(i)
  ctx.optLocalDupImpl(result, codeindexes)

proc replaceAlias*(ctx: var TAContext, name: string, by: string) =
  for atom in ctx.atoms:
    if atom.kind == atVar and atom.varname == name:
      atom.varname = by
proc optElimAlias*(ctx: TAContext): TAContext =
  result = newTAContext()
  var replaces = newSeq[(string, string)]()
  for i in 0..<ctx.codes.len:
    if ctx.codes[i].kind == taVar and ctx.codes[i].value.kind == atVar:
      replaces.add((ctx.codes[i].varname, ctx.codes[i].value.varname))
    else:
      ctx.reloc(result, i)
      result.add(ctx.codes[i])
  for r in replaces:
    let (left, right) = r
    result.replaceAlias(left, right)

proc optElimRetDeadcode*(ctx: TAContext): TAContext =
  result = newTAContext()
  var indead = false
  for i in 0..<ctx.codes.len:
    if ctx.revlabels.hasKey(i):
      indead = false
    elif indead:
      continue
    elif ctx.codes[i].kind == taGoto or ctx.codes[i].kind == taRet:
      indead = true
    ctx.reloc(result, i)
    result.add(ctx.codes[i])

proc findInlineFn*(inlines: seq[(string, TAAtom, seq[TACode])], fn: string): Option[(TAAtom, seq[TACode])] =
  for inline in inlines:
    let (iname, at, icodes) = inline
    if iname == fn:
      return some((at, icodes))
  return none((TAAtom, seq[TACode]))
proc optInline*(ctx: TAContext): TAContext =
  result = newTAContext()
  var infn = false
  var fnlabel = ""
  var codes = newSeq[TACode]()
  var inlines = newSeq[(string, TAAtom, seq[TACode])]()
  for i in 0..<ctx.codes.len:
    if ctx.codes[i].kind == taRet:
      if codes.len <= 5:
        inlines.add((fnlabel, ctx.codes[i].ret, codes))
      fnlabel = ""
      codes = @[]
      infn = false
    elif ctx.codes[i].kind == taFn:
      fnlabel = ctx.codes[i].fnname
      infn = true
    else:
      codes.add(ctx.codes[i])
  var indead = false
  for i in 0..<ctx.codes.len:
    if ctx.codes[i].kind == taFn:
      let opt = inlines.findInlineFn(ctx.codes[i].fnname)
      if opt.isSome:
        indead = true
        continue
    if indead and ctx.codes[i].kind == taRet:
      indead = false
      continue
    elif indead:
      continue

    if ctx.codes[i].kind == taCall:
      let opt = inlines.findInlineFn(ctx.codes[i].fnlabel)
      if opt.isSome:
        let (atom, codes) = opt.get
        let s = result.codes.len
        for c in codes:
          result.add(c)
        result.add(codeVar(ctx.codes[i].calldstname, 0, atom))
        let e = result.codes.len-1
        for i, arg in ctx.codes[i].args:
          for atom in result.atoms(s, e):
            if atom.kind == atArg and atom.argindex == i:
              atom = arg
        continue
    ctx.reloc(result, i)
    result.add(ctx.codes[i])

proc optimize*(ctx: var TAContext): TAContext =
  result = ctx
  result = result.optElimRetDeadcode()
  result = result.optLocalDup()
  result = result.optElimAlias()
  result = result.optInline()
  result = result.optLocalDup()
  result = result.optElimAlias()

proc countCode*(ctx: TAContext): int =
  ctx.codes.len
