
import ../image, ../parser, ../fexpr
import tacode, tagen
import tables
import options

proc replaceAlias*(ctx: var TAContext, name: string, by: string) =
  for atom in ctx.atoms:
    if atom.kind == TAAtomKind.AVar and atom.avar.name == name:
      atom.avar.name = by

proc searchRetName*(ctx: TAContext): Option[string] =
  for code in ctx.codes:
    if code.kind == TACodeKind.Ret:
      if code.ret.value.kind == TAAtomKind.AVar:
        return some(code.ret.value.avar.name)
      else:
        return none(string)
  return none(string)

proc optElimAlias*(ctx: TAContext): TAContext =
  result = newTAContext()
  let retname = ctx.searchRetName()
  var returned = false
  var replaces = newSeq[(string, string)]()
  for i in 0..<ctx.codes.len:
    if ctx.codes[i].kind == TACodeKind.AVar and ctx.codes[i].avar.value.kind == TAAtomKind.AVar:
      replaces.add((ctx.codes[i].avar.name, ctx.codes[i].avar.value.avar.name))
    elif ctx.codes[i].kind == TACodeKind.Set and ctx.codes[i].set.value.kind == TAAtomKind.AVar:
      if retname.isSome and ctx.codes[i].set.name == retname.get:
        result.add(initTACodeRet(ctx.codes[i].set.value))
        returned = true
      else:
        replaces.add((ctx.codes[i].set.name, ctx.codes[i].set.value.avar.name))
    else:
      if ctx.codes[i].kind == TACodeKind.Ret and returned: continue
      result.add(ctx.codes[i])
  for r in replaces:
    let (left, right) = r
    result.replaceAlias(left, right)

proc optElimDeadLabel*(ctx: TAContext): TAContext =
  result = newTAContext()
  var prevlabelpos = -1
  var deadlabels = newSeq[string]()
  for i in 0..<ctx.codes.len:
    if ctx.codes[i].kind == TACodeKind.Label:
      if i - prevlabelpos == 0:
        deadlabels.add(ctx.codes[prevlabelpos].label.name)
      prevlabelpos = i
  if ctx.codes.len-1 - prevlabelpos == 0:
    deadlabels.add(ctx.codes[prevlabelpos].label.name)

  for code in ctx.codes:
    if code.kind == TACodeKind.Label and code.label.name in deadlabels: continue
    if code.kind == TACodeKind.Goto and code.goto.gotolabel in deadlabels: continue
    result.add(code)

# proc findInlineFn*(inlines: seq[(string, TAAtom, seq[TACode])], fn: string): Option[(TAAtom, seq[TACode])] =
#   for inline in inlines:
#     let (iname, at, icodes) = inline
#     if iname == fn:
#       return some((at, icodes))
#   return none((TAAtom, seq[TACode]))
# proc optInline*(ctx: TAContext): TAContext =
#   result = newTAContext()
#   var infn = false
#   var fnlabel = ""
#   var codes = newSeq[TACode]()
#   var inlines = newSeq[(string, TAAtom, seq[TACode])]()
#   for i in 0..<ctx.codes.len:
#     if ctx.codes[i].kind == taRet:
#       if codes.len <= 5:
#         inlines.add((fnlabel, ctx.codes[i].ret, codes))
#       fnlabel = ""
#       codes = @[]
#       infn = false
#     elif ctx.codes[i].kind == taFn:
#       fnlabel = ctx.codes[i].fnname
#       infn = true
#     else:
#       codes.add(ctx.codes[i])
#   var indead = false
#   for i in 0..<ctx.codes.len:
#     if ctx.codes[i].kind == taFn:
#       let opt = inlines.findInlineFn(ctx.codes[i].fnname)
#       if opt.isSome:
#         indead = true
#         continue
#     if indead and ctx.codes[i].kind == taRet:
#       indead = false
#       continue
#     elif indead:
#       continue

#     if ctx.codes[i].kind == taCall:
#       let opt = inlines.findInlineFn(ctx.codes[i].fnlabel)
#       if opt.isSome:
#         let (atom, codes) = opt.get
#         let s = result.codes.len
#         for c in codes:
#           result.add(c)
#         result.add(codeVar(ctx.codes[i].calldstname, 0, atom))
#         let e = result.codes.len-1
#         for i, arg in ctx.codes[i].args:
#           for atom in result.atoms(s, e):
#             if atom.kind == atArg and atom.argindex == i:
#               atom = arg
#         continue
#     ctx.reloc(result, i)
#     result.add(ctx.codes[i])

proc optimize*(ctx: var TAContext): TAContext =
  result = newTAContext()
  for fn in ctx.fns:
    var fnctx = newTAContext()
    fnctx.codes = fn.body
    var optimized = fnctx
    # optimize pass begin
    optimized = optimized.optElimAlias()
    optimized = optimized.optElimDeadLabel()
    # optimize pass end
    result.fns.add(TAFn(fnname: fn.fnname, args: fn.args, retsize: fn.retsize, body: optimized.codes))
