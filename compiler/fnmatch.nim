
import tables
import options
import strutils, sequtils

import types, fexpr, metadata

proc isCurrentScope*(n: Name): bool = $n == "flori_current_scope"

proc match*(a, b: Symbol): Matched =
  if b.kind == symbolGenerics:
    return Matched(kind: matchType)
  elif a.kind == symbolTypeGenerics and b.kind == symbolTypeGenerics:
    if a.name != b.name: return Matched(kind: matchNone)
    if a.types.len != b.types.len: return Matched(kind: matchNone)
    for i in 0..<a.types.len:
      if not a.types[i].match(b.types[i]).isMatch:
        return Matched(kind: matchNone)
    return Matched(kind: matchType)
  elif a.kind == symbolFuncType and b.kind == symbolFuncType:
    if a.argtypes.len != b.argtypes.len: return Matched(kind: matchNone)
    for i in 0..<a.argtypes.len:
      if not a.argtypes[i].match(b.argtypes[i]).isMatch:
        return Matched(kind: matchNone)
    if not a.rettype.match(b.rettype).isMatch: return Matched(kind: matchNone)
    return Matched(kind: matchType)
  elif a.kind == symbolRef and b.kind == symbolRef:
    return a.wrapped.match(b.wrapped)
  elif a.kind == symbolVar and b.kind == symbolRef:
    return a.wrapped.match(b.wrapped)
  elif b.kind == symbolVar:
    return a.match(b.wrapped)
  elif a.kind == symbolRef:
    return a.wrapped.match(b)
  elif a.kind == symbolVar:
    return a.wrapped.match(b)
  elif a == b:
    return Matched(kind: matchType)
  elif a.fexpr.hasConverters:
    for conv in a.fexpr.converters.converters:
      let opt = conv.defn.ret.symbol.match(b)
      if opt.isMatch:
        return Matched(kind: matchConvert, convsym: conv.defn.name.symbol)
    return Matched(kind: matchNone)
  else:
    return Matched(kind: matchNone)

proc match*(a: ProcName, b: ProcDecl): Option[seq[Matched]] =
  if a.name != b.name: return none(seq[Matched])
  if b.isInternal: return some(repeat(Matched(kind: matchType), a.argtypes.len))
  if b.isSyntax: return some(repeat(Matched(kind: matchType), a.argtypes.len))
  if a.argtypes.len != b.argtypes.len: return none(seq[Matched])
  var s = newSeq[Matched]()
  for i in 0..<a.argtypes.len:
    let opt = a.argtypes[i].match(b.argtypes[i])
    if not opt.isMatch:
      return none(seq[Matched])
    s.add(opt)
  return some(s)

proc getFunc*(scope: Scope, pd: ProcName, importscope = true): Option[tuple[pd: ProcDecl, matches: seq[Matched]]] =
  if not scope.procdecls.hasKey(pd.name):
    if importscope:
      for scopename, s in scope.importscopes:
        let opt = s.getFunc(pd, importscope = scopename.isCurrentScope())
        if opt.isSome:
          return opt
      return none((ProcDecl, seq[Matched]))
    else:
      return none((ProcDecl, seq[Matched]))

  let group = scope.procdecls[pd.name]
  for decl in group.decls:
    let opt = pd.match(decl)
    if opt.isSome:
      return some((decl, opt.get))

  if importscope:
    for scopename, s in scope.importscopes:
      let opt = s.getFunc(pd, importscope = scopename.isCurrentScope())
      if opt.isSome:
        return opt
    return none((ProcDecl, seq[Matched]))
  else:
    return none((ProcDecl, seq[Matched]))
