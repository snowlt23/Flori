
import tables
import options
import strutils, sequtils

import linmem, image, fexpr, symbol, localparser

proc isCurrentScope*(n: IString): bool = $n == "flori_current_scope"

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
  else:
    for conv in a.fexpr.metadata.converters:
      let ret = conv.getReturn()
      if ret.isNone: continue
      let opt = ret.get.symbol.match(b)
      if opt.isMatch:
        return Matched(kind: matchConvert, convsym: conv.getName().get.symbol)
    return Matched(kind: matchNone)

proc match*(a: ProcName, b: ProcDecl): Option[seq[Matched]] =
  if a.name != $b.name: return none(seq[Matched])
  if b.internalproc.isSome: return some(repeat(Matched(kind: matchType), a.argtypes.len))
  if b.isSyntax: return some(repeat(Matched(kind: matchType), a.argtypes.len))
  if a.argtypes.len != b.argtypes.len: return none(seq[Matched])
  var s = newSeq[Matched]()
  for i in 0..<a.argtypes.len:
    let opt = a.argtypes[i].match(b.argtypes[i])
    if not opt.isMatch:
      return none(seq[Matched])
    s.add(opt)
  return some(s)

proc find*[T](lst: IList[TupleTable[T]], key: string): Option[T] =
  for e in lst:
    if e.name == key:
      return some(e.value)
  return none(T)

proc getFunc*(scope: Scope, pd: ProcName, importscope = true): Option[tuple[pd: ProcDecl, matches: seq[Matched]]] =
  let group = scope.procdecls.find($pd.name)
  if group.isSome:
    for decl in group.get.decls:
      let opt = pd.match(decl)
      if opt.isSome:
        return some((decl, opt.get))

  if importscope:
    for s in scope.imports:
      let opt = s.value.getFunc(pd, importscope = s.name.isCurrentScope())
      if opt.isSome:
        return opt
    return none((ProcDecl, seq[Matched]))
  else:
    return none((ProcDecl, seq[Matched]))
