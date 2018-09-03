
import tables
import options
import strutils, sequtils

import linmem, image, fexpr, symbol, fnmatch

proc newScope*(name: IString, path: string): Scope =
  result = genScope(ScopeObj())
  result.name = name
  result.top = result
  result.level = 0
  result.decls = ilistNil[TupleTable[Symbol]]()
  result.procdecls = ilistNil[TupleTable[ProcDeclGroup]]()
  result.imports = ilistNil[TupleTable[Scope]]()
  result.exports = ilistNil[TupleTable[Scope]]()

proc extendScope*(scope: Scope): Scope =
  result = genScope(ScopeObj())
  result.name = scope.name
  result.top = scope.top
  result.level = scope.level + 1
  result.decls = scope.decls
  result.procdecls = scope.procdecls
  result.imports = scope.imports
  result.exports = scope.exports

proc procname*(name: string, argtypes: seq[Symbol], generics = newSeq[Symbol]()): ProcName =
  ProcName(name: name, argtypes: argtypes, generics: generics)

proc spec*(a, b: Symbol): bool =
  if a.kind == symbolType and b.kind == symbolType:
    return a == b
  elif a.kind == symbolTypeGenerics and b.kind == symbolTypeGenerics:
    if a.name != b.name: return false
    if a.types.len != b.types.len: return false
    for i in 0..<a.types.len:
      if not a.types[i].spec(b.types[i]):
        return false
    return true
  elif a.kind == symbolFuncType and b.kind == symbolFuncType:
    if a.argtypes.len != b.argtypes.len: return false
    for i in 0..<a.argtypes.len:
      if not a.argtypes[i].spec(b.argtypes[i]):
        return false
    if not a.rettype.spec(b.rettype): return false
    return true
  elif a.kind == symbolIntLit and b.kind == symbolIntLit:
    return a.intval == b.intval
  elif a.kind == symbolRef and b.kind == symbolRef:
    return a.wrapped.spec(b.wrapped)
  elif a.kind == symbolVar and b.kind == symbolRef:
    return a.wrapped.spec(b.wrapped)
  elif a.kind == symbolRef:
    return a.wrapped.spec(b)
  elif a.kind == symbolVar:
    return a.wrapped.spec(b)
  else:
    return false
proc spec*(a: ProcName, b: ProcDecl): bool =
  if a.generics.len != b.generics.len: return false
  if a.argtypes.len != b.argtypes.len: return false
  for i in 0..<a.generics.len:
    if not a.generics[i].spec(b.generics[i]): return false
  for i in 0..<a.argtypes.len:
    if not a.argtypes[i].spec(b.argtypes[i]): return false
  return true

proc initProcIdentGroup*(): ProcDeclGroup =
  result.decls = ilistNil[ProcDecl]()

proc getDecl*(scope: Scope, n: string, importscope = true): Option[Symbol] =
  let opt = scope.decls.find(n)
  if opt.isSome:
    return opt
  if importscope:
    for s in scope.imports:
      let opt = s.value.getDecl(n, importscope = s.name.isCurrentScope())
      if opt.isSome:
        return opt
    return none(Symbol)
  else:
    return none(Symbol)
proc getFnDecl*(scope: Scope, n: string): Option[Symbol] =
  var fns = newSeq[ProcDecl]()
  let opt = scope.procdecls.find(n)
  if opt.isSome:
    for d in opt.get.decls:
      fns.add(d)
  for s in scope.imports:
    let opt = s.value.procdecls.find(n)
    if opt.isSome:
      for d in opt.get.decls:
        fns.add(d)

  if fns.len == 1:
    return some(fns[0].sym)
  else:
    return none(Symbol)
    
proc getSpecType*(scope: Scope, n: string, types: seq[Symbol], importscope = true): Option[Symbol] =
  let opt = scope.decls.find(n)
  if opt.isSome:
    if opt.get.types.len != types.len: return none(Symbol)
    for i in 0..<opt.get.types.len:
      if opt.get.types[i] != types[i]:
        return none(Symbol)
    return opt

  if importscope:
    for s in scope.imports:
      let opt = s.value.getSpecType(n, types, importscope = s.name.isCurrentScope())
      if opt.isSome:
        return opt
    return none(Symbol)
  else:
    return none(Symbol)

proc getSpecFunc*(scope: Scope, pd: ProcName, importscope = true): Option[ProcDecl] =
  let opt = scope.procdecls.find(pd.name)
  if opt.isSome:
    let group = opt.get
    for decl in group.decls:
      if pd.spec(decl):
        return some(decl)

  if importscope:
    for s in scope.imports:
      let opt = s.value.getSpecFunc(pd, importscope = s.name.isCurrentScope())
      if opt.isSome:
        return opt
    return none(ProcDecl)
  else:
    return none(ProcDecl)

proc addDecl*(scope: Scope, n: IString, v: Symbol): bool =
  if scope.getDecl($n).isSome: return false
  scope.obj.decls.add(TupleTable[Symbol](name: n, value: v))
  return true

proc addFunc*(scope: Scope, decl: ProcDecl): bool =
  let pn = ProcName(name: $decl.name, argtypes: toSeq(decl.argtypes.items))
  if scope.getFunc(pn).isSome: return false
  let opt = scope.procdecls.find($decl.name)
  if opt.isNone:
    scope.obj.procdecls.add(TupleTable[ProcDeclGroup](name: decl.name, value: initProcIdentGroup()))
  
  for g in scope.obj.procdecls.mitems:
    if g.name == decl.name:
      g.value.decls.add(decl)
      return true
  assert(false)
proc addSpecFunc*(scope: Scope, decl: ProcDecl) =
  let opt = scope.procdecls.find($decl.name)
  if opt.isNone:
    scope.obj.procdecls.add(TupleTable[ProcDeclGroup](name: decl.name, value: initProcIdentGroup()))
  for g in scope.obj.procdecls.mitems:
    if g.name == decl.name:
      g.value.decls.add(decl)
      return

proc importScope*(scope: Scope, name: IString, importscope: Scope) =
  scope.obj.imports.add(TupleTable[Scope](name: name, value: importscope))
  for exportscope in importscope.exports:
    scope.obj.imports.add(exportscope)

proc isType*(sym: Symbol, name: string): bool =
  $sym.name == name
proc isBoolType*(sym: Symbol): bool =
  sym.isType("Bool")
proc isVoidType*(sym: Symbol): bool =
  sym.isType("Void")
