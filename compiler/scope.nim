
import tables
import options
import strutils

import image, fexpr

proc newFScope*(name: string, path: string): FScope =
  genFScope(FScopeObj(
    name: istring(name),
    top: FScope(index: -1),
    level: 0,
    imports: ilistNil[(IString, FScope)](),
    decls: ilistNil[(IString, Symbol)](),
    procdecls: ilistNil[(IString, ProcDeclGroup)]()
  ))
proc extendFScope*(scope: FScope): FScope =
  genFScope(FScopeObj(
    name: scope.obj.name,
    top: scope,
    level: scope.obj.level+1,
    imports: scope.obj.imports,
    decls: scope.obj.decls,
    procdecls: scope.obj.procdecls
  ))

proc imports*(scope: FScope): var IList[TupleTable[FScope]] = scope.obj.imports
proc decls*(scope: FScope): var IList[TupleTable[Symbol]] = scope.obj.decls
proc procdecls*(scope: FScope): var IList[TupleTable[ProcDeclGroup]] = scope.obj.procdecls

proc procname*(name: string, argtypes: seq[Symbol], generics = newSeq[Symbol]()): ProcName =
  ProcName(name: name, argtypes: argtypes, generics: generics)

proc initProcDeclGroup*(): ProcDeclGroup =
  result.decls = ilistNil[ProcDecl]()

proc match*(a, b: Symbol): bool =
  if b.kind == symbolGenerics:
    return true
  elif a.kind == symbolTypeGenerics and b.kind == symbolTypeGenerics:
    if a.name != b.name: return false
    if a.types.len != b.types.len: return false
    for i in 0..<a.types.len:
      if not a.types[i].match(b.types[i]):
        return false
    return true
  elif a.kind == symbolFuncType and b.kind == symbolFuncType:
    if a.argtypes.len != b.argtypes.len: return false
    for i in 0..<a.argtypes.len:
      if not a.argtypes[i].match(b.argtypes[i]):
        return false
    if not a.rettype.match(b.rettype): return false
    return true
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
    return true
  else:
    return false
proc match*(a: ProcDecl, b: ProcName): bool =
  if a.name != b.name: return false
  if a.argtypes.len != b.argtypes.len: return false
  for i in 0..<a.argtypes.len:
    if not a.argtypes[i].match(b.argtypes[i]):
      return false
  return true
  
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
proc spec*(a: ProcDecl, b: ProcName): bool =
  if a.generics.len != b.generics.len: return false
  if a.argtypes.len != b.argtypes.len: return false
  for i in 0..<a.generics.len:
    if not a.generics[i].spec(b.generics[i]): return false
  for i in 0..<a.argtypes.len:
    if not a.argtypes[i].spec(b.argtypes[i]): return false
  return true

proc find*(lst: IList[TupleTable[Symbol]], n: string): Option[Symbol] =
  for f in lst:
    let (name, sym) = f
    if name == n:
      return some(sym)
  return none(Symbol)
proc find*(lst: IList[TupleTable[ProcDeclGroup]], n: string): Option[IList[TupleTable[ProcDeclGroup]]] =
  var cur = lst
  while true:
    if cur.isNil:
      break
    let (name, group) = cur.value
    if name == n:
      return some(cur)
    cur = cur.next
  return none(IList[TupleTable[ProcDeclGroup]])

proc getDecl*(scope: FScope, n: string): Option[Symbol] =
  let opt = scope.decls.find(n)
  if opt.isSome:
    return opt

  for imscope in scope.imports:
    let opt = imscope.value.decls.find(n)
    if opt.isSome:
      return opt

  return none(Symbol)
proc getFunc*(scope: FScope, pd: ProcName): Option[ProcDecl] =
  let groupopt = scope.procdecls.find(pd.name)
  if groupopt.isSome:
    for decl in groupopt.get.value.value.decls:
      if decl.match(pd):
        return some(decl)

  for scopetup in scope.imports:
    let (name, imscope) = scopetup
    let groupopt = imscope.procdecls.find(pd.name)
    if groupopt.isSome:
      for decl in groupopt.get.value.value.decls:
        if decl.match(pd):
          return some(decl)

  return none(ProcDecl)
proc getSpecFunc*(scope: FScope, pd: ProcName): Option[ProcDecl] =
  let groupopt = scope.procdecls.find(pd.name)
  if groupopt.isSome:
    for decl in groupopt.get.value.value.decls:
      if decl.spec(pd):
        return some(decl)

  for imscope in scope.imports:
    let groupopt = imscope.value.procdecls.find(pd.name)
    if groupopt.isSome:
      for decl in groupopt.get.value.value.decls:
        if decl.spec(pd):
          return some(decl)

  return none(ProcDecl)

proc addDecl*(scope: FScope, n: IString, v: Symbol) =
  scope.decls.add((n, v))
proc addFunc*(scope: FScope, decl: ProcDecl) =
  let opt = scope.procdecls.find($decl.name)
  let group = if opt.isSome:
                opt.get
              else:
                scope.procdecls.add((decl.name, initProcDeclGroup()))
                scope.procdecls.find($decl.name).get
  group.value.value.decls.add(decl)

proc importScope*(scope: FScope, name: IString, importscope: FScope) =
  scope.imports.add((name, importscope))
  # for name, exportscope in importscope.exportscopes:
  #   scope.importscopes[name] = exportscope

proc isType*(sym: Symbol, name: string): bool =
  $sym.name == name
proc isBoolType*(sym: Symbol): bool =
  sym.isType("Bool")
proc isVoidType*(sym: Symbol): bool =
  sym.isType("Void")
