
import tables
import options
import strutils

import image, fexpr, symbol

proc newFScope*(name: string, path: string): FScope =
  genFScope(FScopeObj(
    name: istring(name),
    top: FScope(index: -1),
    level: 0,
    imports: ilistNil[TupleTable[FScope]](),
    decls: ilistNil[TupleTable[Symbol]](),
    procdecls: ilistNil[TupleTable[ProcDeclGroup]]()
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

proc name*(scope: FScope): IString = scope.obj.name
proc word*(scope: FScope): Option[FExpr] = scope.obj.word
proc `word=`*(scope: FScope, opt: Option[FExpr]) = scope.obj.word = opt
proc imports*(scope: FScope): var IList[TupleTable[FScope]] = scope.obj.imports
proc decls*(scope: FScope): var IList[TupleTable[Symbol]] = scope.obj.decls
proc procdecls*(scope: FScope): var IList[TupleTable[ProcDeclGroup]] = scope.obj.procdecls

proc procname*(name: string, argtypes: seq[Symbol], generics = newSeq[Symbol]()): ProcName =
  ProcName(name: name, argtypes: argtypes, generics: generics)

proc initProcDeclGroup*(): ProcDeclGroup =
  ProcDeclGroup(decls: ilistNil[ProcDecl]())

proc match*(a, b: Symbol): bool =
  if a.kind == symbolGenerics:
    return true
  elif b.kind == symbolGenerics:
    return true
  elif a.kind == symbolTypeGenerics and b.kind == symbolTypeGenerics:
    if a.name != b.name: return false
    if a.types.len != b.types.len: return false
    for i in 0..<a.types.len:
      if not a.types[i].match(b.types[i]):
        return false
    return true
  elif a.kind == symbolUnion:
    for t in a.uniontypes:
      if t.match(b):
        return true
    return false
  elif b.kind == symbolUnion:
    for t in b.uniontypes:
      if t.match(a):
        return true
    return false
  elif a.kind == symbolRef and b.kind == symbolRef:
    return a.wrapped.match(b.wrapped)
  elif a.kind == symbolRef and b.kind == symbolVar:
    return a.wrapped.match(b.wrapped)
  elif a.kind == symbolLink:
    return a.wrapped.match(b)
  elif b.kind == symbolLink:
    return a.match(b.wrapped)
  elif b.kind == symbolVar:
    return a.match(b.wrapped)
  elif a.scope.name == b.scope.name and a.name == b.name:
    return true
  else:
    return false
proc match*(a: ProcDecl, b: ProcName): bool =
  if a.internalproc.isSome: return true
  if a.name != b.name: return false
  if a.sym.fexpr.internal.obj.argtypes.get.len != b.argtypes.len: return false
  for i in 0..<a.sym.fexpr.internal.obj.argtypes.get.len:
    if not a.sym.fexpr.internal.obj.argtypes.get[i].match(b.argtypes[i]):
      return false
  return true

proc spec*(a, b: Symbol): bool =
  if a.kind == symbolType and b.kind == symbolType:
    return a.scope.name == b.scope.name and a.name == b.name
  elif a.kind == symbolTypeGenerics and b.kind == symbolTypeGenerics:
    if a.name != b.name: return false
    if a.types.len != b.types.len: return false
    for i in 0..<a.types.len:
      if not a.types[i].spec(b.types[i]):
        return false
    return true
  elif a.kind == symbolRef and b.kind == symbolRef:
    return a.wrapped.spec(b.wrapped)
  elif a.kind == symbolVar and b.kind == symbolRef:
    return a.wrapped.spec(b.wrapped)
  elif a.kind == symbolLink:
    return a.wrapped.spec(b)
  elif b.kind == symbolLink:
    return a.spec(b.wrapped)
  elif a.kind == symbolRef:
    return a.wrapped.spec(b)
  elif a.kind == symbolVar:
    return a.wrapped.spec(b)
  else:
    return false
proc spec*(a: ProcDecl, b: ProcName): bool =
  if a.sym.fexpr.internal.obj.argtypes.get.len != b.argtypes.len: return false
  for i in 0..<a.sym.fexpr.internal.obj.argtypes.get.len:
    if not a.sym.fexpr.internal.obj.argtypes.get[i].spec(b.argtypes[i]): return false
  return true

proc find*(lst: IList[TupleTable[Symbol]], n: string): Option[Symbol] =
  for f in lst:
    if f.name == n:
      return some(f.value)
  return none(Symbol)
proc find*(lst: IList[TupleTable[ProcDeclGroup]], n: string): Option[ProcDeclGroup] =
  for f in lst:
    if f.name == n:
      return some(f.value)
  return none(ProcDeclGroup)
proc find*(lst: IList[TupleTable[FScope]], n: string): Option[FScope] =
  for f in lst:
    if f.name == n:
      return some(f.value)
  return none(FScope)

proc getDecl*(scope: FScope, n: string): Option[Symbol] =
  let opt = scope.decls.find(n)
  if opt.isSome:
    return opt

  for imscope in scope.imports:
    let opt = imscope.value.decls.find(n)
    if opt.isSome:
      return opt

  return none(Symbol)
proc getWords*(scope: FScope, name: string): seq[ProcDecl] =
  result = @[]

  let groupopt = scope.procdecls.find(name)
  if groupopt.isSome:
    for decl in groupopt.get.decls:
      result.add(decl)

  for scopetup in scope.imports:
    let groupopt = scopetup.value.procdecls.find(name)
    if groupopt.isSome:
      for decl in groupopt.get.decls:
        result.add(decl)

# proc getFunc*(scope: FScope, pd: ProcName): Option[ProcDecl] =
#   let groupopt = scope.procdecls.find(pd.name)
#   if groupopt.isSome:
#     for decl in groupopt.get.value.value.decls:
#       if decl.match(pd):
#         return some(decl)

#   for scopetup in scope.imports:
#     let groupopt = scopetup.value.procdecls.find(pd.name)
#     if groupopt.isSome:
#       for decl in groupopt.get.value.value.decls:
#         if decl.match(pd):
#           return some(decl)

#   return none(ProcDecl)
proc getSpecFunc*(scope: FScope, pd: ProcName): Option[ProcDecl] =
  let groupopt = scope.procdecls.find(pd.name)
  if groupopt.isSome:
    for decl in groupopt.get.decls:
      if decl.spec(pd):
        return some(decl)

  for imscope in scope.imports:
    let groupopt = imscope.value.procdecls.find(pd.name)
    if groupopt.isSome:
      for decl in groupopt.get.decls:
        if decl.spec(pd):
          return some(decl)

  return none(ProcDecl)

proc addDecl*(scope: FScope, n: IString, v: Symbol) =
  scope.decls.add(TupleTable[Symbol](name: n, value: v))
proc addWord*(scope: FScope, decl: ProcDecl) =
  var cur = scope.procdecls
  while true:
    if cur.isNil:
      var group = initProcDeclGroup()
      group.decls.add(decl)
      scope.procdecls.add(TupleTable[ProcDeclGroup](name: decl.name, value: group))
      return

    if $cur.value.name == $decl.name:
      cur.value.value.decls.add(decl)
      return
    cur = cur.next

proc importFScope*(scope: FScope, name: IString, importscope: FScope) =
  scope.imports.add(TupleTable[FScope](name: name, value: importscope))
  # for name, exportscope in importscope.exportscopes:
  #   scope.importscopes[name] = exportscope

proc isType*(sym: Symbol, name: string): bool =
  $sym.name == name
proc isBoolType*(sym: Symbol): bool =
  sym.isType("Bool")
proc isVoidType*(sym: Symbol): bool =
  sym.isType("Void")
