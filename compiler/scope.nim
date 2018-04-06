
import tables
import options
import strutils

import types
export types.SymbolKind
export types.Symbol
export types.Name
export types.ProcDecl
export types.ProcName
export types.ProcDeclGroup
export types.Scope

import fexpr

proc newScope*(ctx: SemanticContext, name: Name, path: string): Scope =
  new result
  result.ctx = ctx
  result.path = path
  result.name = name
  result.top = result
  result.level = 0
  result.decls = initTable[Name, Symbol]()
  result.procdecls = initTable[Name, ProcDeclGroup]()
  result.importscopes = initOrderedTable[Name, Scope]()
  result.exportscopes = initOrderedTable[Name, Scope]()
  result.toplevels = @[]
  result.scopevalues = @[]
  result.scopedepends = @[]

proc extendScope*(scope: Scope): Scope =
  new result
  result.ctx = scope.ctx
  result.path = scope.path
  result.name = scope.name
  result.top = scope.top
  result.level = scope.level + 1
  result.decls = scope.decls
  result.procdecls = scope.procdecls
  result.importscopes = scope.importscopes
  result.exportscopes = scope.exportscopes
  result.toplevels = @[]
  result.scopevalues = @[]
  result.scopedepends = @[]

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
  elif a.kind == symbolOnce and b.kind == symbolOnce:
    return a.wrapped.match(b.wrapped)
  elif b.kind == symbolOnce:
    return a.match(b.wrapped)
  elif a.kind == symbolRef:
    return a.wrapped.match(b)
  elif a.kind == symbolVar:
    return a.wrapped.match(b)
  else:
    return a == b

proc procname*(name: Name, argtypes: seq[Symbol], generics = newSeq[Symbol]()): ProcName =
  ProcName(name: name, argtypes: argtypes, generics: generics)

proc match*(a: ProcName, b: ProcDecl): bool =
  if a.name != b.name: return false
  if b.isInternal: return true
  if b.isSyntax: return true
  if a.argtypes.len != b.argtypes.len: return false
  for i in 0..<a.argtypes.len:
    if not a.argtypes[i].match(b.argtypes[i]): return false
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
  elif a.kind == symbolIntLit and b.kind == symbolIntLit:
    return a.intval == b.intval
  elif a.kind == symbolRef and b.kind == symbolRef:
    return a.wrapped.match(b.wrapped)
  elif a.kind == symbolVar and b.kind == symbolRef:
    return a.wrapped.match(b.wrapped)
  elif a.kind == symbolOnce and b.kind == symbolOnce:
    return a.wrapped.match(b.wrapped)
  elif b.kind == symbolOnce:
    return a.match(b.wrapped)
  elif a.kind == symbolRef:
    return a.wrapped.match(b)
  elif a.kind == symbolVar:
    return a.wrapped.match(b)
  else:
    return false
proc spec*(a: ProcName, b: ProcDecl): bool =
  if a.generics.len != b.generics.len: return false
  for i in 0..<a.generics.len:
    if not a.generics[i].spec(b.generics[i]): return false
  return true

proc initProcIdentGroup*(): ProcDeclGroup =
  result.decls = @[]

proc isCurrentScope*(n: Name): bool = $n == "flori_current_scope"

proc getDecl*(scope: Scope, n: Name, importscope = true): Option[Symbol] =
  if not scope.decls.hasKey(n):
    if importscope:
      for scopename, s in scope.importscopes:
        let opt = s.getDecl(n, importscope = scopename.isCurrentScope())
        if opt.isSome:
          return opt
      return none(Symbol)
    else:
      return none(Symbol)
  return some scope.decls[n]
proc getSpecType*(scope: Scope, n: Name, types: seq[Symbol], importscope = true): Option[Symbol] =
  if scope.decls.hasKey(n):
    if scope.decls[n].types == types:
      return some(scope.decls[n])
    else:
      return none(Symbol)
  else:
    if importscope:
      for scopename, s in scope.importscopes:
        let opt = s.getDecl(n, importscope = scopename.isCurrentScope())
        if opt.isSome:
          return opt
      return none(Symbol)
    else:
      return none(Symbol)
proc getFunc*(scope: Scope, pd: ProcName, importscope = true): Option[ProcDecl] =
  if not scope.procdecls.hasKey(pd.name):
    if importscope:
      for scopename, s in scope.importscopes:
        let opt = s.getFunc(pd, importscope = scopename.isCurrentScope())
        if opt.isSome:
          return opt
      return none(ProcDecl)
    else:
      return none(ProcDecl)

  let group = scope.procdecls[pd.name]
  for decl in group.decls:
    if pd.match(decl):
      return some(decl)

  if importscope:
    for scopename, s in scope.importscopes:
      let opt = s.getFunc(pd, importscope = scopename.isCurrentScope())
      if opt.isSome:
        return opt
    return none(ProcDecl)
  else:
    return none(ProcDecl)
proc getSpecFunc*(scope: Scope, pd: ProcName, importscope = true): Option[ProcDecl] =
  if not scope.procdecls.hasKey(pd.name):
    if importscope:
      for scopename, s in scope.importscopes:
        let opt = s.getSpecFunc(pd, importscope = scopename.isCurrentScope())
        if opt.isSome:
          return opt
      return none(ProcDecl)
    else:
      return none(ProcDecl)

  let group = scope.procdecls[pd.name]
  for decl in group.decls:
    if pd.spec(decl):
      return some(decl)

  if importscope:
    for scopename, s in scope.importscopes:
      let opt = s.getSpecFunc(pd, importscope = scopename.isCurrentScope())
      if opt.isSome:
        return opt
    return none(ProcDecl)
  else:
    return none(ProcDecl)

proc addDecl*(scope: Scope, n: Name, v: Symbol): bool =
  if scope.getDecl(n).isSome: return false
  scope.decls[n] = v
  return true
proc addFunc*(scope: Scope, decl: ProcDecl): bool =
  let pn = ProcName(name: decl.name, argtypes: decl.argtypes)
  if scope.getFunc(pn).isSome: return false
  if not scope.procdecls.hasKey(decl.name):
    scope.procdecls[decl.name] = initProcIdentGroup()
  scope.procdecls[decl.name].decls.add(decl)
  return true
proc addSpecFunc*(scope: Scope, decl: ProcDecl) =
  if not scope.procdecls.hasKey(decl.name):
    scope.procdecls[decl.name] = initProcIdentGroup()
  scope.procdecls[decl.name].decls.add(decl)

proc importScope*(scope: Scope, name: Name, importscope: Scope) =
  scope.importscopes[name] = importscope
  for name, exportscope in importscope.exportscopes:
    scope.importscopes[name] = exportscope

proc isType*(sym: Symbol, name: string): bool =
  $sym.name == name
proc isBoolType*(sym: Symbol): bool =
  sym.isType("Bool")
proc isVoidType*(sym: Symbol): bool =
  sym.isType("Void")
