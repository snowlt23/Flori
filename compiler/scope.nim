
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

proc newScope*(name: Name): Scope =
  new result
  result.name = name
  result.top = result
  result.level = 0
  result.decls = initTable[Name, Symbol]()
  result.procdecls = initTable[Name, ProcDeclGroup]()
  result.toplevels = @[]

proc extendScope*(scope: Scope): Scope =
  new result
  result.name = scope.name
  result.top = scope.top
  result.level = scope.level + 1
  result.decls = scope.decls
  result.procdecls = scope.procdecls
  result.toplevels = @[]

proc `==`*(a, b: Scope): bool =
  a.name == b.name and a.level == b.level

proc match*(a, b: FExpr): bool
proc match*(a, b: Symbol): bool =
  if b.kind == symbolGenerics:
    return true
  elif a.kind == symbolTypeGenerics and b.kind == symbolTypeGenerics:
    if a != b: return false
    if a.types.len != b.types.len: return false
    for i in 0..<a.types.len:
      if not a.types[i].match(b.types[i]):
        return false
    return true
  else:
    return a == b
proc match*(a, b: FExpr): bool =
  if a.kind == fexprSymbol and b.kind == fexprSymbol:
    return a.symbol.match(b.symbol)
  else:
    return false

proc procname*(name: Name, argtypes: seq[Symbol]): ProcName =
  ProcName(name: name, argtypes: argtypes)

proc match*(a: ProcName, b: ProcDecl): bool =
  if a.name != b.name: return false
  if b.isInternal: return true
  if a.argtypes.len != b.argtypes.len: return false
  for i in 0..<a.argtypes.len:
    if not a.argtypes[i].match(b.argtypes[i]): return false
  return true

proc initProcIdentGroup*(): ProcDeclGroup =
  result.decls = @[]

proc getDecl*(scope: Scope, n: Name): Option[Symbol] =
  if not scope.decls.hasKey(n):
    if scope == scope.top:
      return none(Symbol)
    else:
      return scope.top.getDecl(n)
  return some scope.decls[n]
proc getFunc*(scope: Scope, pd: ProcName): Option[ProcDecl] =
  if not scope.procdecls.hasKey(pd.name):
    if scope == scope.top:
      return none(ProcDecl)
    else:
      return scope.top.getFunc(pd)
  let group = scope.procdecls[pd.name]
  for decl in group.decls:
    if pd.match(decl):
      return some(decl)
  if scope == scope.top:
    return none(ProcDecl)
  else:
    return scope.top.getFunc(pd)

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

proc createImportSymbol*(sym: Symbol): Symbol =
  Symbol(scope: sym.scope, isImported: true, name: sym.name, kind: sym.kind)
proc importScope*(scope: Scope, importscope: Scope) =
  for key, sym in importscope.decls:
    if not sym.isImported:
      discard scope.addDecl(key, sym.createImportSymbol())
  for key, group in importscope.procdecls:
    for decl in group.decls:
      if not decl.sym.isImported:
        var importdecl = decl
        importdecl.sym = decl.sym.createImportSymbol()
        discard scope.addFunc(importdecl)

proc isType*(sym: Symbol, name: string): bool =
  $sym.name == name
proc isBoolType*(sym: Symbol): bool =
  sym.isType("Bool")
proc isVoidType*(sym: Symbol): bool =
  sym.isType("Void")
