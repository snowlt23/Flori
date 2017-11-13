  
import fexpr

import tables, hashes
import options
import strutils

type
  Symbol* = object
    scope*: Scope
    fexpr*: FExpr
  Name* = object
    names*: seq[string]
  ProcDecl* = object
    isInternal*: bool
    name*: Name
    argtypes*: seq[Name]
    sym*: Symbol
  ProcName* = object
    name*: Name
    argtypes*: seq[Name]
  ProcDeclGroup* = object
    decls*: seq[ProcDecl]
  Scope* = ref object
    name*: Name
    top*: Scope
    level*: int
    decls*: Table[Name, Symbol]
    procdecls*: Table[Name, ProcDeclGroup]

proc hash*(name: Name): Hash = hash(name.names.join("_"))
proc `==`*(a, b: Name): bool =
  if a.names.len != b.names.len: return false
  for i in 0..<a.names.len:
    if a.names[i] != b.names[i]:
      return false
  return true

proc match*(a: ProcName, b: ProcDecl): bool =
  if a.name != b.name: return false
  if b.isInternal: return true
  if a.argtypes.len != b.argtypes.len: return false
  for i in 0..<a.argtypes.len:
    if a.argtypes[i] != b.argtypes[i]: return false
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
proc getProc*(scope: Scope, pd: ProcName): Option[Symbol] =
  if not scope.procdecls.hasKey(pd.name):
    if scope == scope.top:
      return none(Symbol)
    else:
      return scope.top.getProc(pd)
  let group = scope.procdecls[pd.name]
  for decl in group.decls:
    if pd.match(decl):
      return some(decl.sym)
  if scope == scope.top:
    return none(Symbol)
  else:
    return scope.top.getProc(pd)

proc addDecl*(scope: Scope, n: Name, v: Symbol): bool =
  if scope.getDecl(n).isSome: return false
  scope.decls[n] = v
  return true
proc addFunc*(scope: Scope, decl: ProcDecl, sym: Symbol): bool =
  let pn = ProcName(name: decl.name, argtypes: decl.argtypes)
  if scope.getProc(pn).isSome: return false
  if not scope.procdecls.hasKey(decl.name):
    scope.procdecls[decl.name] = initProcIdentGroup()
  scope.procdecls[decl.name].decls.add(decl)
  return true
