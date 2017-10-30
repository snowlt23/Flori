
import sast

import tables, hashes
import options

type
  SemExprKind* = enum
    seIdent
    seSym
    seFuncCall
    seIf
    seWhile
    seSet
    seInt
    seString
  SemFuncKind* = enum
    sfFunc
    sfCFunc
  SemTypeKind* = enum
    stStruct
    stCType
    stProtocol
  SemSymKind* = enum
    symUnresolve
    symSemFunc
    symSemType

type
  FuncType* = object
    argtypes*: seq[SemSym]
    returntype*: SemSym
  SemExpr* = ref object
    sexpr*: SExpr
    typ*: Option[SemSym]
    case kind*: SemExprKind
    of seIdent:
      nameid*: string
    of seSym:
      sym*: SemSym
    of seFuncCall:
      fn*: SemSym
      args*: seq[SemExpr]
    of seIf:
      ifcond*: SemExpr
      iftrue*: SemExpr
      iffalse*: SemExpr
    of seWhile:
      whilecond*: SemExpr
      whilebody*: seq[SemExpr]
    of seSet:
      setplace*: SemExpr
      setvalue*: SemExpr
    of seInt:
      intval*: int64
    of seString:
      strval*: string
  SemFunc* = ref object
    sexpr*: SExpr
    case kind*: SemFuncKind
    of sfFunc:
      funcname*: string
      functype*: FuncType
      funcargs*: seq[SemExpr]
      funcbody*: seq[SemExpr]
    of sfCFunc:
      cfuncname*: string
      cfunctype*: FuncType
      cfuncheader*: Option[string]
  SemType* = ref object
    sexpr*: SExpr
    case kind*: SemTypeKind
    of stStruct:
      discard
    of stCType:
      ctypename*: string
      ctypeheader*: Option[string]
    of stProtocol:
      discard

  SemSym* = object
    scope*: SemScope
    name*: SemExpr
    case kind*: SemSymKind
    of symUnresolve:
      discard
    of symSemFunc:
      sf*: SemFunc
    of symSemType:
      st*: SemType

  ProcIdentDecl* = object
    name*: string
    args*: seq[SemSym]
    value*: SemFunc
  ProcIdentGroup* = object
    idents*: seq[ProcIdentDecl]
  ProcIdent* = object
    name*: string
  TypeIdent* = object
    name*: string
  ScopeIdent* = object
    name*: string
  SemScope* = ref object
    top*: SemScope
    procidents*: Table[ProcIdent, ProcIdentGroup]
    typeidents*: Table[TypeIdent, SemType]
    toplevels*: seq[SemExpr]

proc hash*(procid: ProcIdent): Hash = hash(procid.name)
proc hash*(typeid: TypeIdent): Hash = hash(typeid.name)
proc hash*(scopeid: ScopeIdent): Hash = hash(scopeid.name)

proc newSemScope*(): SemScope =
  new result
  result.top = result
  result.procidents = initTable[ProcIdent, ProcIdentGroup]()
  result.typeidents = initTable[TypeIdent, SemType]()
  result.toplevels = @[]

proc extendSemScope*(scope: SemScope): SemScope =
  new result
  result.top = scope.top
  result.procidents = scope.procidents
  result.typeidents = scope.typeidents
  result.toplevels = @[]

proc semsym*(scope: SemScope, name: SemExpr): SemSym =
  SemSym(scope: scope, name: name, kind: symUnresolve)

# FIXME: scope
proc `==`*(a, b: SemExpr): bool =
  if a.kind != b.kind: return false
  if a.kind == seIdent:
    return a.nameid == b.nameid
  else:
    return false
# FIXME: scope
proc `==`*(a, b: SemSym): bool =
  a.name == b.name
proc `==`*(a, b: ProcIdentDecl): bool =
  if a.name != b.name: return false
  if a.args.len != b.args.len: return false
  for i in 0..<a.args.len:
    if a.args[i] != b.args[i]: return false
  return true

proc initProcIdentGroup*(): ProcIdentGroup =
  result.idents = @[]

proc toProcIdent*(pi: ProcIdentDecl): ProcIdent =
  ProcIdent(name: pi.name)
proc toProcIdentDecl*(semdecl: SemFunc): ProcIdentDecl =
  if semdecl.kind == sfCFunc:
    result.name = semdecl.cfuncname
    result.args = semdecl.cfunctype.argtypes
    result.value = semdecl
  else:
    result.name = semdecl.funcname
    result.args = semdecl.functype.argtypes
    result.value = semdecl

proc getProc*(scope: SemScope, pi: ProcIdentDecl): Option[SemFunc] =
  if not scope.procidents.hasKey(pi.toProcIdent):
    return none(SemFunc)
  let group = scope.procidents[pi.toProcIdent]
  for decl in group.idents:
    if pi == decl:
      return some decl.value
  return none(SemFunc)
proc getType*(scope: SemScope, ti: TypeIdent): Option[SemType] =
  if not scope.typeidents.hasKey(ti):
    return none(SemType)
  return some scope.typeidents[ti]

proc addTopExpr*(scope: SemScope, e: SemExpr) =
  scope.toplevels.add(e)

proc addProc*(scope: SemScope, pi: ProcIdentDecl): bool =
  if scope.getProc(pi).isSome: return false
  if not scope.procidents.hasKey(pi.toProcIdent):
    scope.procidents[pi.toProcIdent] = initProcIdentGroup()
  scope.procidents[pi.toProcIdent].idents.add(pi)
  return true
proc addType*(scope: SemScope, ti: TypeIdent, t: SemType): bool =
  if scope.getType(ti).isSome: return false
  scope.typeidents[ti] = t
  return true

proc initScopeIdent*(name: string): ScopeIdent =
  result.name = name
