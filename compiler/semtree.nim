
import sast

import tables, hashes
import options

type
  SemTypeKind* = enum
    stSymbol
    stIdent
  SemExprKind* = enum
    seSExpr
    seIdent
    seFuncCall
    seIf
    seWhile
    seSet
    seInt
    seString
  SemDeclKind* = enum
    sdRequire
    sdVariable
    sdFunc
    sdStruct
    sdCFunc
    sdCStruct
    sdProtocol
  SemSymKind* = enum
    symSemTypeSym
    symSemExpr
    symSemDecl

type
  FuncType* = object
    argtypes*: seq[SemType]
    returntype*: SemType
  SemType* = ref object
    case kind*: SemTypeKind
    of stSymbol:
      sym*: SemDecl
    of stIdent:
      idscope*: SemScope
      idname*: SemExpr
  SemExpr* = ref object
    typ*: Option[SemType]
    case kind*: SemExprKind
    of seSExpr:
      sexpr*: SExpr
    of seIdent:
      nameid*: string
    of seFuncCall:
      fn*: SemType
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
  SemDecl* = ref object
    case kind*: SemDeclKind
    of sdRequire:
      requiremodule*: SemScope
    of sdVariable:
      discard
    of sdFunc:
      funcname*: string
      functype*: FuncType
      funcargs*: SExpr
      funcbody*: seq[SemExpr]
    of sdStruct:
      discard
    of sdCFunc:
      discard
    of sdCStruct:
      discard
    of sdProtocol:
      discard

  SemSym* = object
    case kind*: SemSymKind
    of symSemTypeSym:
      st*: SemType
    of symSemExpr:
      se*: SemExpr
    of symSemDecl:
      sd*: SemDecl

  ProcIdentDecl* = object
    name*: string
    args*: seq[SemType]
    value*: SemDecl
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
    typeidents*: Table[TypeIdent, SemDecl]
    toplevels*: seq[SemExpr]

proc hash*(procid: ProcIdent): Hash = hash(procid.name)
proc hash*(typeid: TypeIdent): Hash = hash(typeid.name)
proc hash*(scopeid: ScopeIdent): Hash = hash(scopeid.name)

proc newSemScope*(): SemScope =
  new result
  result.top = result
  result.procidents = initTable[ProcIdent, ProcIdentGroup]()
  result.typeidents = initTable[TypeIdent, SemDecl]()
  result.toplevels = @[]

proc extendSemScope*(scope: SemScope): SemScope =
  new result
  result.top = scope.top
  result.procidents = scope.procidents
  result.typeidents = scope.typeidents
  result.toplevels = @[]

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
proc toProcIdentDecl*(semdecl: SemDecl): ProcIdentDecl =
  result.name = semdecl.funcname
  result.args = semdecl.functype.argtypes
  result.value = semdecl

proc getProc*(scope: SemScope, pi: ProcIdentDecl): Option[SemDecl] =
  if not scope.procidents.hasKey(pi.toProcIdent):
    return none(SemDecl)
  let group = scope.procidents[pi.toProcIdent]
  for decl in group.idents:
    if pi == decl:
      return some decl.value
  return none(SemDecl)
proc getType*(scope: SemScope, ti: TypeIdent): Option[SemDecl] =
  if not scope.typeidents.hasKey(ti):
    return none(SemDecl)
  return some scope.typeidents[ti]

proc addProc*(scope: SemScope, pi: ProcIdentDecl): bool =
  if scope.getProc(pi).isSome: return false
  if not scope.procidents.hasKey(pi.toProcIdent):
    scope.procidents[pi.toProcIdent] = initProcIdentGroup()
  scope.procidents[pi.toProcIdent].idents.add(pi)
  return true
proc addType*(scope: SemScope, ti: TypeIdent, t: SemDecl): bool =
  if scope.getType(ti).isSome: return false
  scope.typeidents[ti] = t
  return true

proc initScopeIdent*(name: string): ScopeIdent =
  result.name = name
