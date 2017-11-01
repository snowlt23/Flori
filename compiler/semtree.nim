
import sast

import tables, hashes
import options

type
  SemExprKind* = enum
    seIdent
    seAttr
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
    scope*: SemScope
    typ*: Option[SemSym]
    case kind*: SemExprKind
    of seIdent:
      idname*: string
    of seAttr:
      attrname*: string
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
    scope*: SemScope
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
      cfuncinfix*: bool
  SemType* = ref object
    sexpr*: SExpr
    scope*: SemScope
    case kind*: SemTypeKind
    of stStruct:
      discard
    of stCType:
      ctypename*: string
      ctypeheader*: Option[string]
    of stProtocol:
      discard

  SemSym* = ref object
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
  VarIdent* = object
    name*: string
  ProcIdent* = object
    name*: string
  TypeIdent* = object
    name*: string
  ScopeIdent* = object
    name*: string
  SemScope* = ref object
    name*: string
    top*: SemScope
    varidents*: Table[VarIdent, SemExpr]
    procidents*: Table[ProcIdent, ProcIdentGroup]
    typeidents*: Table[TypeIdent, SemType]
    toplevels*: seq[SemExpr]

proc hash*(varid: VarIdent): Hash = hash(varid.name)
proc hash*(procid: ProcIdent): Hash = hash(procid.name)
proc hash*(typeid: TypeIdent): Hash = hash(typeid.name)
proc hash*(scopeid: ScopeIdent): Hash = hash(scopeid.name)

proc newSemScope*(name: string): SemScope =
  new result
  result.name = name
  result.top = result
  result.varidents = initTable[VarIdent, SemExpr]()
  result.procidents = initTable[ProcIdent, ProcIdentGroup]()
  result.typeidents = initTable[TypeIdent, SemType]()
  result.toplevels = @[]

proc extendSemScope*(scope: SemScope): SemScope =
  new result
  result.name = scope.name
  result.top = scope.top
  result.procidents = scope.procidents
  result.typeidents = scope.typeidents
  result.toplevels = @[]

proc semident*(scope: SemScope, name: string): SemExpr =
  SemExpr(sexpr: newSNil(internalSpan), typ: none(SemSym), kind: seIdent, scope: scope, idname: name)

proc semsym*(scope: SemScope, name: SemExpr): SemSym =
  SemSym(scope: scope, name: name, kind: symUnresolve)

# FIXME: scope
proc `==`*(a, b: SemExpr): bool =
  if a.kind != b.kind: return false
  if a.kind == seIdent:
    return a.idname == b.idname
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

proc getVar*(scope: SemScope, vi: VarIdent): Option[SemExpr] =
  if not scope.varidents.hasKey(vi):
    return none(SemExpr)
  return some scope.varidents[vi]
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

proc addVar*(scope: SemScope, vi: VarIdent, t: SemExpr): bool =
  if scope.getVar(vi).isSome: return false
  scope.varidents[vi] = t
  return true
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

proc getReturnType*(semfunc: SemFunc): SemSym =
  case semfunc.kind
  of sfCFunc:
    semfunc.cfunctype.returntype
  of sfFunc:
    semfunc.functype.returntype

proc isVoidType*(semsym: SemSym): bool =
  semsym.name.kind == seIdent and semsym.name.idname == "Void"
