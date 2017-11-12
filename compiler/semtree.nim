
import ast

import tables, hashes
import options

type
  SemExprKind* = enum
    seIdent
    seSym
    seGenericsParent
    seGenericsChild
    seFuncCall
    seArray
    seList
    seBlock
    seVar
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
    fexpr*: FExpr
    scope*: SemScope
    typ*: Option[SemSym]
    case kind*: SemExprKind
    of seIdent:
      idname*: string
    of seSym:
      sym*: SemSym
    of seGenericsParent:
      genspec: Option[SemSym]
    of seGenericsChild:
      genparent*: SemExpr
    of seFuncCall:
      fn*: SemSym
      args*: seq[SemExpr]
    of seArray, seList, seBlock:
      body*: seq[SemExpr]
    of seVar:
      varname*: string
      varvalue*: SemExpr
      vartoplevel*: bool
    of seIf:
      ifcond*: SemExpr
      iftrue*: SemExpr
      iffalse*: Option[SemExpr]
    of seWhile:
      whilecond*: SemExpr
      whilebody*: SemExpr
    of seSet:
      setplace*: SemExpr
      setvalue*: SemExpr
    of seInt:
      intval*: int64
    of seString:
      strval*: string
  SemFunc* = ref object
    fexpr*: FExpr
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
    fexpr*: FExpr
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
    level*: int
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
  result.level = 0
  result.varidents = initTable[VarIdent, SemExpr]()
  result.procidents = initTable[ProcIdent, ProcIdentGroup]()
  result.typeidents = initTable[TypeIdent, SemType]()
  result.toplevels = @[]

proc extendSemScope*(scope: SemScope): SemScope =
  new result
  result.name = scope.name
  result.top = scope.top
  result.level = scope.level + 1
  result.varidents = scope.varidents
  result.procidents = scope.procidents
  result.typeidents = scope.typeidents
  result.toplevels = @[]

proc `==`*(a, b: SemScope): bool =
  a.name == b.name and a.level == b.level

proc semident*(scope: SemScope, name: string): SemExpr =
  SemExpr(fexpr: fident(internalSpan, name), typ: none(SemSym), kind: seIdent, scope: scope, idname: name)

proc semsym*(scope: SemScope, name: SemExpr): SemSym =
  SemSym(scope: scope, name: name, kind: symUnresolve)
proc `$`*(sym: SemSym): string =
  if sym.name.kind == seIdent:
    return sym.name.idname
  else:
    return "Unknown"

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
    if scope == scope.top:
      return none(SemExpr)
    else:
      return scope.top.getVar(vi)
  return some scope.varidents[vi]
proc getProc*(scope: SemScope, pi: ProcIdentDecl): Option[SemFunc] =
  if not scope.procidents.hasKey(pi.toProcIdent):
    if scope == scope.top:
      return none(SemFunc)
    else:
      return scope.top.getProc(pi)
  let group = scope.procidents[pi.toProcIdent]
  for decl in group.idents:
    if pi == decl:
      return some(decl.value)
  if scope == scope.top:
    return none(SemFunc)
  else:
    return scope.top.getProc(pi)
proc getType*(scope: SemScope, ti: TypeIdent): Option[SemType] =
  if not scope.typeidents.hasKey(ti):
    if scope == scope.top:
      return none(SemType)
    else:
      return scope.top.getType(ti)
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

proc isType*(semsym: SemSym, name: string): bool =
  semsym.name.kind == seIdent and semsym.name.idname == name
proc isBoolType*(semsym: SemSym): bool =
  semsym.isType("Bool")
proc isVoidType*(semsym: SemSym): bool =
  semsym.isType("Void")