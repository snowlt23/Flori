
import sast

import tables, hashes

type
  SemTypeSymKind* = enum
    stSpec
    stGenerics
    stTypeGenerics
  SemExprKind* = enum
    seSExpr
    seIf
    seWhile
    seSet
  SemDeclKind* = enum
    seRequire
    sdFunc
    sdStruct
    sdCFunc
    sdCStruct
    sdProtocol

type
  SemTypeSym* = ref object
    case kind*: SemTypeSymKind
    of stSpec:
      spec*: SemDecl
    of stGenerics:
      parent*: SemExpr
    of stTypeGenerics:
      origin*: SemTypeSym
      typeargs*: seq[SemTypeSym]
  SemExpr* = ref object
    typ*: SemTypeSym
    case kind*: SemExprKind
    of seSExpr:
      sexpr*: SExpr
    of seIf:
      discard
    of seWhile:
      discard
    of seSet:
      discard
  SemDecl* = ref object
  
type
  SemSymKind* = enum
    symSemTypeSym
    symSemExpr
    symSemDecl
  SemSym* = object
    case kind*: SemSymKind
    of symSemTypeSym:
      st*: SemTypeSym
    of symSemExpr:
      se*: SemExpr
    of symSemDecl:
      sd*: SemDecl

type
  ProcIdentDecl* = ref object
    name*: string
    args*: seq[SemTypeSym]
    value*: SemDecl
  ProcIdentGroup* = ref object
    idents*: seq[ProcIdentDecl]
  ProcIdent* = ref object
    scope*: SemScope
    name*: string
  TypeIdent* = ref object
    scope*: SemScope
    name*: string
  ScopeIdent* = ref object
    scope*: SemScope
    name*: string
  SemScope* = ref object
    top*: SemScope
    procidents*: Table[ProcIdent, ProcIdentGroup]
    typeidents*: Table[TypeIdent, SemDecl]

proc hash*(procid: ProcIdent): Hash = hash(procid.name)
proc hash*(typeid: TypeIdent): Hash = hash(typeid.name)
proc hash*(scopeid: ScopeIdent): Hash = hash(scopeid.name)

proc newSemScope*(): SemScope =
  new result
  result.top = result
  result.procidents = initTable[ProcIdent, ProcIdentGroup]()
  result.typeidents = initTable[TypeIdent, SemDecl]()

proc extendSemScope*(scope: SemScope): SemScope =
  new result
  result.top = scope.top
  result.procidents = scope.procidents
  result.typeidents = scope.typeidents
