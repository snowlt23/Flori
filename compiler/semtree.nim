
import sast

import tables, hashes
import options

type
  SemTypeSymKind* = enum
    stSpec
    stGenerics
    stTypeGenerics
  SemExprKind* = enum
    seSExpr
    seIdent
    seFuncCall
    seIf
    seWhile
    seSet
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
    argtypes*: seq[SemTypeSym]
    returntype*: SemTypeSym
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
    of seIdent:
      nameid*: string
    of seFuncCall:
      fn*: SemDecl
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
  SemDecl* = ref object
    name*: string
    functype*: Option[FuncType]
    case kind*: SemDeclKind
    of sdRequire:
      requiremodule*: SemScope
    of sdVariable:
      discard
    of sdFunc:
      discard
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
      st*: SemTypeSym
    of symSemExpr:
      se*: SemExpr
    of symSemDecl:
      sd*: SemDecl

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
