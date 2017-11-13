
import ast

import tables, hashes
import options
import strutils

type
  SemExprKind* = enum
    semIdent
    semSym
    semGenericsParent
    semGenericsChild
    semCall
    semFunc
    semMacro
    semInternal
    semStruct
    semProtocol
    semArray
    semList
    semBlock
    semVar
    semIf
    semWhile
    semSet
    semIntLit
    semStrLit
  SemFuncKind* = enum
    sfFunc
    sfMacro
    sfInternal
  SemTypeKind* = enum
    stStruct
    stProtocol
  SemSymKind* = enum
    symUnresolve
    symSemFunc
    symSemType
  ProcDeclKind* = enum
    pdFunc
    pdMacro
    pdInternal

type
  FuncType* = object
    argtypes*: seq[SemSym]
    returntype*: SemSym
  FuncPragma* = object
    importc*: Option[string]
    header*: Option[string]
    nodecl*: bool
  StructPragma* = object
    importc*: Option[string]
    header*: Option[string]
    nodecl*: bool

  SemExpr* = ref object
    fexpr*: FExpr
    scope*: SemScope
    typ*: Option[SemSym]
    case kind*: SemExprKind
    of semIdent:
      idname*: string
    of semSym:
      sym*: SemSym
    of semGenericsParent:
      genespec: Option[SemSym]
    of semGenericsChild:
      geneparent*: SemExpr
    of semCall:
      callfn*: SemSym
      callargs*: seq[SemExpr]

    of semFunc:
      funcname*: string
      functype*: FuncType
      funcargs*: seq[SemExpr]
      funcbody*: seq[SemExpr]
      funcpragma*: FuncPragma
    of semMacro: # TODO:
      discard
    of semInternal:
      internalproc*: proc (ctx: SemContext, scope: SemScope, semexpr: SemExpr): SemExpr

    of semStruct: # TODO:
      structpragma*: StructPragma
    of semProtocol: # TODO:
      discard

    of semArray, semList, semBlock:
      body*: seq[SemExpr]

    of semVar:
      varname*: string
      varvalue*: SemExpr
      vartoplevel*: bool
    of semIf:
      ifcond*: SemExpr
      iftrue*: SemExpr
      iffalse*: Option[SemExpr]
    of semWhile:
      whilecond*: SemExpr
      whilebody*: SemExpr
    of semSet:
      setplace*: SemExpr
      setvalue*: SemExpr
    of semIntLit:
      intval*: int64
    of semStrLit:
      strval*: string

  SemSym* = ref object
    isimported*: bool
    scope*: SemScope
    name*: SemExpr
    semexpr*: SemExpr

  SemName* = object
    names*: seq[string]
  ProcName* = object
    name*: SemName
    args*: seq[SemSym]
  ProcDeclGroup* = object
    decls*: seq[SemSym]
  SemScope* = ref object
    name*: SemName
    top*: SemScope
    level*: int
    decls*: Table[SemName, SemSym]
    procdecls*: Table[SemName, ProcDeclGroup]
    toplevels*: seq[SemExpr]
  SemContext* = ref object
    modules*: Table[SemName, SemScope]

proc newSemScope*(name: SemName): SemScope =
  new result
  result.name = name
  result.top = result
  result.level = 0
  result.decls = initTable[SemName, SemSym]()
  result.procdecls = initTable[SemName, ProcDeclGroup]()
  result.toplevels = @[]

proc extendSemScope*(scope: SemScope): SemScope =
  new result
  result.name = scope.name
  result.top = scope.top
  result.level = scope.level + 1
  result.decls = scope.decls
  result.procdecls = scope.procdecls
  result.toplevels = @[]

proc `==`*(a, b: SemScope): bool =
  a.name == b.name and a.level == b.level

# proc semident*(scope: SemScope, name: string): SemExpr =
#   SemExpr(fexpr: fident(internalSpan, name), typ: scope.unresolvesym(), kind: semIdent, scope: scope, idname: name)

# proc semsym*(scope: SemScope, name: SemExpr): SemSym =
#   SemSym(scope: scope, name: name, kind: symUnresolve)
proc `$`*(sym: SemSym): string =
  if sym.name.kind == semIdent:
    return sym.name.idname
  else:
    return "Unknown"

proc hash*(name: SemName): Hash = hash(name.names.join("_"))
proc semname*(names: seq[string]): SemName = SemName(names: names)
proc semname*(name: string): SemName = SemName(names: @[name])
proc `$`*(name: SemName): string =
  name.names.join(".")
proc toSemName*(semexpr: SemExpr): SemName =
  if semexpr.kind == semIdent:
    return semname(semexpr.idname)
  else:
    semexpr.fexpr.error("$# is not name." % $semexpr.kind)

proc isFunc*(sym: SemSym): bool =
  return sym.semexpr.kind == semFunc
proc isInternal*(sym: SemSym): bool =
  return sym.semexpr.kind == semInternal

proc newSymbol*(scope: SemScope, name: SemExpr, semexpr: SemExpr): SemSym =
  SemSym(isimported: false, scope: scope, name: name, semexpr: semexpr)
proc createImportSymbol*(sym: SemSym): SemSym =
  SemSym(isimported: true, scope: sym.scope, name: sym.name, semexpr: sym.semexpr)

proc `==`*(a, b: SemExpr): bool =
  if a.kind != b.kind: return false
  if a.kind == semIdent:
    return a.idname == b.idname
  else:
    return false
proc `==`*(a, b: SemSym): bool =
  a.scope == b.scope and a.name == b.name
proc `==`*(a: ProcName, b: SemSym): bool =
  if a.name != b.name.toSemName(): return false
  if b.isInternal: return true
  if a.args.len != b.semexpr.functype.argtypes.len: return false
  for i in 0..<a.args.len:
    if a.args[i] != b.semexpr.functype.argtypes[i]: return false
  return true

proc initProcIdentGroup*(): ProcDeclGroup =
  result.decls = @[]

proc getDecl*(scope: SemScope, n: SemName): Option[SemSym] =
  if not scope.decls.hasKey(n):
    if scope == scope.top:
      return none(SemSym)
    else:
      return scope.top.getDecl(n)
  return some scope.decls[n]
proc getProc*(scope: SemScope, pd: ProcName): Option[SemSym] =
  if not scope.procdecls.hasKey(pd.name):
    if scope == scope.top:
      return none(SemSym)
    else:
      return scope.top.getProc(pd)
  let group = scope.procdecls[pd.name]
  for sym in group.decls:
    if pd == sym:
      return some(sym)
  if scope == scope.top:
    return none(SemSym)
  else:
    return scope.top.getProc(pd)

proc addTopExpr*(scope: SemScope, e: SemExpr) =
  scope.toplevels.add(e)

proc getArgtypes*(sym: SemSym): seq[SemSym] =
  case sym.semexpr.kind
  of semFunc:
    return sym.semexpr.functype.argtypes
  of semMacro:
    return @[]
  of semInternal:
    return @[]
  else:
    sym.name.fexpr.error("$# hasn't argtypes." % $sym.semexpr.kind)

proc addDecl*(scope: SemScope, n: SemName, v: SemSym): bool =
  if scope.getDecl(n).isSome: return false
  scope.decls[n] = v
  return true
proc addFunc*(scope: SemScope, n: SemName, sym: SemSym): bool =
  let pn = ProcName(name: n, args: sym.getArgtypes())
  if scope.getProc(pn).isSome: return false
  if not scope.procdecls.hasKey(n):
    scope.procdecls[n] = initProcIdentGroup()
  scope.procdecls[n].decls.add(sym)
  return true

proc getReturnType*(semexpr: SemExpr): SemSym =
  if semexpr.kind == semFunc:
    return semexpr.functype.returntype
  else:
    semexpr.fexpr.error("$# hasn't return type." % $semexpr.kind)

proc isType*(semsym: SemSym, name: string): bool =
  $semsym.name.toSemName == name
proc isBoolType*(semsym: SemSym): bool =
  semsym.isType("Bool")
proc isVoidType*(semsym: SemSym): bool =
  semsym.isType("Void")
