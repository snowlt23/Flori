
import tables, hashes
import strutils, sequtils
import sparser
import sast
import options
import macros
import os

type
  SemanticError* = object of Exception

  CTRefCounterKind* = enum
    ctrefOwner
    ctrefBorrow
  CTRefCounter* = ref object
    case kind*: CTRefCounterKind
    of ctrefOwner:
      count*: int
    of ctrefBorrow:
      owner*: CTRefCounter

  Symbol* = object
    isImported*: bool
    scope*: Scope
    name*: string
    semexpr*: SemanticExpr

  SemanticTypeArdKind* = enum
    semtypeName
    semtypeGenerics
    semtypeTypeGenerics
    semtypeVarargs
  SemanticTypeArg* = object
    case kind*: SemanticTypeArdKind
    of semtypeName:
      namesym*: Symbol
    of semtypeGenerics:
      genericssym*: Symbol
    of semtypeTypeGenerics:
      typegenericssym*: Symbol
    of semtypeVarargs:
      varargssym*: Symbol
  SemanticIdent* = ref object
    scope*: Scope
    span*: Span
    name*: string
    args*: seq[SemanticTypeArg]
  SemanticIdentGroup* = object
    idsymbols*: seq[tuple[semid: SemanticIdent, value: Symbol]]
    
  Metadata* = object
    metatable*: Table[string, SemanticExpr]
  FuncType* = object
    returntype*: Symbol
    argtypes*: seq[Symbol]
  PrimitiveFuncKind* = enum
    primitiveCall
    primitiveInfix

  SemanticExprKind* = enum
    semanticNotType
    semanticSExpr
    semanticIdent
    semanticSymbol
    semanticCExpr
    semanticVariable
    semanticGenerics
    semanticTypeGenerics
    semanticType
    semanticProtocol
    semanticProtocolFunc
    semanticFunction
    semanticMacro
    semanticStruct
    semanticStructConstructor
    semanticFieldAccess
    semanticVarargsType
    semanticPrimitiveType
    semanticPrimitiveValue
    semanticPrimitiveFunc
    semanticPrimitiveEval
    semanticIfExpr
    semanticWhileSyntax
    semanticSetSyntax
    semanticModule
    semanticRequire
    semanticFuncCall
    semanticInt
    semanticString
  SemanticExpr* = ref object
    span*: Span
    metadata*: Metadata
    typesym*: Symbol
    refcounter*: CTRefCounter
    case kind*: SemanticExprKind
    of semanticNotType:
      discard
    of semanticSExpr:
      sexpr*: SExpr
    of semanticIdent:
      ident*: string
    of semanticSymbol:
      symbol*: Symbol
    of semanticCExpr:
      cexpr*: CExpr
    of semanticVariable:
      variable*: Variable
    of semanticGenerics:
      generics*: Generics
    of semanticTypeGenerics:
      typegenerics*: TypeGenerics
    of semanticType:
      semtype*: SemType
    of semanticProtocol:
      protocol*: Protocol
    of semanticProtocolFunc:
      protocolfntype*: FuncType
    of semanticFunction:
      function*: Function
    of semanticMacro: # TODO:
      discard
    of semanticStruct:
      struct*: Struct
    of semanticStructConstructor:
      structconstructor*: StructConstructor
    of semanticFieldAccess:
      fieldaccess*: FieldAccess
    of semanticVarargsType:
      varargstype*: VarargsType
    of semanticPrimitiveType:
      primtype*: PrimitiveType
    of semanticPrimitiveValue:
      primValue*: string
    of semanticPrimitiveFunc:
      primfunc*: PrimitiveFunc
    of semanticPrimitiveEval:
      evalproc*: proc (scope: var Scope, sexpr: SExpr): SemanticExpr
    of semanticIfExpr:
      ifexpr*: IfExpr
    of semanticWhileSyntax:
      whilesyntax*: WhileSyntax
    of semanticSetSyntax:
      setsyntax*: SetSyntax
    of semanticModule:
      module*: Module
    of semanticRequire:
      requiremodule*: Module
    of semanticFuncCall:
      funccall*: FuncCall
    of semanticInt:
      intval*: int64
    of semanticString:
      strval*: string
  CExpr* = ref object
    format*: string
    args*: seq[SemanticExpr]
  Variable* = ref object
    name*: string
    value*: SemanticExpr
  Generics* = ref object
    attr*: string
    protocol*: Symbol
    spec*: Option[Symbol]
  TypeGenerics* = ref object
    typ*: Symbol
    generics*: seq[Symbol]
  SemType* = ref object
    sym*: Symbol
    specs*: seq[Symbol]
  Protocol* = ref object
    isGenerics*: bool
    funcs*: seq[tuple[name: string, fntype: FuncType]]
  Function* = ref object
    isGenerics*: bool
    name*: string
    argnames*: seq[string]
    fntype*: FuncType
    body*: seq[SemanticExpr]
  Struct* = ref object
    isGenerics*: bool
    generics*: seq[Symbol]
    name*: string
    fields*: seq[tuple[name: string, typesym: Symbol]]
  StructConstructor* = ref object
    structsym*: Symbol
    values*: seq[tuple[name: string, value: SemanticExpr]]
  FieldAccess* = ref object
    valuesym*: Symbol
    fieldname*: string
  Cffi* = ref object
    name*: string
    primname*: string
    argtypes*: seq[Symbol]
    rettype*: Symbol
  CCodegenInfo* = object
    headers*: OrderedTable[string, bool]
    decls*: seq[string]
    cffis*: seq[Cffi]
  VarargsType* = ref object
    generics*: Symbol
  PrimitiveType* = ref object
    isGenerics*: bool
    name*: string
    primname*: string
    argtypes*: seq[Symbol]
  PrimitiveFunc* = ref object
    isGenerics*: bool
    kind*: PrimitiveFuncKind
    pattern*: bool
    name*: string
    fntype*: FuncType
  IfExpr* = ref object
    cond*: SemanticExpr
    tbody*: SemanticExpr
    fbody*: SemanticExpr
  WhileSyntax* = ref object
    cond*: SemanticExpr
    body*: seq[SemanticExpr]
  SetSyntax* = ref object
    variable*: SemanticExpr
    value*: SemanticExpr
  Module* = ref object
    context*: SemanticContext
    name*: string
    semanticidents*: Table[SemanticIdent, SemanticIdentGroup]
    symbols*: seq[Symbol]
    toplevelcalls*: seq[SemanticExpr]
    ccodegenInfo*: CCodegenInfo
  FuncCall* = ref object
    callfunc*: Symbol
    args*: seq[SemanticExpr]
  Scope* = object
    module*: Module
    semanticidents*: Table[SemanticIdent, SemanticIdentGroup]
    scopevalues*: seq[Symbol]
  SemanticContext* = ref object
    includepaths*: seq[string]
    topmodule*: Module
    modules*: OrderedTable[string, Module]
    symcount*: int

proc `$`*(semtypearg: SemanticTypeArg): string
proc getSemanticTypeArg*(sym: Symbol): SemanticTypeArg
proc getSemanticTypeArgs*(syms: seq[Symbol]): seq[SemanticTypeArg]
proc newModule*(context: SemanticContext, modulename: string): Module
proc newScope*(module: Module): Scope
proc newSymbol*(scope: Scope, name: string, semexpr: SemanticExpr, isImported = false): Symbol
proc addSymbol*(scope: var Scope, semid: SemanticIdent, sym: Symbol)
proc getSymbol*(scope: var Scope, semid: SemanticIdent): Symbol
proc evalSExpr*(scope: var Scope, sexpr: SExpr): SemanticExpr
proc evalFile*(context: SemanticContext, modulepath: string): Module

#
# Consts
#

let globalModule* = newModule(nil, "global")
let globalScope* = newScope(globalModule)

template notTypeSym*(): Symbol =
  const internalname = instantiationInfo().filename
  const internalline = instantiationInfo().line
  let internalSpan = Span(line: 0, linepos: 0, internal: (internalname, internalline))
  let notTypeSemExpr = SemanticExpr(span: internalSpan, kind: semanticNotType)
  newSymbol(globalScope, "not_type_symbol", notTypeSemExpr)

#
# Error
#

proc raiseError*(span: Span, s: string) =
  if span.line == 0 and span.linepos == 0:
    let msg = "internal($#:$#) " % [span.internal.filename, $span.internal.line] & s
    when not defined(release):
      raise newException(SemanticError, msg)
    else:
      quit msg
  else:
    let msg = "$#($#:$#) " % [span.filename, $span.line, $span.linepos] & s
    when not defined(release):
      raise newException(SemanticError, msg)
    else:
      quit msg
proc raiseError*(semexpr: SemanticExpr, s: string) =
  semexpr.span.raiseError(s)
proc raiseError*(sym: Symbol, s: string) =
  sym.semexpr.raiseError(s)
proc raiseError*(semid: SemanticIdent, s: string) =
  semid.span.raiseError(s)

#
# CTRefCounter
#

proc refinc*(refcounter: CTRefCounter) =
  case refcounter.kind
  of ctrefOwner:
    refcounter.count.inc
  of ctrefBorrow:
    refcounter.owner.count.inc
proc refdec*(refcounter: CTRefCounter) =
  case refcounter.kind
  of ctrefOwner:
    refcounter.count.dec
  of ctrefBorrow:
    refcounter.owner.count.dec

proc refinc*(semexpr: SemanticExpr) =
  if semexpr.kind == semanticSymbol:
    semexpr.symbol.semexpr.refcounter.refinc
  else:
    semexpr.refcounter.refinc
proc refdec*(semexpr: SemanticExpr) =
  if semexpr.kind == semanticSymbol:
    semexpr.symbol.semexpr.refcounter.refdec
  else:
    semexpr.refcounter.refdec

proc canOwner*(semexpr: SemanticExpr): bool =
  if semexpr.kind == semanticSymbol:
    if semexpr.symbol.semexpr.refcounter.kind == ctrefOwner and semexpr.symbol.semexpr.refcounter.count == 0:
      return true
    else:
      return false
  else:
    if semexpr.refcounter.kind == ctrefOwner and semexpr.refcounter.count == 0:
      return true
    else:
      return false
proc isGarbage*(semexpr: SemanticExpr): bool =
  if semexpr.refcounter.kind == ctrefOwner and semexpr.refcounter.count == 0:
    return true
  else:
    return false

#
# SemanticIdent
#

proc newSemanticIdent*(scope: Scope, span: Span, name: string, args: seq[SemanticTypeArg]): SemanticIdent =
  SemanticIdent(scope: scope, span: span, name: name, args: args)
proc newSemanticIdent*(scope: var Scope, sexpr: SExpr): SemanticIdent =
  if sexpr.kind == sexprList:
    var args = newSeq[Symbol]()
    for e in sexpr.rest:
      args.add(scope.getSymbol(scope.newSemanticIdent(e)))
    return newSemanticIdent(scope, sexpr.span, $sexpr.first, args.getSemanticTypeArgs())
  elif sexpr.kind == sexprIdent:
    return newSemanticIdent(scope, sexpr.span, $sexpr, @[])
  elif sexpr.kind == sexprAttr:
    return newSemanticIdent(scope, sexpr.span, $sexpr, @[])
  else:
    sexpr.span.raiseError("can't use $# instead ident" % $sexpr.kind)
proc newSemanticIdent*(symbol: Symbol): SemanticIdent =
  newSemanticIdent(symbol.scope, symbol.semexpr.span, symbol.name, @[])
proc newSemanticIdentGroup*(): SemanticIdentGroup =
  result.idsymbols = @[]
proc addSymbol*(symgroup: var SemanticIdentGroup, semid: SemanticIdent, symbol: Symbol) =
  symgroup.idsymbols.add((semid, symbol))
proc hash*(semid: SemanticIdent): Hash =
  hash(semid.name)
proc `==`*(a, b: SemanticIdent): bool =
  a.name == b.name
proc `$`*(semid: SemanticIdent): string =
  if semid.args.len == 0:
    semid.name
  else:
    "(" & semid.name & " " & semid.args.mapIt($it).join(" ") & ")"

#
# SemanticTypeArg
#

proc `$`*(semtypearg: SemanticTypeArg): string =
  case semtypearg.kind
  of semtypeName:
    semtypearg.namesym.name
  of semtypeGenerics:
    semtypearg.genericssym.name
  of semtypeTypeGenerics:
    semtypearg.typegenericssym.name
  of semtypeVarargs:
    semtypearg.varargssym.name
proc debug*(semtypearg: SemanticTypeArg): string =
  case semtypearg.kind
  of semtypeName:
    $semtypearg.kind & ":" & semtypearg.namesym.name
  of semtypeGenerics:
    $semtypearg.kind & ":" & semtypearg.genericssym.name
  of semtypeTypeGenerics:
    $semtypearg.kind & ":" & semtypearg.typegenericssym.name
  of semtypeVarargs:
    $semtypearg.kind & ":" & semtypearg.varargssym.name
proc `$`*(symbolargs: seq[SemanticTypeArg]): string =
  if symbolargs.len == 0:
    return ""
  else:
    return "(" & symbolargs.mapIt($it).join(" ") & ")"
proc debug*(symbolargs: seq[SemanticTypeArg]): string =
  if symbolargs.len == 0:
    return ""
  else:
    return "(" & symbolargs.mapIt(it.debug).join(" ") & ")"

#
# Symbol
#

proc newSymbol*(scope: Scope, name: string, semexpr: SemanticExpr, isImported = false): Symbol =
  Symbol(isImported: isImported, scope: scope, name: name, semexpr: semexpr)
proc globalSymbol*(name: string, semexpr: SemanticExpr): Symbol =
  newSymbol(globalScope, name, semexpr)
proc `$`*(symbol: Symbol): string =
  symbol.scope.module.name & "_" & symbol.name
proc `==`*(a: Symbol, b: Symbol): bool =
  a.scope.module.name == b.scope.module.name and a.name == b.name
proc isSpecType*(sym: Symbol): bool =
  if sym.semexpr.kind == semanticPrimitiveType:
    return not sym.semexpr.primtype.isGenerics
  elif sym.semexpr.kind == semanticGenerics and sym.semexpr.generics.spec.isSome:
    return true
  elif sym.semexpr.kind == semanticGenerics:
    return false
  elif sym.semexpr.kind == semanticTypeGenerics:
    return false
  elif sym.semexpr.kind == semanticStruct:
    return not sym.semexpr.struct.isGenerics # FIXME:
  else:
    return false
    # sym.raiseError("$# is not type" % $sym) # FIXME:
proc isSpecTypes*(syms: seq[Symbol]): bool =
  for sym in syms:
    if not sym.isSpecType:
      return false
  return true

#
# Semantic Symbol Table
#

proc match*(arg, garg: SemanticTypeArg): bool =
  if arg.kind == semtypeName and garg.kind == semtypeName:
    return arg.namesym == garg.namesym
  elif garg.kind == semtypeGenerics:
    return true
  elif arg.kind == semtypeName and garg.kind == semtypeTypeGenerics:
    return true
  elif arg.kind == semtypeGenerics and garg.kind == semtypeTypeGenerics:
    return true
  elif arg.kind == semtypeTypeGenerics and garg.kind == semtypeTypeGenerics:
    return match(getSemanticTypeArg(arg.typegenericssym), getSemanticTypeArg(garg.typegenericssym)):
  elif arg.kind == semtypeName and garg.kind == semtypeVarargs:
    return match(getSemanticTypeArg(arg.namesym), getSemanticTypeArg(garg.varargssym.semexpr.varargstype.generics))
  else:
    return false

proc match*(semid, gsemid: SemanticIdent): bool =
  for i in 0..<gsemid.args.len:
    if not match(semid.args[i], gsemid.args[i]):
      return false
  return true
proc equal*(semid, gsemid: SemanticIdent): bool =
  for i in 0..<gsemid.args.len:
    let arg = semid.args[i]
    let garg = gsemid.args[i]
    if arg.kind == semtypeName and garg.kind == semtypeName:
      if arg.namesym != garg.namesym:
        return false
    elif garg.kind == semtypeGenerics:
      return false
    elif garg.kind == semtypeTypeGenerics:
      if arg.typegenericssym != garg.typegenericssym:
        return false
  return true
proc matchVarargs*(semid, gsemid: SemanticIdent): bool =
  if gsemid.args.len == 0:
    return false
  for i in 0..<semid.args.len:
    if i >= gsemid.args.len:
      if not match(semid.args[i], gsemid.args[^1]):
        return false
    else:
      if not match(semid.args[i], gsemid.args[i]):
        return false
  return true

proc hasSemId*(semids: Table[SemanticIdent, SemanticIdentGroup], semid: SemanticIdent): bool =
  if semids.hasKey(semid):
    let symgroup = semids[semid]
    for gsemidpair in symgroup.idsymbols:
      if gsemidpair.semid.args.len != semid.args.len:
        if matchVarargs(semid, gsemidpair.semid):
          return true
      elif match(semid, gsemidpair.semid):
        return true
    return false
  else:
    return false
proc hasSpecSemId*(semids: Table[SemanticIdent, SemanticIdentGroup], semid: SemanticIdent): bool =
  if semids.hasKey(semid):
    let symgroup = semids[semid]
    for gsemidpair in symgroup.idsymbols:
      if gsemidpair.semid.args.len != semid.args.len:
        continue
      elif equal(semid, gsemidpair.semid):
        return true
    return false
  else:
    return false
proc addSymbol*(semids: var Table[SemanticIdent, SemanticIdentGroup], semid: SemanticIdent, symbol: Symbol) =
  if semids.hasSpecSemId(semid):
    symbol.raiseError("can't redefine symbol: $#" % [semid.name])
  if not semids.hasKey(semid):
    semids[semid] = newSemanticIdentGroup()
  semids[semid].addSymbol(semid, symbol)
proc getSymbol*(semids: var Table[SemanticIdent, SemanticIdentGroup], semid: SemanticIdent): Symbol =
  if semids.hasKey(semid):
    let symgroup = semids[semid]
    for gsemidpair in symgroup.idsymbols:
      if gsemidpair.semid.args.len != semid.args.len:
        if matchVarargs(semid, gsemidpair.semid):
          return gsemidpair.value
      elif match(semid, gsemidpair.semid):
        return gsemidpair.value
  semid.raiseError("undeclared ident: $#" % $semid)
proc getSpecSymbol*(semids: var Table[SemanticIdent, SemanticIdentGroup], semid: SemanticIdent): Symbol =
  if semids.hasKey(semid):
    let symgroup = semids[semid]
    for gsemidpair in symgroup.idsymbols:
      if gsemidpair.semid.args.len != semid.args.len:
        continue
      elif equal(semid, gsemidpair.semid):
        return gsemidpair.value
  semid.raiseError("undeclared ident: $#" % $semid)
proc trySymbol*(semids: var Table[SemanticIdent, SemanticIdentGroup], semid: SemanticIdent): Option[Symbol] =
  if semids.hasKey(semid):
    let symgroup = semids[semid]
    for gsemidpair in symgroup.idsymbols:
      if gsemidpair.semid.args.len != semid.args.len:
        continue
      elif match(semid, gsemidpair.semid):
        return some(gsemidpair.value)
  else:
    return none(Symbol)

#
# SemanticTypeArg from Symbol
#

proc getSemanticTypeArg*(sym: Symbol): SemanticTypeArg =
  if sym.semexpr.kind == semanticGenerics:
    return SemanticTypeArg(kind: semtypeGenerics, genericssym: sym)
  elif sym.semexpr.kind == semanticTypeGenerics:
    return SemanticTypeArg(kind: semtypeTypeGenerics, typegenericssym: sym)
  elif sym.semexpr.kind == semanticVarargsType:
    return SemanticTypeArg(kind: semtypeVarargs, varargssym: sym)
  else:
    return SemanticTypeArg(kind: semtypeName, namesym: sym)
proc getSemanticTypeArgs*(syms: seq[Symbol]): seq[SemanticTypeArg] =
  syms.mapIt(getSemanticTypeArg(it))

#
# Metadata
#

proc newMetadata*(): MetaData =
  result.metatable = initTable[string, SemanticExpr]()
proc addMetadata*(semexpr: SemanticExpr, name: string, meta: SemanticExpr) =
  semexpr.metadata.metatable[name] = meta
proc getMetadata*(semexpr: SemanticExpr, name: string): SemanticExpr =
  if not semexpr.metadata.metatable.hasKey(name):
    semexpr.raiseError("expression hasn't `$#` metadata" % name)
  semexpr.metadata.metatable[name]

#
# SemanticExpr
#

macro newSemanticExpr*(span: Span, kind: SemanticExprKind, typesym: Symbol, body: varargs[untyped]): SemanticExpr =
  let tmpid= genSym(nskVar, "tmp")
  result = quote do:
    var `tmpid` = SemanticExpr(kind: `kind`, span: `span`, typesym: `typesym`, metadata: newMetadata(), refcounter: CTRefCounter(kind: ctrefOwner, count: 0))
  for b in body:
    expectKind(b, nnkExprColonExpr)
    let name = b[0]
    name.expectKind(nnkIdent)
    let value = b[1]
    result.add(quote do:
      `tmpid`.`name` = `value`
    )
  result.add(tmpid)
proc expectSemantic*(semexpr: SemanticExpr, kind: SemanticExprKind) =
  if semexpr.kind != kind:
    semexpr.raiseError("expression is not $#, actually: $#" % [$kind, $semexpr.kind])

#
# CCodegenInfo
#

proc newCCodegenInfo*(): CCodegenInfo =
  result.headers = initOrderedTable[string, bool]()
  result.decls = @[]
proc addHeader*(info: var CCodegenInfo, name: string) =
  info.headers[name] = true

#
# Module
#

proc newModule*(context: SemanticContext, modulename: string): Module =
  new result
  result.context = context
  result.name = modulename
  result.semanticidents = initTable[SemanticIdent, SemanticIdentGroup]()
  result.symbols = @[]
  result.toplevelcalls = @[]
  result.ccodegenInfo = newCCodegenInfo()

proc addSymbol*(module: Module, semid: SemanticIdent, sym: Symbol) =
  module.semanticidents.addSymbol(semid, sym)
  module.symbols.add(sym)
proc addToplevelCall*(module: Module, sexpr: SExpr, funccall: FuncCall) =
  module.toplevelcalls.add(newSemanticExpr(sexpr.span, semanticFuncCall, notTypeSym, funccall: funccall))
proc addCffi*(module: Module, cffi: Cffi) =
  module.ccodegeninfo.cffis.add(cffi)
proc addDecl*(module: Module, decl: string) =
  module.ccodegeninfo.decls.add(decl)

iterator semidsymbols*(module: Module): tuple[semid: SemanticIdent, symbol: Symbol] =
  for semidgroup in module.semanticidents.values:
    for semidvalue in semidgroup.idsymbols:
      yield(semidvalue.semid, semidvalue.value)

proc newSemanticContext*(): SemanticContext =
  new result
  result.includepaths = @[]
  result.modules = initOrderedTable[string, Module]()

#
# Scope
#

proc newScope*(module: Module): Scope =
  result.module = module
  result.semanticidents = initTable[SemanticIdent, SemanticIdentGroup]()
  result.scopevalues = @[]
proc hasSemId*(scope: Scope, semid: SemanticIdent): bool =
  scope.semanticidents.hasSemId(semid) or scope.module.semanticidents.hasSemId(semid)
proc hasSpecSemId*(scope: Scope, semid: SemanticIdent): bool =
  scope.semanticidents.hasSpecSemId(semid) or scope.module.semanticidents.hasSpecSemId(semid)
proc addSymbol*(scope: var Scope, semid: SemanticIdent, sym: Symbol) =
  scope.semanticidents.addSymbol(semid, sym)
proc getSymbol*(scope: var Scope, semid: SemanticIdent): Symbol =
  if scope.semanticidents.hasSemId(semid):
    return scope.semanticidents.getSymbol(semid)
  elif scope.module.semanticidents.hasSemId(semid):
    return scope.module.semanticidents.getSymbol(semid)
  else:
    semid.raiseError("couldn't find symbol: $#" % $semid)
proc getSpecSymbol*(scope: var Scope, semid: SemanticIdent): Symbol =
  if scope.semanticidents.hasSpecSemId(semid):
    return scope.semanticidents.getSpecSymbol(semid)
  elif scope.module.semanticidents.hasSpecSemId(semid):
    return scope.module.semanticidents.getSpecSymbol(semid)
  else:
    semid.raiseError("couldn't find symbol: $#" % $semid)
proc trySymbol*(scope: var Scope, semid: SemanticIdent): Option[Symbol] =
  let opt = scope.semanticidents.trySymbol(semid)
  if opt.isSome:
    return opt
  else:
    return scope.module.semanticidents.trySymbol(semid)

proc addScopeValue*(scope: var Scope, sym: Symbol) =
  scope.scopevalues.add(sym)

proc getScopeValues*(scope: Scope): tuple[survived: seq[Symbol], garbages: seq[Symbol]] =
  result = (@[], @[])
  for scopevalue in scope.scopevalues:
    scopevalue.semexpr.refdec
    if scopevalue.semexpr.isGarbage:
      result.garbages.add(scopevalue)
    elif scopevalue.semexpr.refcounter.kind == ctrefBorrow:
      discard
    else:
      result.survived.add(scopevalue)

#
# define primitives
#

# FIXME: defPrimitiveType
proc defPrimitiveType*(scope: var Scope, span: Span, generics: seq[string], typename: string, primname: string): Symbol =
  let genericssyms = generics.mapIt(scope.getSymbol(scope.newSemanticIdent(span, it, @[])))
  let semexpr = newSemanticExpr(
    span,
    semanticPrimitiveType,
    notTypeSym,
    primtype: PrimitiveType(
      isGenerics: false,
      name: typename,
      primname: primname,
      argtypes: genericssyms
    )
  )
  let sym = newSymbol(scope, typename, semexpr)
  semexpr.typesym = sym
  let semid = scope.newSemanticIdent(span, typename, genericssyms.getSemanticTypeArgs)
  scope.module.addSymbol(semid, sym)
  return sym
proc defPrimitiveValue*(scope: var Scope, span: Span, typename: string, valuename: string, value: string) =
  var semexpr = newSemanticExpr(
    span,
    semanticPrimitiveValue,
    scope.getSymbol(scope.newSemanticIdent(span, typename, @[])),
    primValue: value
  )
  let sym = newSymbol(scope, valuename, semexpr)
  scope.module.addSymbol(newSemanticIdent(sym), sym)
proc defPrimitiveFunc*(scope: var Scope, span: Span, isPattern: bool, funcname: string, argtypes: seq[Symbol], rettype: Symbol, kind: PrimitiveFuncKind, primname: string): Symbol =
  let primfunc = PrimitiveFunc(
    isGenerics: false,
    kind: kind,
    pattern: isPattern,
    name: primname,
    fntype: FuncType(
      returntype: rettype,
      argtypes: argtypes
    )
  )
  let semexpr = newSemanticExpr(span, semanticPrimitiveFunc, rettype, primfunc: primfunc)
  let sym = newSymbol(scope, funcname, semexpr)
  let semid = scope.newSemanticIdent(span, funcname, argtypes.getSemanticTypeArgs)
  scope.module.addSymbol(semid, sym)
  return sym
proc defPrimitiveEval*(scope: var Scope, span: Span, macroname: string, evalproc: proc (scope: var Scope, sexpr: SExpr): SemanticExpr) =
  let semexpr = newSemanticExpr(span, semanticPrimitiveEval, notTypeSym, evalproc: evalproc)
  let sym = newSymbol(scope, macroname, semexpr, isImported = true)
  scope.module.addSymbol(newSemanticIdent(sym), sym)

#
# Type Specialize
#

proc parseTypeAnnotation*(sexpr: SExpr): tuple[argtypes: seq[SExpr], rettype: SExpr, body: SExpr] =
  var argtypes = newSeq[SExpr]()
  var rettype: SExpr
  let body = sexpr.last
  sexpr.rest.each(arg):
    if $arg.first == "->":
      rettype = arg.rest.first
      break
    elif arg.rest.kind == sexprNil:
      break
    else:
      argtypes.add(arg.first)
  if rettype == nil:
    rettype = newSIdent(sexpr.span, "Void")
  result = (argtypes, rettype, body)
proc getTypeGenerics*(scope: var Scope, sexpr: SExpr): Symbol = # FIXME: infinite loop?
  if $sexpr.first == "Varargs":
    let semexpr = newSemanticExpr(
      sexpr.span,
      semanticVarargsType,
      notTypeSym,
      varargstype: VarargsType(
        generics: scope.getSymbol(newSemanticIdent(scope, sexpr.rest.first))
      )
    )
    let sym = newSymbol(scope, $sexpr.first, semexpr)
    semexpr.typesym = sym
    return sym
  else:
    var generics = newSeq[Symbol]()
    for e in sexpr.rest:
      generics.add(scope.getSymbol(newSemanticIdent(scope, e)))
    let typegenerics = TypeGenerics(
      typ: scope.getSymbol(newSemanticIdent(scope, sexpr)),
      generics: generics
    )  
    let semexpr = newSemanticExpr(sexpr.span, semanticTypeGenerics, notTypeSym, typegenerics: typegenerics)
    let sym = newSymbol(scope, $sexpr.first, semexpr)
    semexpr.typesym = sym
    return sym

proc getTypeAnnotation*(scope: var Scope, sexpr: SExpr): tuple[argtypes: seq[Symbol], rettype: Symbol, body: SExpr] =
  let (argtypes, rettype, body) = parseTypeAnnotation(sexpr)
  var argtypesyms = newSeq[Symbol]()
  for argtype in argtypes:
    if argtype.kind == sexprList:
      argtypesyms.add(scope.getTypeGenerics(argtype))
    else:
      argtypesyms.add(scope.getSymbol(scope.newSemanticIdent(argtype)))
  let rettypesym = if rettype.kind == sexprList:
                     scope.getTypeGenerics(rettype)
                   else:
                     scope.getSymbol(scope.newSemanticIdent(rettype))
  result = (argtypesyms, rettypesym, body)

proc addArgSymbols*(scope: var Scope, argtypesyms: seq[Symbol], funcdef: SExpr) =
  if funcdef.rest.rest.first.kind == sexprNil:
    return
  if funcdef.rest.rest.first.len != argtypesyms.len:
    raise newException(SemanticError, "($#:$#) argument length is not equals type length" % [$funcdef.span.line, $funcdef.span.linepos])
  for i, arg in funcdef.rest.rest.first:
    var semexpr = newSemanticExpr(funcdef.span, semanticIdent, argtypesyms[i], ident: $arg)
    let sym = newSymbol(scope, $arg, semexpr)
    scope.addSymbol(newSemanticIdent(sym), sym)

proc genSpecGenericsPrimitiveType*(scope: var Scope, typesemexpr: SemanticExpr, typesyms: seq[Symbol]): Symbol
proc genSpecGenericsStruct*(scope: var Scope, typesemexpr: SemanticExpr, typesyms: seq[Symbol]): Symbol

proc getSpecType*(scope: var Scope, sym: Symbol): Symbol =
  if sym.semexpr.kind == semanticGenerics:
    if sym.semexpr.generics.spec.isNone:
      sym.raiseError("couldn't specialize generics param: $#" % sym.name)
    return sym.semexpr.generics.spec.get
  elif sym.semexpr.kind == semanticTypeGenerics:
    let typesym = sym.semexpr.typegenerics.typ
    if typesym.semexpr.kind == semanticPrimitiveType:
      return genSpecGenericsPrimitiveType(scope, typesym.semexpr, sym.semexpr.typegenerics.generics)
    elif typesym.semexpr.kind == semanticStruct:
      return genSpecGenericsStruct(scope, typesym.semexpr, sym.semexpr.typegenerics.generics)
    else:
      sym.raiseError("$# is unsuppoted in getSpecType" % $typesym.semexpr.kind)
  else:
    return sym

#
# Generics
#

proc specializeGenerics*(genericssym: Symbol, typesym: Symbol) =
  if genericssym.semexpr.kind == semanticGenerics:
    genericssym.semexpr.generics.spec = some(typesym)
  elif genericssym.semexpr.kind == semanticTypeGenerics:
    for i in 0..<genericssym.semexpr.typegenerics.generics.len:
      if typesym.semexpr.kind == semanticPrimitiveType:
        specializeGenerics(genericssym.semexpr.typegenerics.generics[i], typesym.semexpr.primtype.argtypes[i]) # FIXME:
      else:
        genericssym.raiseError("specializeGenerics: $# is not supported in currently" % $typesym.semexpr.kind)
proc unspecializeGenerics*(genericssym: Symbol) =
  if genericssym.semexpr.kind == semanticGenerics:
    genericssym.semexpr.generics.spec = none(Symbol)
  elif genericssym.semexpr.kind == semanticTypeGenerics:
    for gene in genericssym.semexpr.typegenerics.generics:
      unspecializeGenerics(gene)

proc specializeGenericsPrimitiveType*(primtypesemexpr: SemanticExpr, typesyms: seq[Symbol]) =
  primtypesemexpr.expectSemantic(semanticPrimitiveType)
  for i, argtype in primtypesemexpr.primtype.argtypes:
    if argtype.semexpr.kind == semanticGenerics:
      specializeGenerics(argtype, typesyms[i])
    elif argtype.semexpr.kind == semanticTypeGenerics:
      specializeGenerics(argtype, typesyms[i])

proc specializeGenericsStruct*(semexpr: SemanticExpr, typesyms: seq[Symbol]) =
  semexpr.expectSemantic(semanticStruct)
  for i, argtype in semexpr.struct.generics:
    if argtype.semexpr.kind == semanticGenerics:
      specializeGenerics(argtype, typesyms[i])
    elif argtype.semexpr.kind == semanticTypeGenerics:
      specializeGenerics(argtype, typesyms[i])

proc specializeGenericsPrimitiveFunc*(primfuncsemexpr: SemanticExpr, typesyms: seq[Symbol]) =
  primfuncsemexpr.expectSemantic(semanticPrimitiveFunc)
  for i, argtype in primfuncsemexpr.primfunc.fntype.argtypes:
    if argtype.semexpr.kind == semanticGenerics:
      specializeGenerics(argtype, typesyms[i])
    elif argtype.semexpr.kind == semanticTypeGenerics:
      specializeGenerics(argtype, typesyms[i])
proc unspecializeGenericsPrimitiveFunc*(primfuncsemexpr: SemanticExpr) =
  primfuncsemexpr.expectSemantic(semanticPrimitiveFunc)
  for argtype in primfuncsemexpr.primfunc.fntype.argtypes:
    if argtype.semexpr.kind == semanticGenerics:
      unspecializeGenerics(argtype)
    elif argtype.semexpr.kind == semanticTypeGenerics:
      unspecializeGenerics(argtype)

proc specializeGenericsFunc*(funcsemexpr: SemanticExpr, typesyms: seq[Symbol]) =
  funcsemexpr.expectSemantic(semanticFunction)
  for i, argtype in funcsemexpr.function.fntype.argtypes:
    if argtype.semexpr.kind == semanticGenerics:
      specializeGenerics(argtype, typesyms[i])
    elif argtype.semexpr.kind == semanticTypeGenerics:
      specializeGenerics(argtype, typesyms[i])

proc genSpecGenericsFunc*(scope: var Scope, funcsemexpr: SemanticExpr, typesyms: seq[Symbol]): Symbol =
  funcsemexpr.expectSemantic(semanticFunction)

  specializeGenericsFunc(funcsemexpr, typesyms)

  let funcname = funcsemexpr.function.name
  let argtypes = funcsemexpr.function.fntype.argtypes.mapIt(scope.getSpecType(it))
  let rettype = scope.getSpecType(funcsemexpr.function.fntype.returntype)
  let f = Function(
    isGenerics: false,
    name: funcname,
    argnames: funcsemexpr.function.argnames,
    fntype: FuncType(
      returntype: rettype,
      argtypes: argtypes
    ),
    body: funcsemexpr.function.body
  )
  let semexpr = newSemanticExpr(funcsemexpr.span, semanticFunction, rettype, function: f)
  let sym = newSymbol(scope, funcname, semexpr)
  let semid = newSemanticIdent(scope, typesyms[0].semexpr.span, funcname, argtypes.getSemanticTypeArgs)
  if scope.hasSpecSemId(semid):
    return sym
  scope.module.addSymbol(semid, sym)
  return sym

proc genSpecGenericsPrimitiveType*(scope: var Scope, typesemexpr: SemanticExpr, typesyms: seq[Symbol]): Symbol =
  typesemexpr.expectSemantic(semanticPrimitiveType)

  specializeGenericsPrimitiveType(typesemexpr, typesyms)
  let argtypes = typesemexpr.primtype.argtypes.mapIt(scope.getSpecType(it))
  let semexpr = newSemanticExpr(
    typesemexpr.span,
    semanticPrimitiveType,
    notTypeSym,
    primtype: PrimitiveType(
      isGenerics: false,
      name: typesemexpr.primtype.name,
      primname: typesemexpr.primtype.primname,
      argtypes: argtypes
    )
  )
  let sym = newSymbol(scope, typesemexpr.primtype.name, semexpr)
  semexpr.typesym = sym
  let semid = scope.newSemanticIdent(typesemexpr.span, typesemexpr.primtype.name, argtypes.getSemanticTypeArgs)
  if scope.hasSpecSemId(semid):
    return sym
  scope.module.addSymbol(semid, sym)
  return sym

proc genSpecGenericsStruct*(scope: var Scope, typesemexpr: SemanticExpr, typesyms: seq[Symbol]): Symbol =
  typesemexpr.expectSemantic(semanticStruct)

  specializeGenericsStruct(typesemexpr, typesyms)
  let argtypes = typesemexpr.struct.generics.mapIt(scope.getSpecType(it))
  var fields = newSeq[tuple[name: string, typesym: Symbol]]()
  for field in typesemexpr.struct.fields:
    echo field.typesym
    fields.add((field.name, scope.getSpecType(field.typesym)))
  let semexpr = newSemanticExpr(
    typesemexpr.span,
    semanticStruct,
    notTypeSym,
    struct: Struct(
      isGenerics: false,
      generics: @[],
      name: typesemexpr.struct.name,
      fields: fields,
    )
  )
  let sym = newSymbol(scope, typesemexpr.struct.name, semexpr)
  semexpr.typesym = sym
  let semid = scope.newSemanticIdent(typesemexpr.span, typesemexpr.struct.name, argtypes.getSemanticTypeArgs)
  if scope.hasSpecSemId(semid):
    return sym
  scope.module.addSymbol(semid, sym)
  return sym

proc genStructConstructor*(scope: var Scope, sexpr: SExpr, sym: Symbol): SemanticExpr =
  let semexpr = newSemanticExpr(
    sexpr.span,
    semanticStructConstructor,
    sym,
    structconstructor: StructConstructor(structsym: sym, values: @[])
  )
  for field in sym.semexpr.struct.fields:
    let attr = sexpr.getAttr(field.name)
    if attr.isNone:
      sexpr.span.raiseError("needs initialzie $# field." % field.name)
    semexpr.structconstructor.values.add((field.name, scope.evalSExpr(attr.get)))
  return semexpr

proc genCallableStruct*(scope: var Scope, sexpr: SExpr, sym: Symbol, typesemexpr: SemanticExpr): SemanticExpr =
  let fieldname = $sexpr.rest.first
  var callsemexpr = newSemanticExpr(
    sexpr.span,
    semanticFieldAccess,
    notTypeSym,
    fieldaccess: FieldAccess(
      valuesym: sym,
      fieldname: fieldname,
    ),
  )
  var rettype = notTypeSym
  for field in typesemexpr.struct.fields:
    if field.name == fieldname:
      rettype = field.typesym
      break
  if rettype == notTypeSym:
    sexpr.rest.first.span.raiseError("undeclared field: $#" % fieldname)
  callsemexpr.typesym = rettype
  return callsemexpr

proc evalType*(scope: var Scope, sexpr: SExpr): Symbol =
  if sexpr.kind == sexprList:
    let typesym = scope.getSymbol(scope.newSemanticIdent(sexpr))
    var argtypes = newSeq[Symbol]()
    for e in sexpr.rest:
      let sym = scope.getSymbol(scope.newSemanticIdent(e))
      argtypes.add(sym)
    if argtypes.isSpecTypes and typesym.semexpr.kind == semanticPrimitiveType and typesym.semexpr.primtype.isGenerics:
      return genSpecGenericsPrimitiveType(scope, typesym.semexpr, argtypes)
    elif argtypes.isSpecTypes and typesym.semexpr.kind == semanticStruct and typesym.semexpr.struct.isGenerics:
      return genSpecGenericsStruct(scope, typesym.semexpr, argtypes)
    elif argtypes.isSpecTypes:
      sexpr.span.raiseError("$# is unsupported in currently" % $typesym.semexpr.kind)
    else:
      return typesym
      # if sexpr.kind == sexprList:
      #   return scope.getTypeGenerics(sexpr)
      # else:
      #   return typesym
  else:
    return scope.getSymbol(scope.newSemanticIdent(sexpr))

proc evalFuncCall*(scope: var Scope, span: Span, callfuncsym: Symbol, args: seq[SemanticExpr], argtypes: seq[Symbol]): SemanticExpr =
  if callfuncsym.semexpr.kind == semanticPrimitiveFunc:
    specializeGenericsPrimitiveFunc(callfuncsym.semexpr, argtypes)
  let speccallfuncsym = if argtypes.isSpecTypes() and callfuncsym.semexpr.kind == semanticFunction and callfuncsym.semexpr.function.isGenerics:
                           genSpecGenericsFunc(scope, callfuncsym.semexpr, argtypes)
                         elif argtypes.isSpecTypes() and callfuncsym.semexpr.kind == semanticPrimitiveType and callfuncsym.semexpr.primtype.isGenerics:
                           genSpecGenericsPrimitiveType(scope, callfuncsym.semexpr, argtypes)
                         else:
                           callfuncsym
  let typesym = if argtypes.isSpecTypes():
                  scope.getSpecType(speccallfuncsym.semexpr.typesym)
                else:
                  if speccallfuncsym.semexpr.typesym.isSpecType:
                    scope.getSpecType(speccallfuncsym.semexpr.typesym)
                  else:
                    speccallfuncsym.semexpr.typesym
  let semexpr = newSemanticExpr(
    span,
    semanticFuncCall,
    typesym,
    funccall: FuncCall(
      callfunc: speccallfuncsym,
      args: args,
    ),
  )
  if callfuncsym.semexpr.kind == semanticPrimitiveFunc:
    unspecializeGenericsPrimitiveFunc(callfuncsym.semexpr)
  return semexpr

proc evalFuncCall*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  var args = newSeq[SemanticExpr]()
  for arg in sexpr.rest:
    args.add(scope.evalSExpr(arg))
  let argtypes = args.mapIt(it.typesym)
  let callfuncsemid = scope.newSemanticIdent(sexpr.first.span, $sexpr.first, argtypes.getSemanticTypeArgs)
  if not scope.hasSemId(callfuncsemid):
    sexpr.first.span.raiseError("undeclared function $#" % $callfuncsemid)
  let callfuncsym = scope.getSymbol(callfuncsemid)
  return scope.evalFuncCall(sexpr.span, callfuncsym, args, argtypes)

proc evalCall*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let trysym = scope.trySymbol(scope.newSemanticIdent(sexpr.first))
  if trysym.isSome:
    if trysym.get.semexpr.kind == semanticPrimitiveEval: # primitive eval
      let retsemexpr = trysym.get.semexpr.evalproc(scope, sexpr)
      if retsemexpr.kind == semanticSExpr:
        return scope.evalSExpr(retsemexpr.sexpr)
      else:
        return retsemexpr
    elif trysym.get.semexpr.kind == semanticStruct: # struct constructor
      return scope.genStructConstructor(sexpr, trysym.get)
    else:
      let typesemexpr = trysym.get.semexpr.typesym.semexpr
      if typesemexpr.kind == semanticStruct: # callable struct
        return scope.genCallableStruct(sexpr, trysym.get, typesemexpr)
      elif typesemexpr.kind == semanticTypeGenerics and typesemexpr.typegenerics.typ.semexpr.kind == semanticStruct:
        return scope.genCallableStruct(sexpr, trysym.get, typesemexpr.typegenerics.typ.semexpr)
      else: # normal call
        return scope.evalFuncCall(sexpr)
  else:
    return scope.evalFuncCall(sexpr)

#
# Eval
#

proc evalIdent*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let sym = scope.getSymbol(scope.newSemanticIdent(sexpr))
  var semexpr = newSemanticExpr(sexpr.span, semanticSymbol, sym.semexpr.typesym, symbol: sym)
  return semexpr

proc evalAttr*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let semid = scope.newSemanticIdent(sexpr)
  if scope.hasSemId(semid):
    return scope.getSymbol(semid).semexpr
  else:
    sexpr.span.raiseError("couldn't find $# attr" % $sexpr)

proc evalInt*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  result = newSemanticExpr(
    sexpr.span, 
    semanticInt, 
    scope.getSymbol(scope.newSemanticIdent(sexpr.span, "Int32", @[])), 
    intval: sexpr.intval
  )
proc evalString*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  result = newSemanticExpr(
    sexpr.span, 
    semanticString, 
    scope.getSymbol(scope.newSemanticIdent(sexpr.span, "CString", @[])),
    strval: sexpr.strval
  )

proc evalSExpr*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  case sexpr.kind
  of sexprNil:
    sexpr.span.raiseError("can't eval nil")
  of sexprList:
    return scope.evalCall(sexpr)
  of sexprIdent:
    return scope.evalIdent(sexpr)
  of sexprAttr:
    return scope.evalAttr(sexpr)
  of sexprInt:
    return scope.evalInt(sexpr)
  of sexprString:
    return scope.evalString(sexpr)
  else:
    sexpr.span.raiseError("couldn't eval: $#" % $sexpr.kind)

include semantic_predefines

proc evalModule*(context: SemanticContext, modulename: string, sexpr: seq[SExpr]): Module =
  let modulename = modulename.replace("/", "_").replace("\\", "_").replace("-", "_")
  var module = newModule(context, modulename)
  var scope = newScope(module)
  scope.predefine()
  context.modules[modulename] = module
  for e in sexpr:
    let semexpr = scope.evalSExpr(e)
    if semexpr.kind == semanticSymbol:
      discard
    else:
      module.toplevelcalls.add(semexpr)
  return module

proc evalFile*(context: SemanticContext, modulepath: string): Module =
  let modulename = modulepath.replace("/", ".").replace(".flori")
  var specfilepath = ""
  for includepath in context.includepaths:
    let filepath = includepath / modulename.replace(".", "/") & ".flori"
    if existsFile(filepath):
      specfilepath = filepath
      break
  if not existsFile(specfilepath):
    raise newException(SemanticError, "couldn't find file: $#" % modulename)
  let sexpr = parseToplevel(specfilepath, readFile(specfilepath))
  return context.evalModule(modulename, sexpr)

proc evalTopfile*(context: SemanticContext, modulepath: string) =
  context.topmodule = context.evalFile(modulepath)
