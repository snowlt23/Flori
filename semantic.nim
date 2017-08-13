
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

  TypeSymbolKind* = enum
    typesymSpec
    typesymGenerics
    typesymTypeGenerics
    typesymVarargs
    typesymTypedesc
    typesymReftype
    typesymVoid
  TypeSymbol* = ref object
    case kind*: TypeSymbolKind
    of typesymSpec:
      specoriginsym*: Option[Symbol]
      specsym*: Symbol
    of typesymGenerics:
      genericssym*: Symbol
    of typesymTypeGenerics:
      genericsoriginsym*: Symbol
      genericstypes*: seq[TypeSymbol]
    of typesymVarargs:
      varargssym*: TypeSymbol
    of typesymTypedesc:
      typedescsym*: TypeSymbol
    of typesymReftype:
      reftypesym*: TypeSymbol
    of typesymVoid:
      internalspan*: Span

  SemanticIdent* = ref object
    scope*: Scope
    span*: Span
    name*: string
    args*: seq[TypeSymbol]
  SemanticIdentGroup* = object
    idsymbols*: seq[tuple[semid: SemanticIdent, value: Symbol]]
    
  Metadata* = object
    metatable*: Table[string, SemanticExpr]
  FuncType* = object
    returntype*: TypeSymbol
    argtypes*: seq[TypeSymbol]
  PrimitiveFuncKind* = enum
    primitiveCall
    primitiveInfix

  SemanticExprKind* = enum
    semanticNotType
    semanticSExpr
    semanticArgType
    semanticSymbol
    semanticCExpr
    semanticVariable
    semanticGenerics
    semanticGenericsChild
    semanticTypeGenerics
    semanticVarargsType
    semanticTypedesc
    semanticReftype
    # semanticType
    semanticProtocol
    semanticProtocolFunc
    semanticFunction
    semanticMacro
    semanticStruct
    semanticStructConstructor
    semanticFieldAccess
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
    typesym*: TypeSymbol
    refcounter*: CTRefCounter
    case kind*: SemanticExprKind
    of semanticNotType:
      discard
    of semanticSExpr:
      sexpr*: SExpr
    of semanticArgType:
      argtypeident*: string
    of semanticSymbol:
      symbol*: Symbol
    of semanticCExpr:
      cexpr*: CExpr
    of semanticVariable:
      variable*: Variable
    of semanticGenerics:
      generics*: Generics
    of semanticGenericsChild:
      parent*: Symbol
    of semanticTypeGenerics:
      typegenerics*: TypeGenerics
    # of semanticType:
    #   semtype*: SemType
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
    of semanticTypedesc:
      typedesctype*: TypedescType
    of semanticReftype:
      reftype*: Reftype
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
    scope*: Scope
    name*: string
    value*: SemanticExpr
  Generics* = ref object
    scopeindex*: int
    attr*: string
    protocol*: Symbol
    children*: seq[SemanticExpr]
    # spec*: Option[Symbol]
  TypeGenerics* = ref object
    typ*: TypeSymbol
  VarargsType* = ref object
    typ*: TypeSymbol
  TypedescType* = ref object
    typ*: TypeSymbol
  Reftype* = ref object
    typ*: TypeSymbol
  # SemType* = ref object
  #   sym*: Symbol
  #   specs*: seq[Symbol]
  Protocol* = ref object
    isGenerics*: bool
    funcs*: seq[tuple[name: string, fntype: FuncType]]
  Function* = ref object
    isGenerics*: bool
    sym*: Symbol
    argnames*: seq[string]
    fntype*: FuncType
    body*: seq[SemanticExpr]
  Struct* = ref object
    isGenerics*: bool
    argtypes*: seq[TypeSymbol]
    sym*: Symbol
    fields*: seq[tuple[name: string, typesym: TypeSymbol]]
  StructConstructor* = ref object
    structsym*: TypeSymbol
    values*: seq[tuple[name: string, value: SemanticExpr]]
  FieldAccess* = ref object
    valuesym*: SemanticExpr
    fieldname*: string
  Cffi* = ref object
    name*: string
    primname*: string
    argtypes*: seq[TypeSymbol]
    rettype*: TypeSymbol
  CCodegenInfo* = object
    headers*: OrderedTable[string, bool]
    decls*: seq[string]
    cffis*: seq[Cffi]
  PrimitiveType* = ref object
    isGenerics*: bool
    name*: string
    primname*: string
    argtypes*: seq[TypeSymbol]
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
    index*: int
    name*: string
    semanticidents*: Table[SemanticIdent, SemanticIdentGroup]
    symbols*: seq[Symbol]
    toplevelcalls*: seq[SemanticExpr]
    ccodegenInfo*: CCodegenInfo
  FuncCall* = ref object
    callfunc*: TypeSymbol
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

proc `$`*(typesym: TypeSymbol): string
proc debug*(typesym: TypeSymbol): string
proc newModule*(context: SemanticContext, modulename: string): Module
proc newScope*(module: Module): Scope
proc newSymbol*(scope: Scope, name: string, semexpr: SemanticExpr, isImported = false): Symbol
proc `$`*(symbol: Symbol): string
proc debug*(symbol: Symbol): string
proc `==`*(a: Symbol, b: Symbol): bool
proc addSymbol*(scope: var Scope, semid: SemanticIdent, sym: Symbol)
proc evalSExpr*(scope: var Scope, sexpr: SExpr): SemanticExpr
proc evalFile*(context: SemanticContext, modulepath: string): Module
proc tryType*(scope: var Scope, span: Span, typename: string, argtypes: seq[TypeSymbol]): Option[TypeSymbol]
proc tryType*(scope: var Scope, sexpr: SExpr): Option[TypeSymbol]
proc trySymbol*(scope: var Scope, semid: SemanticIdent): Option[Symbol]
proc trySpecSymbol*(scope: var Scope, semid: SemanticIdent): Option[Symbol]
proc tryFuncSymbol*(scope: var Scope, semid: SemanticIdent): Option[Symbol]
proc isSpecTypes*(syms: seq[TypeSymbol]): bool

#
# Consts
#

let globalModule* = newModule(nil, "global")
let globalScope* = newScope(globalModule)

template notTypeSym*(): TypeSymbol =
  const internalname = instantiationInfo().filename
  const internalline = instantiationInfo().line
  let internalSpan = Span(line: 0, linepos: 0, internal: (internalname, internalline))
  TypeSymbol(kind: typesymVoid, internalspan: internalspan)

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
# Macros
#

macro newSemanticExpr*(span: Span, kind: SemanticExprKind, typesym: TypeSymbol, body: varargs[untyped]): SemanticExpr =
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

proc newSemanticIdent*(scope: Scope, span: Span, name: string, args: seq[TypeSymbol]): SemanticIdent =
  SemanticIdent(scope: scope, span: span, name: name, args: args)
proc newSemanticIdent*(scope: var Scope, sexpr: SExpr): SemanticIdent =
  if sexpr.kind == sexprList:
    var args = newSeq[Symbol]()
    for e in sexpr.rest:
      args.add(scope.trySymbol(scope.newSemanticIdent(e)).get)
    return newSemanticIdent(scope, sexpr.span, $sexpr.first, args.mapIt(it.semexpr.typesym))
  elif sexpr.kind == sexprIdent:
    return newSemanticIdent(scope, sexpr.span, $sexpr, @[])
  elif sexpr.kind == sexprAttr:
    return newSemanticIdent(scope, sexpr.span, $sexpr, @[])
  elif sexpr.kind == sexprInt:
    return scope.newSemanticIdent(sexpr.span, "Int32", @[])
  else:
    sexpr.span.raiseError("can't use $# instead ident" % $sexpr.kind)
proc newSemanticIdentGroup*(): SemanticIdentGroup =
  result.idsymbols = @[]
proc addSymbol*(symgroup: var SemanticIdentGroup, semid: SemanticIdent, symbol: Symbol) =
  symgroup.idsymbols.add((semid, symbol))
proc hash*(semid: SemanticIdent): Hash =
  hash(semid.name)
proc `==`*(a, b: SemanticIdent): bool =
  a.name == b.name
proc debug*(semid: SemanticIdent): string =
  if semid.args.len == 0:
    semid.name
  else:
    "(" & semid.name & " " & semid.args.mapIt(it.debug).join(" ") & ")"
proc `$`*(semid: SemanticIdent): string =
  if semid.args.len == 0:
    semid.name
  else:
    "(" & semid.name & " " & semid.args.mapIt($it).join(" ") & ")"

#
# TypeSymbol
#
proc getSymbol*(typesym: TypeSymbol): Symbol =
  case typesym.kind
  of typesymSpec:
    return typesym.specsym
  of typesymGenerics:
    return typesym.genericssym
  of typesymTypeGenerics:
    if typesym.genericstypes.isSpecTypes:
      return typesym.genericsoriginsym.scope.tryType(
        typesym.genericsoriginsym.semexpr.span,
        typesym.genericsoriginsym.name,
        typesym.genericstypes
      ).get.getSymbol()
    else:
      return typesym.genericsoriginsym
  of typesymVarargs:
    return typesym.varargssym.getSymbol()
  of typesymTypedesc:
    return typesym.typedescsym.getSymbol()
  of typesymReftype:
    return typesym.reftypesym.getSymbol()
  of typesymVoid:
    typesym.internalspan.raiseError("typesymVoid can't getSymbol")
proc getSemExpr*(typesym: TypeSymbol): SemanticExpr =
  return typesym.getSymbol().semexpr
proc `$`*(typesym: TypeSymbol): string =
  case typesym.kind
  of typesymSpec:
    $typesym.specsym
  of typesymGenerics:
    $typesym.genericssym
  of typesymTypeGenerics:
    "($# $#)" % [$typesym.genericsoriginsym, typesym.genericstypes.mapIt($it).join(" ")]
  of typesymVarargs:
    $typesym.varargssym
  of typesymTypedesc:
    $typesym.typedescsym
  of typesymReftype:
    $typesym.reftypesym
  of typesymVoid:
    "void"
proc debug*(typesym: TypeSymbol): string =
  case typesym.kind
  of typesymSpec:
    typesym.specsym.debug
  of typesymGenerics:
    $typesym.genericssym
  of typesymTypeGenerics:
    "($# $#)" % [$typesym.genericsoriginsym, typesym.genericstypes.mapIt(it.debug).join(" ")]
  of typesymVarargs:
    typesym.varargssym.debug
  of typesymTypedesc:
    typesym.typedescsym.debug
  of typesymReftype:
    typesym.reftypesym.debug
  of typesymVoid:
    "void"
proc getVoidSym*(scope: var Scope, span: Span): TypeSymbol =
  return scope.tryType(span, "Void", @[]).get

proc getSpecTypeSym*(typesym: TypeSymbol): TypeSymbol =
  case typesym.kind
  of typesymSpec:
    typesym
  of typesymGenerics:
    typesym.getSemExpr().typesym
  of typesymTypeGenerics:
    TypeSymbol(
      kind: typesymTypeGenerics,
      genericsoriginsym: typesym.genericsoriginsym,
      genericstypes: typesym.genericstypes.mapIt(it.getSpecTypeSym())
    )
  else:
    typesym
proc getSpecTypeSym*(semexpr: SemanticExpr): TypeSymbol =
  case semexpr.kind
  of semanticSymbol:
    semexpr.symbol.semexpr.getSpecTypeSym()
  of semanticArgType:
    semexpr.typesym.getSpecTypeSym()
  else:
    semexpr.typesym

#
# Symbol
#

proc newSymbol*(scope: Scope, name: string, semexpr: SemanticExpr, isImported = false): Symbol =
  Symbol(isImported: isImported, scope: scope, name: name, semexpr: semexpr)
proc globalSymbol*(name: string, semexpr: SemanticExpr): Symbol =
  newSymbol(globalScope, name, semexpr)
proc `$`*(symbol: Symbol): string =
  symbol.scope.module.name & "_" & symbol.name
proc debug*(modulename: string, name: string, syms: seq[TypeSymbol]): string =
  if syms.len == 0:
    "$#_$#" % [modulename, name]
  else:
    "($#_$# $#)" % [modulename, name, syms.mapIt(it.debug).join(" ")]
proc debug*(symbol: Symbol): string =
  if symbol.semexpr.kind == semanticPrimitiveFunc:
    debug(symbol.scope.module.name, symbol.name, symbol.semexpr.primfunc.fntype.argtypes)
  elif symbol.semexpr.kind == semanticFunction:
    debug(symbol.scope.module.name, symbol.name, symbol.semexpr.function.fntype.argtypes)
  elif symbol.semexpr.kind == semanticPrimitiveType:
    debug(symbol.scope.module.name, symbol.name, symbol.semexpr.primtype.argtypes)
  elif symbol.semexpr.kind == semanticStruct:
    debug(symbol.scope.module.name, symbol.name, symbol.semexpr.struct.argtypes)
  elif symbol.semexpr.kind == semanticTypeGenerics:
    debug(symbol.scope.module.name, symbol.name, @[symbol.semexpr.typegenerics.typ])
  else:
    symbol.scope.module.name & "_" & symbol.name
proc `==`*(a: Symbol, b: Symbol): bool =
  a.scope.module.name == b.scope.module.name and a.name == b.name
proc isEqualGenerics*(a: Symbol, b: Symbol): bool =
  a.scope.module.name == b.scope.module.name and a.name == b.name and a.semexpr.generics.scopeindex == b.semexpr.generics.scopeindex
proc isGenericsFunc*(sym: Symbol): bool =
  if sym.semexpr.kind == semanticPrimitiveFunc:
    return sym.semexpr.primfunc.isGenerics
  elif sym.semexpr.kind == semanticFunction:
    return sym.semexpr.function.isGenerics
  elif sym.semexpr.kind == semanticProtocolFunc:
    return true
  else:
    return false
proc isSpecType*(typesym: TypeSymbol): bool =
  case typesym.kind
  of typesymSpec:
    return true
  of typesymGenerics:
    return false
  of typesymTypeGenerics:
    for genericstype in typesym.genericstypes:
      if not genericstype.isSpecType:
        return false
    return true
  of typesymVarargs:
    return typesym.varargssym.isSpecType
  of typesymTypedesc:
    return typesym.typedescsym.isSpecType
  of typesymReftype:
    return typesym.reftypesym.isSpecType
  of typesymVoid:
    return true
proc isSpecTypes*(syms: seq[TypeSymbol]): bool =
  for sym in syms:
    if not sym.isSpecType:
      return false
  return true
proc isReturnType*(scope: var Scope, sym: TypeSymbol, rettype: TypeSymbol): bool =
  if sym.kind == typesymVoid:
    return false
  elif sym.getSymbol().name == "Void":
    return false
  elif sym.getSymbol() == rettype.getSymbol():
    return true
  else:
    return false
proc isVariableSym*(sym: Symbol): bool =
  return sym.semexpr.kind == semanticSymbol and sym.semexpr.symbol.semexpr.kind == semanticVariable

#
# Semantic Symbol Table
#

proc match*(semid, gsemid: SemanticIdent): bool

proc match*(arg, garg: TypeSymbol): bool =
  if arg.kind == typesymSpec and garg.kind == typesymSpec:
    return arg.specsym == garg.specsym
  elif garg.kind == typesymGenerics:
    return true
  elif arg.kind == typesymSpec and garg.kind == typesymTypeGenerics:
    return arg.specoriginsym.isSome and arg.specoriginsym.get == garg.genericsoriginsym
  elif arg.kind == typesymGenerics and garg.kind == typesymTypeGenerics:
    return true
  elif arg.kind == typesymTypeGenerics and garg.kind == typesymTypeGenerics:
    return arg.genericsoriginsym == garg.genericsoriginsym
  elif arg.kind == typesymSpec and garg.kind == typesymVarargs:
    return match(arg, garg.varargssym) # FIXME:
  elif arg.kind == typesymReftype and garg.kind == typesymReftype:
    return match(arg.reftypesym, garg.reftypesym)
  elif garg.kind == typesymReftype:
    return match(arg, garg.reftypesym)
  elif garg.kind == typesymTypedesc:
    return true
  else:
    return false
proc equal*(arg, garg: TypeSymbol): bool =
  if arg.kind == typesymSpec and garg.kind == typesymSpec:
    return arg.specsym == garg.specsym
  else:
    return false

proc match*(semid, gsemid: SemanticIdent): bool =
  for i in 0..<gsemid.args.len:
    if not match(semid.args[i], gsemid.args[i]):
      return false
  return true
proc equal*(semid, gsemid: SemanticIdent): bool =
  for i in 0..<gsemid.args.len:
    if not equal(semid.args[i], gsemid.args[i]):
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

proc trySymbol*(semids: var Table[SemanticIdent, SemanticIdentGroup], semid: SemanticIdent): Option[Symbol] =
  if semids.hasKey(semid):
    let symgroup = semids[semid]
    for gsemidpair in symgroup.idsymbols:
      if gsemidpair.semid.args.len != semid.args.len:
        if matchVarargs(semid, gsemidpair.semid):
          return some(gsemidpair.value)
      elif match(semid, gsemidpair.semid):
        return some(gsemidpair.value)
    return none(Symbol)
  else:
    return none(Symbol)
proc trySpecSymbol*(semids: var Table[SemanticIdent, SemanticIdentGroup], semid: SemanticIdent): Option[Symbol] =
  if semids.hasKey(semid):
    let symgroup = semids[semid]
    for gsemidpair in symgroup.idsymbols:
      if gsemidpair.semid.args.len != semid.args.len:
        continue
      elif equal(semid, gsemidpair.semid):
        return some(gsemidpair.value)
  return none(Symbol)
proc tryFuncSymbol*(semids: var Table[SemanticIdent, SemanticIdentGroup], semid: SemanticIdent): Option[Symbol] =
  if semids.hasKey(semid):
    let symgroup = semids[semid]
    for gsemidpair in symgroup.idsymbols:
      if gsemidpair.semid.args.len != semid.args.len:
        continue
      elif match(semid, gsemidpair.semid) and gsemidpair.value.semexpr.kind != semanticProtocolFunc:
        return some(gsemidpair.value)
  return none(Symbol)

proc addSymbol*(semids: var Table[SemanticIdent, SemanticIdentGroup], semid: SemanticIdent, symbol: Symbol) =
  if semids.trySpecSymbol(semid).isSome:
    symbol.raiseError("can't redefine symbol: $#" % [semid.name])
  if not semids.hasKey(semid):
    semids[semid] = newSemanticIdentGroup()
  semids[semid].addSymbol(semid, symbol)

#
# TypeSymbol from Symbol
#

proc getTypeSymbol*(sym: Symbol): TypeSymbol =
  if sym.semexpr.kind == semanticGenerics:
    let semexpr = newSemanticExpr(sym.semexpr.span, semanticGenericsChild, sym.semexpr.typesym, parent: sym)
    let sym = sym.scope.newSymbol(sym.name, semexpr)
    return getTypeSymbol(sym)
  elif sym.semexpr.kind == semanticGenericsChild:
    return TypeSymbol(kind: typesymGenerics, genericssym: sym)
  elif sym.semexpr.kind == semanticPrimitiveType:
    if sym.semexpr.primtype.argtypes.isSpecTypes:
      return TypeSymbol(kind: typesymSpec, specoriginsym: some(sym), specsym: sym)
    else:
      return TypeSymbol(
        kind: typesymTypeGenerics,
        genericsoriginsym: sym,
        genericstypes: sym.semexpr.primtype.argtypes
      )
  elif sym.semexpr.kind == semanticStruct:
    if sym.semexpr.struct.argtypes.isSpecTypes:
      return TypeSymbol(kind: typesymSpec, specoriginsym: some(sym), specsym: sym)
    else:
      return TypeSymbol(
        kind: typesymTypeGenerics,
        genericsoriginsym: sym,
        genericstypes: sym.semexpr.struct.argtypes
      )
  elif sym.semexpr.kind == semanticPrimitiveFunc:
    if sym.semexpr.primfunc.fntype.argtypes.isSpecTypes:
      return TypeSymbol(kind: typesymSpec, specoriginsym: some(sym), specsym: sym)
    else:
      return TypeSymbol(
        kind: typesymTypeGenerics,
        genericsoriginsym: sym,
        genericstypes: sym.semexpr.primfunc.fntype.argtypes
      )
  elif sym.semexpr.kind == semanticFunction:
    if sym.semexpr.function.fntype.argtypes.isSpecTypes:
      return TypeSymbol(kind: typesymSpec, specoriginsym: some(sym), specsym: sym)
    else:
      return TypeSymbol(
        kind: typesymTypeGenerics,
        genericsoriginsym: sym,
        genericstypes: sym.semexpr.function.fntype.argtypes
      )
  elif sym.semexpr.kind == semanticProtocolFunc:
    return TypeSymbol(
      kind: typesymTypeGenerics,
      genericsoriginsym: sym,
      genericstypes: sym.semexpr.protocolfntype.argtypes
    )
  elif sym.semexpr.kind == semanticPrimitiveEval:
    return TypeSymbol(kind: typesymSpec, specsym: sym)
  elif sym.semexpr.kind == semanticVarargsType:
    return TypeSymbol(kind: typesymVarargs, varargssym: sym.semexpr.typesym)
  elif sym.semexpr.kind == semanticTypedesc:
    return TypeSymbol(kind: typesymTypedesc, typedescsym: sym.semexpr.typesym)
  elif sym.semexpr.kind == semanticReftype:
    return TypeSymbol(kind: typesymReftype, reftypesym: sym.semexpr.typesym)
  else:
    sym.semexpr.span.raiseError("$#:$# is not type symbol" % [$sym, $sym.semexpr.kind])

#
# SemanticExpr
#

proc expectSemantic*(semexpr: SemanticExpr, kind: SemanticExprKind) =
  if semexpr.kind != kind:
    semexpr.raiseError("expression is not $#, actually: $#" % [$kind, $semexpr.kind])
proc newSemanticExpr*(sym: TypeSymbol): SemanticExpr =
  return newSemanticExpr(sym.getSemExpr().span, semanticSymbol, sym.getSemExpr().typesym, symbol: sym.getSymbol())
proc newSemanticExpr*(sym: Symbol): SemanticExpr =
  return newSemanticExpr(sym.semexpr.span, semanticSymbol, sym.semexpr.typesym, symbol: sym)

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
  result.index = 0
  result.name = modulename
  result.semanticidents = initTable[SemanticIdent, SemanticIdentGroup]()
  result.symbols = @[]
  result.toplevelcalls = @[]
  result.ccodegenInfo = newCCodegenInfo()

proc genindex*(module: Module): int =
  result = module.index
  module.index.inc
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
proc genindex*(scope: var Scope): int =
  return scope.module.genindex
proc addSymbol*(scope: var Scope, semid: SemanticIdent, sym: Symbol) =
  scope.semanticidents.addSymbol(semid, sym)
proc trySymbol*(scope: var Scope, semid: SemanticIdent): Option[Symbol] =
  let opt = scope.semanticidents.trySymbol(semid)
  if opt.isSome:
    return opt
  else:
    return scope.module.semanticidents.trySymbol(semid)
proc trySpecSymbol*(scope: var Scope, semid: SemanticIdent): Option[Symbol] =
  let opt = scope.semanticidents.trySpecSymbol(semid)
  if opt.isSome:
    return opt
  else:
    return scope.module.semanticidents.trySpecSymbol(semid)
proc tryFuncSymbol*(scope: var Scope, semid: SemanticIdent): Option[Symbol] =
  let opt = scope.semanticidents.tryFuncSymbol(semid)
  if opt.isSome:
    return opt
  else:
    return scope.module.semanticidents.tryFuncSymbol(semid)

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

proc defPrimitiveType*(scope: var Scope, span: Span, argtypes: seq[string], typename: string, primname: string): Symbol =
  let argtypesyms = argtypes.mapIt(scope.tryType(span, it, @[]).get)
  let semexpr = newSemanticExpr(
    span,
    semanticPrimitiveType,
    notTypeSym,
    primtype: PrimitiveType(
      isGenerics: false,
      name: typename,
      primname: primname,
      argtypes: argtypesyms
    )
  )
  let sym = newSymbol(scope, typename, semexpr)
  semexpr.typesym = getTypeSymbol(sym)
  let semid = scope.newSemanticIdent(span, typename, argtypesyms)
  scope.module.addSymbol(semid, sym)
  return sym
proc defPrimitiveValue*(scope: var Scope, span: Span, typename: string, valuename: string, value: string) =
  var semexpr = newSemanticExpr(
    span,
    semanticPrimitiveValue,
    scope.tryType(span, typename, @[]).get,
    primValue: value
  )
  let sym = newSymbol(scope, valuename, semexpr)
  scope.module.addSymbol(scope.newSemanticIdent(span, valuename, @[]), sym)
proc defPrimitiveFunc*(scope: var Scope, span: Span, isPattern: bool, funcname: string, argtypes: seq[TypeSymbol], rettype: TypeSymbol, kind: PrimitiveFuncKind, primname: string): Symbol =
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
  let semid = scope.newSemanticIdent(span, funcname, argtypes)
  scope.module.addSymbol(semid, sym)
  return sym
proc defPrimitiveEval*(scope: var Scope, span: Span, macroname: string, evalproc: proc (scope: var Scope, sexpr: SExpr): SemanticExpr) =
  let semexpr = newSemanticExpr(span, semanticPrimitiveEval, notTypeSym, evalproc: evalproc)
  let sym = newSymbol(scope, macroname, semexpr, isImported = true)
  let semid = scope.newSemanticIdent(span, macroname, @[])
  scope.module.addSymbol(semid, sym)

#
# Type Specialize
#

proc parseTypeAnnotation*(sexpr: SExpr, isAnnot = true): tuple[argtypes: seq[SExpr], rettype: SExpr, body: SExpr] =
  var argtypes = newSeq[SExpr]()
  var rettype: SExpr
  let body = sexpr.last
  sexpr.rest.each(arg):
    if $arg.first == "->":
      rettype = arg.rest.first
      break
    elif isAnnot and arg.rest.kind == sexprNil:
      break
    else:
      argtypes.add(arg.first)
  if rettype == nil:
    rettype = newSIdent(sexpr.span, "Void")
  return (argtypes, rettype, body)

proc getTypeAnnotation*(scope: var Scope, sexpr: SExpr, isAnnot = true): tuple[argtypes: seq[TypeSymbol], rettype: TypeSymbol, body: SExpr] =
  let (argtypes, rettype, body) = parseTypeAnnotation(sexpr, isAnnot)
  var argtypesyms = newSeq[TypeSymbol]()
  for argtype in argtypes:
    let tryargtype = scope.tryType(argtype)
    if tryargtype.isNone:
      argtype.span.raiseError("undeclared type: $#" % $argtype)
    argtypesyms.add(tryargtype.get)
  let rettypesym = scope.tryType(rettype).get
  return (argtypesyms, rettypesym, body)

proc addArgSymbols*(scope: var Scope, argtypesyms: seq[TypeSymbol], funcdef: SExpr) =
  if funcdef.rest.rest.first.kind == sexprNil:
    return
  if funcdef.rest.rest.first.len != argtypesyms.len:
    raise newException(SemanticError, "($#:$#) argument length is not equals type length" % [$funcdef.span.line, $funcdef.span.linepos])
  for i, arg in funcdef.rest.rest.first:
    var semexpr = newSemanticExpr(funcdef.span, semanticArgType, argtypesyms[i], argtypeident: $arg)
    let sym = newSymbol(scope, $arg, semexpr)
    scope.addSymbol(scope.newSemanticIdent(arg.span, $arg, @[]), sym)

# proc genGenericsType*(scope: var Scope, genericssym: Symbol, fntypesym: TypeSymbol, argtypesym: TypeSymbol): TypeSymbol =
#   case fntypesym.kind
#   of typesymSpec:
#     return fntypesym
#   of typesymGenerics:
#     if genericssym == fntypesym.getSymbol():
#       return argtypesym
#     else:
#       return TypeSymbol(kind: typesymGenerics, genericssym: genericssym)
#   of typesymTypeGenerics:
#     for i, fngenericstype in fntypesym.genericstypes:
#       if genericssym == fngenericstype.getSymbol(): # FIXME:
#         if argtypesym.kind == typesymSpec:
#           let semexpr = argtypesym.getSemExpr()
#           if semexpr.kind == semanticPrimitiveType:
#             return semexpr.primtype.argtypes[i]
#           elif semexpr.kind == semanticStruct:
#             return semexpr.struct.argtypes[i]
#           else:
#             argtypesym.getSymbol().raiseError("$# is not type" % $argtypesym.getSymbol())
#         else:
#           return argtypesym.genericstypes[i]
#     return TypeSymbol(kind: typesymGenerics, genericssym: genericssym)
#   of typesymVarargs:
#     return scope.genGenericsType(genericssym, fntypesym.varargssym, argtypesym)
#   of typesymTypedesc:
#     return scope.genGenericsType(genericssym, fntypesym.typedescsym, argtypesym)
#   of typesymReftype:
#     return scope.genGenericsType(genericssym, fntypesym.reftypesym, argtypesym)
#   of typesymVoid:
#     fntypesym.internalspan.raiseError("apply void to genGenericsType")
# proc genGenericsType*(scope: var Scope, genericssym: Symbol, fntypesyms: seq[TypeSymbol], argtypesyms: seq[TypeSymbol]): TypeSymbol =
#   if fntypesyms.len != argtypesyms.len:
#     return TypeSymbol(kind: typesymGenerics, genericssym: genericssym)
#   for i, fntypesym in fntypesyms:
#     let ret = scope.genGenericsType(genericssym, fntypesym, argtypesyms[i])
#     if ret.isSpecType:
#       if genericssym.semexpr.kind == semanticGenerics:
#         for child in genericssym.semexpr.generics.children:
#           child.typesym = ret
#       return ret
#   return TypeSymbol(kind: typesymGenerics, genericssym: genericssym)
# proc genReturnType*(scope: var Scope, rettype: TypeSymbol, fntypesyms: seq[TypeSymbol], argtypesyms: seq[TypeSymbol]): TypeSymbol =
#   case rettype.kind
#   of typesymSpec:
#     return rettype
#   of typesymGenerics:
#     return scope.genGenericsType(rettype.genericssym, fntypesyms, argtypesyms)
#   of typesymTypeGenerics:
#     result = TypeSymbol(kind: typesymTypeGenerics, genericsoriginsym: rettype.genericsoriginsym, genericstypes: @[])
#     for i, fngenericstype in rettype.genericstypes:
#       result.genericstypes.add(scope.genReturnType(fngenericstype, fntypesyms, argtypesyms))
#   of typesymVarargs:
#     return scope.genReturnType(rettype.varargssym, fntypesyms, argtypesyms)
#   of typesymTypedesc:
#     return scope.genReturnType(rettype.typedescsym, fntypesyms, argtypesyms)
#   of typesymReftype:
#     return scope.genReturnType(rettype.reftypesym, fntypesyms, argtypesyms)
#   of typesymVoid:
#     return rettype

proc specGenericsType*(scope: var Scope, typesym: TypeSymbol, specsym: TypeSymbol) =
  case typesym.kind
  of typesymSpec:
    discard
  of typesymGenerics:
    for child in typesym.genericssym.semexpr.parent.semexpr.generics.children:
      child.typesym = specsym
  of typesymTypeGenerics:
    if specsym.kind == typesymTypeGenerics:
      for i in 0..<typesym.genericstypes.len:
        scope.specGenericsType(typesym.genericstypes[i], specsym.genericstypes[i])
    elif specsym.kind == typesymSpec:
      if specsym.getSemExpr().kind == semanticPrimitiveType:
        for i in 0..<typesym.genericstypes.len:
          scope.specGenericsType(typesym.genericstypes[i], specsym.getSemExpr().primtype.argtypes[i])
      elif specsym.getSemExpr().kind == semanticStruct:
        for i in 0..<typesym.genericstypes.len:
          scope.specGenericsType(typesym.genericstypes[i], specsym.getSemExpr().struct.argtypes[i])
      else:
        specsym.getSymbol().raiseError("$# couldn't specGenericsType: $#" % [$specsym.getSymbol(), $specsym.getSemExpr().kind])
    elif specsym.kind == typesymVarargs:
      scope.specGenericsType(typesym, specsym.varargssym)
    elif specsym.kind == typesymTypedesc:
      scope.specGenericsType(typesym, specsym.typedescsym)
    elif specsym.kind == typesymReftype:
      scope.specGenericsType(typesym, specsym.reftypesym)
    else:
      specsym.getSymbol().raiseError("$# couldn't specGenericsType: $#" % [$specsym.getSymbol(), $specsym.kind])
  of typesymVarargs:
    scope.specGenericsType(typesym.varargssym, specsym)
  of typesymTypedesc:
    scope.specGenericsType(typesym.typedescsym, specsym)
  of typesymReftype:
    scope.specGenericsType(typesym.reftypesym, specsym)
  of typesymVoid:
    discard
proc specGenericsTypes*(scope: var Scope, typesyms: seq[TypeSymbol], specsyms: seq[TypeSymbol]) =
  for i in 0..<typesyms.len:
    scope.specGenericsType(typesyms[i], specsyms[i])

#
# Generics
#


proc genSpecGenericsPrimitiveType*(scope: var Scope, typesym: TypeSymbol, typesyms: seq[TypeSymbol]): TypeSymbol
proc genSpecGenericsStruct*(scope: var Scope, typesym: TypeSymbol, typesyms: seq[TypeSymbol]): TypeSymbol
proc genSpecGenericsFunc*(scope: var Scope, funcsym: TypeSymbol, typesyms: seq[TypeSymbol]): TypeSymbol
proc genSpecGenericsPrimitiveFunc*(scope: var Scope, funcsym: TypeSymbol, typesyms: seq[TypeSymbol]): TypeSymbol

proc genSpecGenerics*(scope: var Scope, semexpr: SemanticExpr): SemanticExpr =
  case semexpr.kind
  # of semanticGenerics, semanticTypeGenerics, semanticTypedesc:
  of semanticVariable:
    result = newSemanticExpr(
      semexpr.span,
      semanticVariable,
      notTypeSym,
      variable: Variable(
        scope: semexpr.variable.scope,
        name: semexpr.variable.name,
        value: scope.genSpecGenerics(semexpr.variable.value)
      )
    )
    result.typesym = semexpr.typesym
  of semanticGenericsChild:
    result = semexpr.getSpecTypeSym().getSemExpr()
  of semanticStructConstructor:
    var values = newSeq[tuple[name: string, value: SemanticExpr]]()
    for valuepair in semexpr.structconstructor.values:
      values.add((valuepair.name, scope.genSpecGenerics(valuepair.value)))
    let structtype = scope.genSpecGenericsStruct(semexpr.structconstructor.structsym, semexpr.structconstructor.structsym.genericstypes.mapIt(it.getSpecTypeSym()))
    result = newSemanticExpr(
      semexpr.span,
      semanticStructConstructor,
      notTypeSym,
      structconstructor: StructConstructor(
        structsym: structtype,
        values: values
      )
    )
    result.typesym = structtype
  of semanticFuncCall:
    let args = semexpr.funccall.args.mapIt(scope.genSpecGenerics(it))
    # let argtypes = args.mapIt(it.getSpecTypeSym())
    let argtypes = args.mapIt(it.getSpecTypeSym())
    if semexpr.funccall.callfunc.getSemExpr().kind == semanticFunction:
      let callfuncsym = scope.genSpecGenericsFunc(semexpr.funccall.callfunc, argtypes)
      result = newSemanticExpr(
        semexpr.span,
        semanticFuncCall,
        notTypeSym,
        funccall: FuncCall(
          callfunc: callfuncsym,
          args: args
        )
      )
      result.typesym = callfuncsym.getSemExpr().typesym
    elif semexpr.funccall.callfunc.getSemExpr().kind == semanticProtocolFunc:
      # let protoargtypes = semexpr.funccall.callfunc.getSemExpr().protocolfntype.argtypes
      # scope.specGenericsTypes(protoargtypes, argtypes)
      # let semid = scope.newSemanticIdent(semexpr.span, semexpr.funccall.callfunc.getSymbol().name, protoargtypes.mapIt(it.getSpecTypeSym()))
      # let semid = scope.newSemanticIdent(semexpr.span, semexpr.funccall.callfunc.getSymbol().name, argtypes)
      # let specfuncsym = scope.tryFuncSymbol(semid)
      # if specfuncsym.isNone:
      #   semexpr.raiseError("undeclared function by protocol: $#" % semid.debug)
      let specfuncsym = scope.tryType(semexpr.span, semexpr.funccall.callfunc.getSymbol().name, argtypes)
      if specfuncsym.isNone:
        semexpr.raiseError("undeclared function by protocol: $#" % semexpr.funccall.callfunc.debug)
      result = newSemanticExpr(
        semexpr.span,
        semanticFuncCall,
        notTypeSym,
        funccall: FuncCall(
          callfunc: specfuncsym.get,
          args: args
        )
      )
      result.typesym = specfuncsym.get.getSemExpr().typesym # FIXME:
    elif semexpr.funccall.callfunc.getSemExpr().kind == semanticPrimitiveType: # FIXME:
      let callfuncsym = scope.genSpecGenericsPrimitiveType(semexpr.funccall.callfunc, argtypes) # FIXME:
      result = newSemanticExpr(
        semexpr.span,
        semanticFuncCall,
        notTypeSym,
        funccall: FuncCall(
          callfunc: callfuncsym,
          args: args
        )
      )
      result.typesym = callfuncsym.getSemExpr().typesym
    else: # primitiveFunc
      let callfuncsym = scope.genSpecGenericsPrimitiveFunc(semexpr.funccall.callfunc, argtypes)
      result = newSemanticExpr(
        semexpr.span,
        semanticFuncCall,
        notTypeSym,
        funccall: FuncCall(
          callfunc: callfuncsym,
          args: args
        )
      )
      result.typesym = callfuncsym.getSemExpr().typesym
  of semanticFieldAccess:
    let retsemexpr = scope.genSpecGenerics(semexpr.fieldaccess.valuesym)
    result = newSemanticExpr(
      semexpr.span,
      semanticFieldAccess,
      notTypeSym,
      fieldaccess: FieldAccess(
        valuesym: retsemexpr,
        fieldname: semexpr.fieldaccess.fieldname,
      ),
    )
    result.typesym = retsemexpr.typesym # FIXME:
  of semanticWhileSyntax:
    result = newSemanticExpr(
      semexpr.span,
      semanticWhileSyntax,
      notTypeSym,
      whilesyntax: WhileSyntax(
        cond: scope.genSpecGenerics(semexpr.whilesyntax.cond),
        body: semexpr.whilesyntax.body.mapIt(scope.genSpecGenerics(it))
      )
    )
  else:
    return semexpr

proc genSpecGenericsFunc*(scope: var Scope, funcsym: TypeSymbol, typesyms: seq[TypeSymbol]): TypeSymbol =
  funcsym.getSemExpr().expectSemantic(semanticFunction)
  let funcname = funcsym.getSemExpr().function.sym.name
  scope.specGenericsTypes(funcsym.getSemExpr().function.fntype.argtypes, typesyms)
  let rettype = funcsym.getSemExpr().function.fntype.returntype.getSpecTypeSym()
  # let rettype = scope.genReturnType(
  #   funcsym.getSemExpr().function.fntype.returntype,
  #   funcsym.getSemExpr().function.fntype.argtypes,
  #   typesyms
  # )
  var argtypes = newSeq[TypeSymbol]()
  for i, argtype in funcsym.getSemExpr().function.fntype.argtypes:
    if argtype.kind == typesymTypedesc:
      argtypes.add(TypeSymbol(kind: typesymTypedesc, typedescsym: typesyms[i]))
    elif argtype.kind == typesymVarargs:
      argtypes.add(TypeSymbol(kind: typesymVarargs, varargssym: typesyms[i]))
    elif argtype.kind == typesymReftype:
      argtypes.add(TypeSymbol(kind: typesymReftype, reftypesym: typesyms[i]))
    else:
      argtypes.add(typesyms[i])
    # argtypes.add(argtype.getSpecTypeSym())
  let f = Function(
    isGenerics: not typesyms.isSpecTypes,
    argnames: funcsym.getSemExpr().function.argnames,
    fntype: FuncType(
      returntype: rettype,
      argtypes: argtypes
    ),
    body: funcsym.getSemExpr().function.body.mapIt(scope.genSpecGenerics(it))
  )
  let semexpr = newSemanticExpr(funcsym.getSemExpr().span, semanticFunction, rettype, function: f)
  let sym = newSymbol(funcsym.getSymbol().scope, funcname, semexpr)
  f.sym = sym
  let semid = newSemanticIdent(scope, funcsym.getSemExpr().span, funcname, typesyms)
  let rettypesym = getTypeSymbol(sym)
  if scope.trySpecSymbol(semid).isSome:
    return rettypesym
  scope.module.addSymbol(semid, sym)
  return rettypesym

# proc genSpecGenericsFunc*(scope: var Scope, funcsym: TypeSymbol, typesyms: seq[TypeSymbol]): TypeSymbol =
#   funcsym.getSemExpr().expectSemantic(semanticFunction)
#   if typesyms.isSpecTypes:
#     let funcname = funcsym.getSemExpr().function.sym.name
#     scope.specGenericsTypes(funcsym.getSemExpr().function.fntype.argtypes, typesyms)
#     let rettype = funcsym.getSemExpr().function.fntype.returntype.getSpecTypeSym()
#     # let rettype = scope.genReturnType(
#     #   funcsym.getSemExpr().function.fntype.returntype,
#     #   funcsym.getSemExpr().function.fntype.argtypes,
#     #   typesyms
#     # )
#     var argtypes = newSeq[TypeSymbol]()
#     for i, argtype in funcsym.getSemExpr().function.fntype.argtypes:
#       if argtype.kind == typesymTypedesc:
#         argtypes.add(TypeSymbol(kind: typesymTypedesc, typedescsym: typesyms[i]))
#       elif argtype.kind == typesymVarargs:
#         argtypes.add(TypeSymbol(kind: typesymVarargs, varargssym: typesyms[i]))
#       elif argtype.kind == typesymReftype:
#         argtypes.add(TypeSymbol(kind: typesymReftype, reftypesym: typesyms[i]))
#       else:
#         argtypes.add(typesyms[i])
#     let f = Function(
#       isGenerics: not typesyms.isSpecTypes,
#       argnames: funcsym.getSemExpr().function.argnames,
#       fntype: FuncType(
#         returntype: rettype,
#         argtypes: argtypes
#       ),
#       body: funcsym.getSemExpr().function.body.mapIt(scope.genSpecGenerics(it))
#     )
#     let semexpr = newSemanticExpr(funcsym.getSemExpr().span, semanticFunction, rettype, function: f)
#     let sym = newSymbol(funcsym.getSymbol().scope, funcname, semexpr)
#     f.sym = sym
#     let semid = newSemanticIdent(scope, funcsym.getSemExpr().span, funcname, typesyms)
#     let rettypesym = getTypeSymbol(sym)
#     if scope.trySpecSymbol(semid).isSome:
#       return rettypesym
#     scope.module.addSymbol(semid, sym)
#     return rettypesym
#   else:
#     return TypeSymbol(kind: typesymTypeGenerics, genericsoriginsym: funcsym.getSymbol(), genericstypes: typesyms)

proc genSpecGenericsPrimitiveFunc*(scope: var Scope, funcsym: TypeSymbol, typesyms: seq[TypeSymbol]): TypeSymbol =
  funcsym.getSemExpr().expectSemantic(semanticPrimitiveFunc)
  let funcname = funcsym.getSemExpr().primfunc.name
  scope.specGenericsTypes(funcsym.getSemExpr().primfunc.fntype.argtypes, typesyms)
  let rettype = funcsym.getSemExpr().primfunc.fntype.returntype.getSpecTypeSym()
  # let rettype = scope.genReturnType(
  #   funcsym.getSemExpr().primfunc.fntype.returntype,
  #   funcsym.getSemExpr().primfunc.fntype.argtypes,
  #   typesyms
  # )
  let f = PrimitiveFunc(
    isGenerics: not typesyms.isSpecTypes,
    kind: funcsym.getSemExpr().primfunc.kind,
    pattern: funcsym.getSemExpr().primfunc.pattern,
    name: funcname,
    fntype: FuncType(
      returntype: rettype,
      argtypes: typesyms
    )
  )
  let semexpr = newSemanticExpr(funcsym.getSemExpr().span, semanticPrimitiveFunc, rettype, primfunc: f)
  let sym = newSymbol(scope, funcsym.getSymbol().name, semexpr)
  let semid = newSemanticIdent(scope, funcsym.getSemExpr().span, funcname, typesyms)
  let rettypesym = getTypeSymbol(sym)
  if scope.trySpecSymbol(semid).isSome:
    return rettypesym
  scope.module.addSymbol(semid, sym)
  return rettypesym

# proc genSpecGenericsPrimitiveFunc*(scope: var Scope, funcsym: TypeSymbol, typesyms: seq[TypeSymbol]): TypeSymbol =
#   funcsym.getSemExpr().expectSemantic(semanticPrimitiveFunc)
#   if typesyms.isSpecTypes:
#     let funcname = funcsym.getSemExpr().primfunc.name
#     # scope.specGenericsTypes(funcsym.getSemExpr().primfunc.fntype.argtypes, typesyms)
#     # let rettype = funcsym.getSemExpr().primfunc.fntype.returntype
#     let rettype = scope.genReturnType(
#       funcsym.getSemExpr().primfunc.fntype.returntype,
#       funcsym.getSemExpr().primfunc.fntype.argtypes,
#       typesyms
#     )
#     let f = PrimitiveFunc(
#       isGenerics: not typesyms.isSpecTypes,
#       kind: funcsym.getSemExpr().primfunc.kind,
#       pattern: funcsym.getSemExpr().primfunc.pattern,
#       name: funcname,
#       fntype: FuncType(
#         returntype: rettype,
#         argtypes: typesyms
#       )
#     )
#     let semexpr = newSemanticExpr(funcsym.getSemExpr().span, semanticPrimitiveFunc, rettype, primfunc: f)
#     let sym = newSymbol(scope, funcsym.getSymbol().name, semexpr)
#     let semid = newSemanticIdent(scope, funcsym.getSemExpr().span, funcname, typesyms)
#     let rettypesym = getTypeSymbol(sym)
#     if scope.trySpecSymbol(semid).isSome:
#       return rettypesym
#     scope.module.addSymbol(semid, sym)
#     return rettypesym
#   else:
#     return TypeSymbol(kind: typesymTypeGenerics, genericsoriginsym: funcsym.getSymbol(), genericstypes: typesyms)

proc genSpecGenericsPrimitiveType*(scope: var Scope, typesym: TypeSymbol, typesyms: seq[TypeSymbol]): TypeSymbol =
  typesym.getSemExpr().expectSemantic(semanticPrimitiveType)
  if typesyms.isSpecTypes:
    scope.specGenericsTypes(typesym.getSemExpr().primtype.argtypes, typesyms)
    let semexpr = newSemanticExpr(
      typesym.getSemExpr().span,
      semanticPrimitiveType,
      notTypeSym,
      primtype: PrimitiveType(
        isGenerics: false,
        name: typesym.getSemExpr().primtype.name,
        primname: typesym.getSemExpr().primtype.primname,
        argtypes: typesyms
      )
    )
    let sym = newSymbol(typesym.getSymbol().scope, typesym.getSemExpr().primtype.name, semexpr)
    let rettypesym = getTypeSymbol(sym)
    semexpr.typesym = rettypesym
    let semid = scope.newSemanticIdent(typesym.getSemExpr().span, typesym.getSymbol().name, typesyms)
    if scope.trySpecSymbol(semid).isSome:
      return rettypesym
    scope.module.addSymbol(semid, sym)
    return rettypesym
  else:
    return TypeSymbol(kind: typesymTypeGenerics, genericsoriginsym: typesym.getSymbol(), genericstypes: typesyms)

# proc genSpecGenericsStruct*(scope: var Scope, typesym: TypeSymbol, typesyms: seq[TypeSymbol]): TypeSymbol =
#   typesym.getSemExpr().expectSemantic(semanticStruct)
#   scope.specGenericsTypes(typesym.getSemExpr().struct.argtypes, typesyms)
#   var fields = newSeq[tuple[name: string, typesym: TypeSymbol]]()
#   for field in typesym.getSemExpr().struct.fields:
#     fields.add((field.name, field.typesym.getSpecTypeSym()))
#   let struct = Struct(
#       isGenerics: not typesyms.isSpecTypes,
#       argtypes: typesyms,
#       sym: typesym.getSemExpr().struct.sym,
#       fields: fields,
#     )
#   let semexpr = newSemanticExpr(
#     typesym.getSemExpr().span,
#     semanticStruct,
#     notTypeSym,
#     struct: struct
#   )
#   let sym = newSymbol(typesym.getSymbol().scope, typesym.getSemExpr().struct.sym.name, semexpr)
#   struct.sym = sym
#   let semid = scope.newSemanticIdent(typesym.getSemExpr().span, typesym.getSemExpr().struct.sym.name, typesyms)
#   let rettypesym = getTypeSymbol(sym)
#   semexpr.typesym = rettypesym
#   for module in scope.module.context.modules.values: # FIXME:
#     let opt = module.semanticidents.trySpecSymbol(semid)
#     if opt.isSome:
#       return getTypeSymbol(opt.get)
#   # if scope.trySpecSymbol(semid).isSome:
#   #   return rettypesym
#   scope.module.addSymbol(semid, sym)
#   return rettypesym

proc genSpecGenericsStruct*(scope: var Scope, typesym: TypeSymbol, typesyms: seq[TypeSymbol]): TypeSymbol =
  typesym.getSemExpr().expectSemantic(semanticStruct)
  if typesyms.isSpecTypes:
    scope.specGenericsTypes(typesym.getSemExpr().struct.argtypes, typesyms)
    var fields = newSeq[tuple[name: string, typesym: TypeSymbol]]()
    for field in typesym.getSemExpr().struct.fields:
      fields.add((field.name, field.typesym.getSpecTypeSym()))
    let struct = Struct(
      isGenerics: false,
      argtypes: typesyms,
      sym: typesym.getSemExpr().struct.sym,
      fields: fields,
    )
    let semexpr = newSemanticExpr(
      typesym.getSemExpr().span,
      semanticStruct,
      notTypeSym,
      struct: struct
    )
    let sym = newSymbol(typesym.getSymbol().scope, typesym.getSemExpr().struct.sym.name, semexpr)
    struct.sym = sym
    let semid = scope.newSemanticIdent(typesym.getSemExpr().span, typesym.getSemExpr().struct.sym.name, typesyms)
    let rettypesym = getTypeSymbol(sym)
    semexpr.typesym = rettypesym
    for module in scope.module.context.modules.values: # FIXME:
      let opt = module.semanticidents.trySpecSymbol(semid)
      if opt.isSome:
        return getTypeSymbol(opt.get)
    # if scope.trySpecSymbol(semid).isSome:
    #   return rettypesym
    scope.module.addSymbol(semid, sym)
    return rettypesym
  else:
    return TypeSymbol(kind: typesymTypeGenerics, genericsoriginsym: typesym.getSymbol(), genericstypes: typesyms)

#
# Eval
#

proc tryType*(scope: var Scope, span: Span, typename: string, argtypes: seq[TypeSymbol]): Option[TypeSymbol] =
  if typename == "Varargs":
    let semexpr = newSemanticExpr(
      span,
      semanticVarargsType,
      argtypes[0],
      varargstype: VarargsType(
        typ: argtypes[0]
      )
    )
    let sym = newSymbol(scope, typename, semexpr)
    let typesym = getTypeSymbol(sym)
    semexpr.typesym = typesym
    return some(typesym)
  elif typename == "Typedesc":
    let semexpr = newSemanticExpr(
      span,
      semanticTypedesc,
      argtypes[0],
      typedesctype: TypedescType(typ: argtypes[0])
    )
    let sym = newSymbol(scope, typename, semexpr)
    let typesym = getTypeSymbol(sym)
    semexpr.typesym = typesym
    return some(typesym)
  elif typename == "Ref":
    let semexpr = newSemanticExpr(
      span,
      semanticReftype,
      argtypes[0],
      reftype: Reftype(typ: argtypes[0])
    )
    let sym = newSymbol(scope, typename, semexpr)
    let typesym = getTypeSymbol(sym)
    semexpr.typesym = typesym
    return some(typesym)
  else:
    let typesymopt = scope.trySymbol(scope.newSemanticIdent(span, typename, argtypes))
    if typesymopt.isNone:
      return none(TypeSymbol)
    let typesym = getTypeSymbol(typesymopt.get)
    if typesymopt.get.semexpr.kind == semanticPrimitiveType and typesymopt.get.semexpr.primtype.isGenerics:
      return some(genSpecGenericsPrimitiveType(scope, typesym, argtypes))
    elif typesymopt.get.semexpr.kind == semanticStruct and typesymopt.get.semexpr.struct.isGenerics:
      return some(genSpecGenericsStruct(scope, typesym, argtypes))
    elif typesymopt.get.semexpr.kind == semanticPrimitiveFunc and typesymopt.get.semexpr.primfunc.isGenerics:
      return some(genSpecGenericsPrimitiveFunc(scope, typesym, argtypes))
    elif typesymopt.get.semexpr.kind == semanticFunction and typesymopt.get.semexpr.function.isGenerics:
      return some(genSpecGenericsFunc(scope, typesym, argtypes))
    elif typesymopt.get.semexpr.kind == semanticGenerics:
      let semexpr = newSemanticExpr(typesymopt.get.semexpr.span, semanticGenericsChild, typesymopt.get.semexpr.typesym, parent: typesymopt.get)
      typesymopt.get.semexpr.generics.children.add(semexpr)
      let sym = scope.newSymbol(typesymopt.get.name, semexpr)
      return some(getTypeSymbol(sym))
    else:
      return some(typesym)
    # else:
    #   let newtypesym = TypeSymbol(kind: typesymTypeGenerics, genericsoriginsym: typesymopt.get, genericstypes: argtypes)
    #   return some(newtypesym)

proc tryType*(scope: var Scope, sexpr: SExpr): Option[TypeSymbol] =
  if sexpr.kind == sexprList:
    var argtypes = newSeq[TypeSymbol]()
    for e in sexpr.rest:
      let opt = scope.tryType(e)
      if opt.isNone:
        return none(TypeSymbol)
      argtypes.add(opt.get)
    return scope.tryType(sexpr.span, $sexpr.first, argtypes)
  else:
    return scope.tryType(sexpr.span, $sexpr, @[])

proc evalCallableStruct*(scope: var Scope, sexpr: SExpr, sym: SemanticExpr, typesemexpr: SemanticExpr): SemanticExpr =
  if ($sexpr.rest.first)[0] != '.':
    sexpr.rest.first.span.raiseError("$# is not field access syntax" % $sexpr.rest.first)
  let fieldname = ($sexpr.rest.first)[1..^1]
  
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

proc evalFuncCall*(scope: var Scope, span: Span, funcname: string, args: seq[SemanticExpr], argtypes: seq[TypeSymbol]): SemanticExpr =
  let tryfuncsym = scope.tryType(span, funcname, argtypes)
  if tryfuncsym.isNone:
    span.raiseError("undeclared function call: ($# $#)" % [funcname, argtypes.mapIt(it.debug).join(" ")])
  let typesym = tryfuncsym.get
  let semexpr = newSemanticExpr(
    span,
    semanticFuncCall,
    typesym.getSemexpr().typesym,
    funccall: FuncCall(
      callfunc: tryfuncsym.get,
      args: args,
    )
  )
  return semexpr

proc evalFuncCall*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  var args = newSeq[SemanticExpr]()
  for arg in sexpr.rest:
    args.add(scope.evalSExpr(arg))
  let argtypes = args.mapIt(it.typesym)
  return scope.evalFuncCall(sexpr.span, $sexpr.first, args, argtypes)

proc evalCall*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let trysym = scope.trySymbol(scope.newSemanticIdent(sexpr.first))
  if trysym.isSome:
    let semexpr = trysym.get.semexpr
    if semexpr.kind == semanticPrimitiveEval: # primitive eval
      let retsemexpr = semexpr.evalproc(scope, sexpr)
      if retsemexpr.kind == semanticSExpr:
        return scope.evalSExpr(retsemexpr.sexpr)
      else:
        return retsemexpr
    elif semexpr.kind == semanticFunction or semexpr.kind == semanticStruct:
      return scope.evalFuncCall(sexpr)
    else:
      let typesemexpr = semexpr.typesym.getSemexpr()
      if typesemexpr.kind == semanticStruct: # callable struct
        return scope.evalCallableStruct(sexpr, newSemanticExpr(trysym.get), typesemexpr)
      elif typesemexpr.kind == semanticReftype and typesemexpr.reftype.typ.getSemExpr().kind == semanticStruct:
        return scope.evalCallableStruct(sexpr, newSemanticExpr(trysym.get), typesemexpr.reftype.typ.getSemExpr())
      elif typesemexpr.kind == semanticTypeGenerics and typesemexpr.typegenerics.typ.getSemExpr().kind == semanticStruct:
        return scope.evalCallableStruct(sexpr, newSemanticExpr(trysym.get), typesemexpr.typegenerics.typ.getSemExpr())
      else: # normal call
        return scope.evalFuncCall(sexpr)
  else:
    return scope.evalFuncCall(sexpr)

#
# Eval
#

proc evalIdent*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let opt = scope.trySymbol(scope.newSemanticIdent(sexpr))
  if opt.isNone:
    sexpr.span.raiseError("undeclared ident: $#" % $sexpr)
  var semexpr = newSemanticExpr(sexpr.span, semanticSymbol, opt.get.semexpr.typesym, symbol: opt.get)
  return semexpr

proc evalAttr*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let semid = scope.newSemanticIdent(sexpr)
  let opt = scope.trySymbol(semid)
  if opt.isSome:
    if opt.get.semexpr.kind == semanticGenerics:
      let semexpr = newSemanticExpr(sexpr.span, semanticGenericsChild, getTypeSymbol(opt.get), parent: opt.get)
      opt.get.semexpr.generics.children.add(semexpr)
      return semexpr
    else:
      return opt.get.semexpr
  else:
    sexpr.span.raiseError("couldn't find $# attr" % $sexpr)

proc evalInt*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  result = newSemanticExpr(
    sexpr.span, 
    semanticInt, 
    scope.tryType(sexpr.span, "Int32", @[]).get, 
    intval: sexpr.intval
  )
proc evalString*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  result = newSemanticExpr(
    sexpr.span, 
    semanticString, 
    scope.tryType(sexpr.span, "CString", @[]).get,
    strval: sexpr.strval
  )

proc evalSExpr*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  # echo sexpr
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
