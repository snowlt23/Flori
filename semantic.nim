
import tables, hashes
import strutils, sequtils
import sast
import options
import macros

type
  SemanticError* = object of Exception

  Symbol* = object
    scope*: Scope
    name*: string
    semexpr*: SemanticExpr

  SemanticTypeArdKind* = enum
    semtypeGenerics
    semtypeName
  SemanticTypeArg* = object
    case kind*: SemanticTypeArdKind
    of semtypeName:
      namesym*: Symbol
    of semtypeGenerics:
      genericssym*: Symbol
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
    semanticType
    semanticProtocol
    semanticIfExpr
    semanticProtocolFunc
    semanticFunction
    semanticMacro
    semanticStruct
    semanticFieldAccess
    semanticPrimitiveType
    semanticPrimitiveValue
    semanticPrimitiveFunc
    semanticPrimitiveEval
    semanticModule
    semanticFuncCall
    semanticInt
    semanticString
  SemanticExpr* = ref object
    span*: Span
    metadata*: Metadata
    typesym*: Symbol
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
    of semanticType:
      semtype*: SemType
    of semanticProtocol:
      protocol*: Protocol
    of semanticIfExpr:
      ifexpr*: IfExpr
    of semanticProtocolFunc:
      protocolfntype*: FuncType
    of semanticFunction:
      function*: Function
    of semanticMacro: # TODO:
      discard
    of semanticStruct:
      struct*: Struct
    of semanticFieldAccess:
      fieldaccess*: FieldAccess
    of semanticPrimitiveType:
      primTypeGenerics*: seq[Symbol]
      primTypeName*: string
      primTypeIsGenerics*: bool
    of semanticPrimitiveValue:
      primValue*: string
    of semanticPrimitiveFunc:
      primfunc*: PrimitiveFunc
    of semanticPrimitiveEval:
      evalproc*: proc (scope: var Scope, sexpr: SExpr): SemanticExpr
    of semanticModule:
      module*: Module
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
  SemType* = ref object
    sym*: Symbol
    specs*: seq[Symbol]
  Protocol* = ref object
    isGenerics*: bool
    funcs*: seq[tuple[name: string, fntype: FuncType]]
  IfExpr* = ref object
    cond*: SemanticExpr
    tbody*: SemanticExpr
    fbody*: SemanticExpr
  Function* = ref object
    isGenerics*: bool
    name*: string
    argnames*: seq[string]
    fntype*: FuncType
    body*: seq[SemanticExpr]
  Struct* = ref object
    isGenerics*: bool
    name*: string
    fields*: seq[tuple[name: string, typesym: Symbol]]
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
  PrimitiveFunc* = ref object
    isGenerics*: bool
    kind*: PrimitiveFuncKind
    pattern*: bool
    name*: string
    fntype*: FuncType
  Module* = ref object
    context*: SemanticContext
    name*: string
    semanticidents*: OrderedTable[SemanticIdent, SemanticIdentGroup]
    toplevelcalls*: seq[SemanticExpr]
    exportedsymbols*: seq[Symbol]
    ccodegenInfo*: CCodegenInfo
  FuncCall* = ref object
    callfunc*: Symbol
    args*: seq[SemanticExpr]
  Scope* = object
    module*: Module
    semanticidents*: OrderedTable[SemanticIdent, SemanticIdentGroup]
  SemanticContext* = ref object
    modules*: OrderedTable[string, Module]
    symcount*: int

proc getSemanticTypeArgs*(syms: seq[Symbol]): seq[SemanticTypeArg]
proc newModule*(modulename: string): Module
proc newScope*(module: Module): Scope
proc newSymbol*(scope: Scope, name: string, semexpr: SemanticExpr): Symbol
proc addSymbol*(scope: var Scope, semid: SemanticIdent, sym: Symbol)
proc getSymbol*(scope: var Scope, semid: SemanticIdent): Symbol
proc evalSExpr*(scope: var Scope, sexpr: SExpr): SemanticExpr

#
# Consts
#

let globalModule* = newModule("global")
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
    let msg = "(internal:$#:$#) " % [span.internal.filename, $span.internal.line] & s
    when not defined(release):
      raise newException(SemanticError, msg)
    else:
      quit msg
  else:
    let msg = "($#:$#) " % [$span.line, $span.linepos] & s
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
  semid.name

#
# SemanticTypeArg
#

proc `$`*(semtypearg: SemanticTypeArg): string =
  case semtypearg.kind
  of semtypeName:
    semtypearg.namesym.name
  of semtypeGenerics:
    semtypearg.genericssym.name
proc debug*(semtypearg: SemanticTypeArg): string =
  case semtypearg.kind
  of semtypeName:
    $semtypearg.kind & ":" & semtypearg.namesym.name
  of semtypeGenerics:
    $semtypearg.kind & ":" & semtypearg.genericssym.name
proc `$`*(symbolargs: seq[SemanticTypeArg]): string =
  if symbolargs.len == 0:
    return ""
  else:
    return "(" & symbolargs.mapIt($it).join(", ") & ")"
proc debug*(symbolargs: seq[SemanticTypeArg]): string =
  if symbolargs.len == 0:
    return ""
  else:
    return "(" & symbolargs.mapIt(it.debug).join(", ") & ")"

#
# Semantic Symbol Table
#

proc match*(semid, gsemid: SemanticIdent): bool =
  for i in 0..<gsemid.args.len:
    let arg = semid.args[i]
    let garg = gsemid.args[i]
    if arg.kind == semtypeName and garg.kind == semtypeName:
      if arg.namesym != garg.namesym:
        return false
    elif garg.kind == semtypeGenerics:
      continue
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
  return true
proc hasSemId*(semids: OrderedTable[SemanticIdent, SemanticIdentGroup], semid: SemanticIdent): bool =
  if semids.hasKey(semid):
    let symgroup = semids[semid]
    for gsemidpair in symgroup.idsymbols:
      if gsemidpair.semid.args.len != semid.args.len:
        continue
      elif match(semid, gsemidpair.semid):
        return true
    return false
  else:
    return false
proc hasSpecSemId*(semids: OrderedTable[SemanticIdent, SemanticIdentGroup], semid: SemanticIdent): bool =
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
proc addSymbol*(semids: var OrderedTable[SemanticIdent, SemanticIdentGroup], semid: SemanticIdent, symbol: Symbol) =
  if semids.hasSpecSemId(semid):
    symbol.raiseError("can't redefine symbol: $#" % [semid.name])
  if not semids.hasKey(semid):
    semids[semid] = newSemanticIdentGroup()
  semids[semid].addSymbol(semid, symbol)
proc getSymbol*(semids: var OrderedTable[SemanticIdent, SemanticIdentGroup], semid: SemanticIdent): Symbol =
  if semids.hasKey(semid):
    let symgroup = semids[semid]
    for gsemidpair in symgroup.idsymbols:
      if gsemidpair.semid.args.len != semid.args.len:
        continue
      elif match(semid, gsemidpair.semid):
        return gsemidpair.value
  semid.raiseError("undeclared ident: $#" % $semid)
proc getSpecSymbol*(semids: var OrderedTable[SemanticIdent, SemanticIdentGroup], semid: SemanticIdent): Symbol =
  if semids.hasKey(semid):
    let symgroup = semids[semid]
    for gsemidpair in symgroup.idsymbols:
      if gsemidpair.semid.args.len != semid.args.len:
        continue
      elif equal(semid, gsemidpair.semid):
        return gsemidpair.value
  semid.raiseError("undeclared ident: $#" % $semid)
proc trySymbol*(semids: var OrderedTable[SemanticIdent, SemanticIdentGroup], semid: SemanticIdent): Option[Symbol] =
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
# Symbol
#

proc newSymbol*(scope: Scope, name: string, semexpr: SemanticExpr): Symbol =
  Symbol(scope: scope, name: name, semexpr: semexpr)
proc globalSymbol*(name: string, semexpr: SemanticExpr): Symbol =
  newSymbol(globalScope, name, semexpr)
proc `$`*(symbol: Symbol): string =
  symbol.scope.module.name & "_" & symbol.name
proc `==`*(a: Symbol, b: Symbol): bool =
  a.scope.module.name == b.scope.module.name and a.name == b.name

#
# SemanticTypeArg from Symbol
#

proc getSemanticTypeArg*(sym: Symbol): SemanticTypeArg =
  if sym.semexpr.kind == semanticGenerics:
    return SemanticTypeArg(kind: semtypeGenerics, genericssym: sym)
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
    var `tmpid` = SemanticExpr(kind: `kind`, span: `span`, typesym: `typesym`, metadata: newMetadata())
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

proc newModule*(modulename: string): Module =
  new result
  result.name = modulename
  result.semanticidents = initOrderedTable[SemanticIdent, SemanticIdentGroup]()
  result.toplevelcalls = @[]
  result.exportedsymbols = @[]
  result.ccodegenInfo = newCCodegenInfo()

proc addSymbol*(module: Module, semid: SemanticIdent, sym: Symbol) =
  module.semanticidents.addSymbol(semid, sym)
proc addToplevelCall*(module: Module, sexpr: SExpr, funccall: FuncCall) =
  module.toplevelcalls.add(newSemanticExpr(sexpr.span, semanticFuncCall, notTypeSym, funccall: funccall))
proc addCffi*(module: Module, cffi: Cffi) =
  module.ccodegeninfo.cffis.add(cffi)
proc addDecl*(module: Module, decl: string) =
  module.ccodegeninfo.decls.add(decl)

proc newSemanticContext*(): SemanticContext =
  new result
  result.modules = initOrderedTable[string, Module]()

#
# Scope
#

proc newScope*(module: Module): Scope =
  result.module = module
  result.semanticidents = initOrderedTable[SemanticIdent, SemanticIdentGroup]()
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
    primTypeGenerics: genericssyms,
    primTypeName: primname
  )
  let sym = newSymbol(scope, typename, semexpr)
  semexpr.typesym = sym
  let semid = scope.newSemanticIdent(span, typename, genericssyms.getSemanticTypeArgs)
  scope.module.semanticidents.addSymbol(semid, sym)
  return sym
proc defPrimitiveValue*(scope: var Scope, span: Span, typename: string, valuename: string, value: string) =
  var semexpr = newSemanticExpr(
    span,
    semanticPrimitiveValue,
    scope.getSymbol(scope.newSemanticIdent(span, typename, @[])),
    primValue: value
  )
  let sym = newSymbol(scope, valuename, semexpr)
  scope.module.semanticidents.addSymbol(newSemanticIdent(sym), sym)
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
  scope.module.semanticidents.addSymbol(semid, sym)
  return sym
proc defPrimitiveEval*(scope: var Scope, span: Span, macroname: string, evalproc: proc (scope: var Scope, sexpr: SExpr): SemanticExpr) =
  let semexpr = newSemanticExpr(span, semanticPrimitiveEval, notTypeSym, evalproc: evalproc)
  let sym = newSymbol(scope, macroname, semexpr)
  scope.module.semanticidents.addSymbol(newSemanticIdent(sym), sym)

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
proc getSpecType*(scope: var Scope, sexpr: SExpr): Symbol =
  let typesym = scope.getSymbol(newSemanticIdent(scope, sexpr))
  return typesym

proc getTypeAnnotation*(scope: var Scope, sexpr: SExpr): tuple[argtypes: seq[Symbol], rettype: Symbol, body: SExpr] =
  let (argtypes, rettype, body) = parseTypeAnnotation(sexpr)
  var argtypesyms = newSeq[Symbol]()
  for argtype in argtypes:
    if argtype.kind == sexprList:
      argtypesyms.add(scope.getSpecType(argtype))
    else:
      argtypesyms.add(scope.getSymbol(scope.newSemanticIdent(argtype)))
  let rettypesym = if rettype.kind == sexprList:
                     scope.getSpecType(rettype)
                   else:
                     scope.getSymbol(scope.newSemanticIdent(rettype))
  result = (argtypesyms, rettypesym, body)

proc addArgSymbols*(scope: var Scope, argtypesyms: seq[Symbol], funcdef: SExpr) =
  if funcdef.rest.rest.first.len != argtypesyms.len:
    raise newException(SemanticError, "($#:$#) argument length is not equals type length" % [$funcdef.span.line, $funcdef.span.linepos])
  for i, arg in funcdef.rest.rest.first:
    var semexpr = newSemanticExpr(funcdef.span, semanticIdent, argtypesyms[i], ident: $arg)
    let sym = newSymbol(scope, $arg, semexpr)
    scope.addSymbol(newSemanticIdent(sym), sym)

proc getType*(semexpr: SemanticExpr): Symbol =
  return semexpr.typesym
proc getType*(sym: Symbol): Symbol =
  if sym.semexpr.kind == semanticGenerics:
    if sym.semexpr.generics.spec.isNone:
      sym.raiseError("couldn't specialize generics param: $#" % sym.name)
    return sym.semexpr.generics.spec.get
  else:
    return sym
proc getRetType*(sym: Symbol): Symbol =
  if sym.semexpr.kind == semanticFunction:
    return sym.semexpr.function.fntype.returntype.getType()
  elif sym.semexpr.kind == semanticPrimitiveFunc:
    return sym.semexpr.primfunc.fntype.returntype.getType()
  elif sym.semexpr.kind == semanticProtocolFunc:
    return sym.getType()
  elif sym.semexpr.kind == semanticPrimitiveType:
    return sym
  else:
    sym.raiseError("symbol is not function: $#" % $sym)

#
# Generics
#

proc specializeGenerics*(genericssym: Symbol, typesym: Symbol) =
  genericssym.semexpr.expectSemantic(semanticGenerics)
  genericssym.semexpr.generics.spec = some(typesym)

proc specializeGenericsPrimitiveType*(typesyms: seq[Symbol], primtypesemexpr: SemanticExpr) =
  primtypesemexpr.expectSemantic(semanticPrimitiveType)
  for i, argtype in primtypesemexpr.primTypeGenerics:
    if argtype.semexpr.kind == semanticGenerics:
      specializeGenerics(argtype, typesyms[i])

proc specializeGenericsPrimitiveFunc*(typesyms: seq[Symbol], primfuncsemexpr: SemanticExpr) =
  primfuncsemexpr.expectSemantic(semanticPrimitiveFunc)
  for i, argtype in primfuncsemexpr.primfunc.fntype.argtypes:
    if argtype.semexpr.kind == semanticGenerics:
      specializeGenerics(argtype, typesyms[i])

proc specializeGenericsFunc*(typesyms: seq[Symbol], funcsemexpr: SemanticExpr) =
  funcsemexpr.expectSemantic(semanticFunction)
  for i, argtype in funcsemexpr.function.fntype.argtypes:
    if argtype.semexpr.kind == semanticGenerics:
      specializeGenerics(argtype, typesyms[i])

proc genGenericsFunc*(scope: var Scope, typesyms: seq[Symbol], funcsemexpr: SemanticExpr): Symbol =
  funcsemexpr.expectSemantic(semanticFunction)

  specializeGenericsFunc(typesyms, funcsemexpr)

  let funcname = funcsemexpr.function.name
  let argtypes = funcsemexpr.function.fntype.argtypes.mapIt(getType(it))
  let rettype = funcsemexpr.function.fntype.returntype.getType()
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

proc genGenericsPrimitiveType*(scope: var Scope, typesyms: seq[Symbol], typesemexpr: SemanticExpr): Symbol =
  typesemexpr.expectSemantic(semanticPrimitiveType)

  specializeGenericsPrimitiveType(typesyms, typesemexpr)
  let argtypes = typesemexpr.primTypeGenerics.mapIt(it.getType)
  let semexpr = newSemanticExpr(
    typesemexpr.span,
    semanticPrimitiveType,
    notTypeSym,
    primTypeGenerics: argtypes,
    primTypeName: typesemexpr.primTypeName
  )
  let sym = newSymbol(scope, typesemexpr.primTypeName, semexpr)
  let semid = scope.newSemanticIdent(typesemexpr.span, typesemexpr.primTypeName, argtypes.getSemanticTypeArgs)
  if scope.hasSpecSemId(semid):
    return sym
  scope.module.addSymbol(semid, sym)
  return sym

proc evalFuncCall*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let trysym = scope.trySymbol(scope.newSemanticIdent(sexpr.first))
  if trysym.isSome:
    if trysym.get.semexpr.kind == semanticPrimitiveEval:
      let retsemexpr = trysym.get.semexpr.evalproc(scope, sexpr)
      if retsemexpr.kind == semanticSExpr:
        return scope.evalSExpr(retsemexpr.sexpr)
      else:
        return retsemexpr
    else:
      let typesemexpr = trysym.get.semexpr.typesym.semexpr
      if typesemexpr.kind == semanticStruct: # callable struct
        let fieldname = $sexpr.rest.first
        var callsemexpr = newSemanticExpr(
          sexpr.span,
          semanticFieldAccess,
          notTypeSym,
          fieldaccess: FieldAccess(
            valuesym: trysym.get,
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

  var args = newSeq[SemanticExpr]()
  for arg in sexpr.rest:
    args.add(scope.evalSExpr(arg))
  let argtypes = args.mapIt(getType(it))
  let callfuncsemid = scope.newSemanticIdent(sexpr.first.span, $sexpr.first, argtypes.getSemanticTypeArgs)
  let callfuncsym = scope.getSymbol(callfuncsemid)

  if callfuncsym.semexpr.kind == semanticPrimitiveFunc:
    specializeGenericsPrimitiveFunc(argtypes, callfuncsym.semexpr)
  let finalcallfuncsym = if callfuncsym.semexpr.kind == semanticFunction and callfuncsym.semexpr.function.isGenerics:
                           genGenericsFunc(scope, argtypes, callfuncsym.semexpr)
                         elif callfuncsym.semexpr.kind == semanticPrimitiveType and callfuncsym.semexpr.primTypeIsGenerics:
                           genGenericsPrimitiveType(scope, argtypes, callfuncsym.semexpr)
                         else:
                           callfuncsym

  let semexpr = newSemanticExpr(
    sexpr.span,
    semanticFuncCall,
    getRetType(finalcallfuncsym),
    funccall: FuncCall(
      callfunc: finalcallfuncsym,
      args: args,
    ),
  )
  return semexpr

proc evalIdent*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let sym = scope.getSymbol(scope.newSemanticIdent(sexpr))
  var semexpr = newSemanticExpr(sexpr.span, semanticSymbol, sym.semexpr.typesym, symbol: sym)
  return semexpr

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
    return scope.evalFuncCall(sexpr)
  of sexprIdent:
    return scope.evalIdent(sexpr)
  of sexprInt:
    return scope.evalInt(sexpr)
  of sexprString:
    return scope.evalString(sexpr)
  else:
    sexpr.span.raiseError("couldnt't eval: $#" % $sexpr.kind)

include semantic_predefines

proc evalModule*(context: SemanticContext, modulename: string, sexpr: seq[SExpr]) =
  let modulename = modulename.replace("/", "_").replace("\\", "_")
  var module = newModule(modulename)
  var scope = newScope(module)
  scope.predefine()
  context.modules[modulename] = module
  for e in sexpr:
    let semexpr = scope.evalSExpr(e)
    if semexpr.kind == semanticSymbol:
      discard
    else:
      module.toplevelcalls.add(semexpr)
