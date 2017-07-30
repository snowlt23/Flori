
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
    semanticSExpr
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
    of semanticSExpr:
      sexpr*: SExpr
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
    of semanticPrimitiveValue:
      primValue*: string
    of semanticPrimitiveFunc:
      primFuncKind*: PrimitiveFuncKind
      primFuncName*: string
      primFuncRetType*: Symbol
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
proc addSymbol*(scope: var Scope, semid: SemanticIdent, sym: Symbol)
proc getSymbol*(scope: var Scope, semid: SemanticIdent): Symbol

#
# Consts
#

let globalModule* = newModule("global")
let globalScope* = newScope(globalModule)
let notTypeSym* = Symbol(scope: globalScope, name: "not_type_symbol")
let notTypeSemExpr* = SemanticExpr(typesym: notTypeSym, kind: semanticSymbol, symbol: notTypeSym)

#
# Error
#

proc raiseError*(span: Span, s: string) =
  if span.line == 0 and span.linepos == 0:
    let msg = "(unknown) " & s
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
  semexpr.sexpr.span.raiseError(s)
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
      args.add(scope.getSymbol(scope.newSemanticIdent(sexpr)))
    return newSemanticIdent(scope, sexpr.span, $sexpr.first, args.getSemanticTypeArgs())
  elif sexpr.kind == sexprIdent:
    return newSemanticIdent(scope, sexpr.first.span, $sexpr, @[])
  else:
    sexpr.span.raiseError("can't use $# instead ident" % $sexpr.kind)
proc newSemanticIdent*(symbol: Symbol): SemanticIdent =
  newSemanticIdent(symbol.scope, symbol.semexpr.span, symbol.name, @[])
proc newSemanticIdentGroup*(): SemanticIdentGroup =
  result.idsymbols = @[]
proc addSymbol*(symgroup: var SemanticIdentGroup, symbol: Symbol) =
  symgroup.idsymbols.add((newSemanticIdent(symbol), symbol))
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
proc `$`*(symbolargs: seq[SemanticTypeArg]): string =
  if symbolargs.len == 0:
    return ""
  else:
    return "(" & symbolargs.mapIt($it).join(", ") & ")"

#
# Semantic Symbol Table
#

proc match*(semid, gsemid: SemanticIdent): bool =
  for i in 0..<gsemid.args.len:
    let arg = semid.args[i]
    let garg = gsemid.args[i]
    if arg.kind == semtypeName and garg.kind == semtypeName:
      if arg.namesym == garg.namesym:
        return true
    elif garg.kind == semtypeGenerics:
      return true
  return false
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
proc addSymbol*(semids: var OrderedTable[SemanticIdent, SemanticIdentGroup], semid: SemanticIdent, symbol: Symbol) =
  if semids.hasSemId(semid):
    symbol.raiseError("can't redefine symbol: $#" % [semid.name])
  let semid = newSemanticIdent(symbol)
  if not semids.hasKey(semid):
    semids[semid] = newSemanticIdentGroup()
  semids[semid].addSymbol(symbol)
proc getSymbol*(semids: var OrderedTable[SemanticIdent, SemanticIdentGroup], semid: SemanticIdent): Symbol =
  if not semids.hasSemId(semid):
    semid.raiseError("undeclared variable: $#" % $semid)
  

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

macro newSemanticExpr*(span: Span, kind: SemanticExprKind, body: varargs[untyped]): SemanticExpr =
  let tmpid= genSym(nskVar, "tmp")
  result = quote do:
    var `tmpid` = SemanticExpr(kind: `kind`, span: `span`, typesym: notTypeSym, metadata: newMetadata())
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
    semexpr.raiseError("expression is not $#" % $kind)

#
# Module
#

proc addSymbol*(module: Module, semid: SemanticIdent, sym: Symbol) =
  module.semanticidents.addSymbol(semid, sym)
proc addToplevelCall*(module: Module, sexpr: SExpr, funccall: FuncCall) =
  module.toplevelcalls.add(newSemanticExpr(sexpr.span, semanticFuncCall, funccall: funccall))
proc addCffi*(module: Module, cffi: Cffi) =
  module.ccodegeninfo.cffis.add(cffi)
proc addDecl*(module: Module, decl: string) =
  module.ccodegeninfo.decls.add(decl)

proc newSemanticContext*(): SemanticContext =
  new result
  result.modules = initOrderedTable[string, Module]()

#
# CCodegenInfo
#

proc newCCodegenInfo*(): CCodegenInfo =
  result.headers = initOrderedTable[string, bool]()
  result.decls = @[]
proc addHeader*(info: var CCodegenInfo, name: string) =
  info.headers[name] = true

#
# Scope
#

proc newScope*(module: Module): Scope =
  result.module = module
  result.semanticidents = initOrderedTable[SemanticIdent, SemanticIdentGroup]()
proc hasSemId*(scope: Scope, semid: SemanticIdent): bool =
  scope.semanticidents.hasSemId(semid) or scope.module.semanticidents.hasSemId(semid)
proc addSymbol*(scope: var Scope, semid: SemanticIdent, sym: Symbol) =
  scope.semanticidents.addSymbol(semid, sym)
proc getSymbol*(scope: var Scope, semid: SemanticIdent): Symbol =
  if scope.hasSemId(semid):
    return scope.semanticidents.getSymbol(semid)
  else:
    semid.raiseError("couldn't find symbol: $#" % $semid)

#
# define primitives
#

# FIXME: defPrimitiveType
proc defPrimitiveType*(scope: var Scope, span: Span, generics: seq[string], typename: string, primname: string) =
  let semexpr = newSemanticExpr(span, semanticPrimitiveType, primTypeGenerics: generics.mapIt(scope.getSymbol(it)), primTypeName: primname)
  let sym = newSymbol(scope, typename, semexpr)
  scope.module.addSymbol(sym)
proc defPrimitiveValue*(scope: var Scope, span: Span, typename: string, valuename: string, value: string) =
  var semexpr =newSemanticExpr(span, semanticPrimitiveValue, primValue: value)
  semexpr.typesym = scope.getSymbol(typename)
  let sym = newSymbol(scope, valuename, semexpr)
  scope.module.addSymbol(sym)
proc defPrimitiveFunc*(scope: var Scope, span: Span, funcname: string, rettype: Symbol, kind: PrimitiveFuncKind, primname: string) =
  let semexpr = newSemanticExpr(span, semanticPrimitiveFunc, primFuncKind: kind, primFuncName: primname, primfuncRetType: rettype)
  let sym = newSymbol(scope, funcname, semexpr)
  scope.module.addSymbol(semexpr)
proc defPrimitiveEval*(scope: var Scope, span: Span, macroname: string, evalproc: proc (scope: var Scope, sexpr: SExpr): SemanticExpr) =
  scope.module.addSymbol(
    newSymbol(newSNil(span), scope, macroname),
    newSemanticExpr(span, semanticPrimitiveEval, evalproc: evalproc)
  )

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
  let typesym = scope.getSymbol(newSemanticIdent(scope, sexpr.first))
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

proc newModule*(modulename: string): Module =
  new result
  result.name = modulename
  result.semanticidents = initOrderedTable[SemanticIdent, SemanticIdentGroup]()
  result.toplevelcalls = @[]
  result.exportedsymbols = @[]
  result.ccodegenInfo = newCCodegenInfo()

proc addArgSymbols*(scope: var Scope, argtypesyms: seq[Symbol], funcdef: SExpr) =
  if funcdef.rest.rest.first.len != argtypesyms.len:
    raise newException(SemanticError, "($#:$#) argument length is not equals type length" % [$funcdef.span.line, $funcdef.span.linepos])
  for i, arg in funcdef.rest.rest.first:
    var semexpr = newSemanticExpr(funcdef.span, semanticSymbol, symbol: newSymbol(arg, scope, $arg))
    semexpr.typesym = argtypesyms[i]
    scope.addSymbol(newSymbol(funcdef.rest.rest, scope, $arg), semexpr)

proc getType*(semexpr: SemanticExpr): Symbol =
  return semexpr.typesym
proc getType*(sym: Symbol): Symbol =
  let semexpr = getSemanticExpr(sym)
  if semexpr.kind == semanticGenerics:
    if semexpr.generics.spec.isNone:
      sym.raiseError("couldn't specialize generics param: $#" % sym.name)
    return semexpr.generics.spec.get
  else:
    return sym
proc getRetType*(scope: Scope, sym: Symbol): Symbol =
  let semexpr = getSemanticExpr(sym)
  if semexpr.kind == semanticFunction:
    return semexpr.function.fntype.returntype.getType()
  elif semexpr.kind == semanticPrimitiveFunc:
    return semexpr.primFuncRetType.getType()
  elif semexpr.kind == semanticProtocolFunc:
    return sym.getType() # TODO: getRetType
  else:
    sym.raiseError("expression is not function: $#" % $sym)

proc specGenericsFunc*(callsemexpr: SemanticExpr, funcsemexpr: SemanticExpr) =
  if funcsemexpr.kind != semanticFunction:
    return
  for i, argtype in funcsemexpr.function.fntype.argtypes:
    let argtypesemexpr = argtype.getSemanticExpr()
    if argtypesemexpr.kind == semanticGenerics:
      # TODO: on rewrite
      argtypesemexpr.generics.spec = some(getType(callsemexpr.funccall.args[i]))

proc genGenericsFunc*(scope: var Scope, callsemexpr: SemanticExpr, funcsemexpr: SemanticExpr) =
  if funcsemexpr.kind != semanticFunction:
    return
  specGenericsFunc(callsemexpr, funcsemexpr)
  let funcname = funcsemexpr.function.name
  let argtypes = funcsemexpr.function.fntype.argtypes.mapIt(getType(it))
  let rettype = funcsemexpr.function.fntype.returntype.getType()
  let sym = newSymbol(funcsemexpr.sexpr, scope, funcname, argtypes.mapIt(getSymbolArg(it)))
  if scope.hasEqualSymbol(sym):
    return
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
  scope.module.semanticexprs.addSymbol(sym, newSemanticExpr(funcsemexpr.sexpr.span, semanticFunction, function: f))

proc evalFuncCall*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let trysym = scope.trySymbol(sexpr, $sexpr.first)
  if trysym.isSome:
    let semexpr = trySemanticExpr(trysym.get)
    if semexpr.isSome and semexpr.get.kind == semanticPrimitiveEval:
      let retsemexpr = semexpr.get.evalproc(scope, sexpr)
      if retsemexpr.kind == semanticSExpr:
        return scope.evalSExpr(retsemexpr.sexpr)
      else:
        return retsemexpr
    elif semexpr.isSome:
      let typesemexpr = trySemanticExpr(semexpr.get.typesym)
      if typesemexpr.isSome and typesemexpr.get.kind == semanticStruct:
        let fieldname = $sexpr.rest.first
        let callsemexpr = newSemanticExpr(
          sexpr.span,
          semanticFieldAccess,
          fieldaccess: FieldAccess(
            valuesym: trysym.get,
            fieldname: fieldname,
          ),
        )
        var rettype = notTypeSym
        for field in typesemexpr.get.struct.fields:
          if field.name == fieldname:
            rettype = field.typesym
            break
        if rettype == notTypeSym:
          sexpr.rest.first.raiseError("undeclared field: $#" % fieldname)
        return callsemexpr

  var args = newSeq[SemanticExpr]()
  for arg in sexpr.rest:
    args.add(scope.evalSExpr(arg))
  let argtypes = args.mapIt(getType(it))

  let callfuncsym = newSymbol(sexpr, scope, $sexpr.first, argtypes.mapIt(getSymbolArg(it)))
  if not scope.hasSymbol(callfuncsym):
    sexpr.raiseError("couldn't find function symbol: $#($#)" % [$sexpr.first, argtypes.mapIt(it.name).join(", ")])
  let funcsemexpr = getSemanticExpr(callfuncsym)
  let callsemexpr = newSemanticExpr(
    sexpr.span,
    semanticFuncCall,
    funccall: FuncCall(
      callfunc: callfuncsym,
      args: args,
    ),
  )
  genGenericsFunc(scope, callsemexpr, funcsemexpr)
  callsemexpr.typesym = scope.getRetType(callfuncsym)
  return callsemexpr

proc evalInt*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  result = newSemanticExpr(sexpr.span, semanticInt, intval: sexpr.intval)
  result.typesym = scope.getSymbol(sexpr, "Int32")
proc evalString*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  result = newSemanticExpr(sexpr.span, semanticString, strval: sexpr.strval)
  result.typesym = scope.getSymbol(sexpr, "CString")

proc evalSExpr*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  case sexpr.kind
  of sexprNil:
    return notTypeSemExpr
  of sexprList:
    return scope.evalFuncCall(sexpr)
  of sexprIdent:
    let sym = scope.getSymbol(sexpr, $sexpr)
    var semexpr = newSemanticExpr(sexpr.span, semanticSymbol, symbol: sym)
    semexpr.typesym = getSemanticExpr(sym).typesym
    return semexpr
  of sexprInt:
    return scope.evalInt(sexpr)
  of sexprString:
    return scope.evalString(sexpr)
  else:
    sexpr.raiseError("couldnt't eval: $#" % $sexpr.kind)

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
