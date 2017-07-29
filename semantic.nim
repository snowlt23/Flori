
import tables, hashes
import strutils, sequtils
import sast
import options
import macros

type
  SemanticError* = object of Exception
  SymbolArgKind* = enum
    symbolargUntyped
    symbolargTyped
    symbolargGenerics
    symbolargName
  SymbolArg* = object
    case kind*: SymbolArgKind
    of symbolargName:
      namesym*: Symbol
    else:
      genericssym*: Symbol
  Symbol* = object
    sexpr*: SExpr
    scope*: Scope
    name*: string
    args*: seq[Symbolarg]
  SymbolGroup* = object
    name*: string
    symbols*: seq[tuple[sym: Symbol, value: SemanticExpr]]
  Metadata* = object
    metatable*: Table[string, SemanticExpr]
  PrimitiveFuncKind* = enum
    primitiveCall
    primitiveInfix
  FuncType* = object
    returntype*: Symbol
    argtypes*: seq[Symbol]
  SemanticExprKind* = enum
    semanticSymbol
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
    semanticPrimitiveMacro
    semanticPrimitiveEval
    semanticModule
    semanticFuncCall
    semanticInt
    semanticString
  SemanticExpr* = ref object
    sexpr*: SExpr
    metadata*: Metadata
    typesym*: Symbol
    case kind*: SemanticExprKind
    of semanticSymbol:
      symbol*: Symbol
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
    of semanticPrimitiveMacro:
      macroproc*: proc (scope: var Scope, sexpr: SExpr): SExpr
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
    semanticexprs*: OrderedTable[Symbol, SymbolGroup]
    toplevelcalls*: seq[SemanticExpr]
    exportedsymbols*: seq[Symbol]
    ccodegenInfo*: CCodegenInfo
  FuncCall* = ref object
    callfunc*: Symbol
    args*: seq[SemanticExpr]
  Scope* = object
    module*: Module
    scopesymbols*: OrderedTable[Symbol, SymbolGroup]
  SemanticContext* = ref object
    modules*: OrderedTable[string, Module]
    symcount*: int

proc newModule*(modulename: string): Module
proc evalSExpr*(scope: var Scope, sexpr: SExpr): SemanticExpr
proc addSymbol*(scope: var Scope, sym: Symbol, value: SemanticExpr)
proc getSymbol*(scope: Scope, sexpr: SExpr, name: string, spec: set[SemanticExprKind] = {semanticSymbol..semanticString}): Symbol
proc getSemanticExpr*(sym: Symbol, spec: set[SemanticExprKind] = {semanticSymbol..semanticString}): SemanticExpr
proc trySemanticExpr*(sym: Symbol): Option[SemanticExpr]
proc addArgSymbols*(scope: var Scope, argtypesyms: seq[Symbol], funcdef: SExpr)
proc addSymbol*(semanticexprs: var OrderedTable[Symbol, SymbolGroup], symbol: Symbol, value: SemanticExpr)
proc getSymbolArg*(sym: Symbol): SymbolArg
proc newScope*(module: Module): Scope

let globalModule* = newModule("global")
let globalScope* = newScope(globalModule)
let notTypeSym* = Symbol(scope: globalScope, name: "not_type_symbol")
let notTypeSemExpr* = SemanticExpr(typesym: notTypeSym, kind: semanticSymbol, symbol: notTypeSym)

proc raiseError*(sexpr: SExpr, s: string) =
  if sexpr.span.line == 0 and sexpr.span.linepos == 0:
    let msg = "(unknown) " & s
    when not defined(release):
      raise newException(SemanticError, msg)
    else:
      quit msg
  else:
    let msg = "($#:$#) " % [$sexpr.span.line, $sexpr.span.linepos] & s
    when not defined(release):
      raise newException(SemanticError, msg)
    else:
      quit msg
proc raiseError*(semexpr: SemanticExpr, s: string) =
  semexpr.sexpr.raiseError(s)
proc raiseError*(sym: Symbol, s: string) =
  sym.sexpr.raiseError(s)

proc newSymbolGroup*(name: string): SymbolGroup =
  result.name = name
  result.symbols = @[]

proc newSymbol*(sexpr: SExpr, scope: Scope, name: string, args = newSeq[SymbolArg]()): Symbol =
  Symbol(sexpr: sexpr, scope: scope, name: name, args: args)
proc globalSymbol*(name: string): Symbol =
  newSymbol(newSNil(), newScope(globalModule), name)
proc repr*(sa: SymbolArg): string =
  case sa.kind
  of symbolargName:
    $sa.namesym
  of symbolargGenerics:
    $sa.genericssym
# proc `$`*(sa: openArray[SymbolArg]): string =
#   if sa.len == 0:
#     return ""
#   else:
#     return 
proc `$`*(symbol: Symbol): string =
  symbol.scope.module.name & "_" & symbol.name & "(" & symbol.args.mapIt(it.repr).join(", ") & ")"
proc repr*(symbol: Symbol): string =
  symbol.scope.module.name.replace("_", ".") & "/" & symbol.name & "(" & $symbol.args.mapIt($it).join(", ") & ")"
proc `==`*(a: Symbol, b: Symbol): bool =
  a.scope.module.name == b.scope.module.name and a.name == b.name
proc hash*(sym: Symbol): Hash =
  hash(sym.scope.module.name & "_" & sym.name)

proc newMetadata*(): MetaData =
  result.metatable = initTable[string, SemanticExpr]()
proc addMetadata*(semexpr: SemanticExpr, name: string, meta: SemanticExpr) =
  semexpr.metadata.metatable[name] = meta
proc getMetadata*(semexpr: SemanticExpr, name: string): SemanticExpr =
  if not semexpr.metadata.metatable.hasKey(name):
    semexpr.raiseError("expression hasn't `$#` metadata" % name)
  semexpr.metadata.metatable[name]

macro newSemanticExpr*(sexpr: SExpr, kind: SemanticExprKind, body: varargs[untyped]): SemanticExpr =
  let tmpid= genSym(nskVar, "tmp")
  result = quote do:
    var `tmpid` = SemanticExpr(kind: `kind`, sexpr: `sexpr`, typesym: notTypeSym, metadata: newMetadata())
  for b in body:
    expectKind(b, nnkExprColonExpr)
    let name = b[0]
    let value = b[1]
    result.add(quote do:
      `tmpid`.`name` = `value`
    )
  result.add(tmpid)
proc expectSemantic*(semexpr: SemanticExpr, kind: SemanticExprKind) =
  if semexpr.kind != kind:
    semexpr.raiseError("expression is not $#" % $kind)

proc addSymbol*(module: Module, sym: Symbol, value: SemanticExpr) =
  module.semanticexprs.addSymbol(sym, value)
proc addToplevelCall*(module: Module, sexpr: SExpr, funccall: FuncCall) =
  module.toplevelcalls.add(newSemanticExpr(sexpr, semanticFuncCall, funccall: funccall))
proc addCffi*(module: Module, cffi: Cffi) =
  module.ccodegeninfo.cffis.add(cffi)
proc addDecl*(module: Module, decl: string) =
  module.ccodegeninfo.decls.add(decl)

proc newSemanticContext*(): SemanticContext =
  new result
  result.modules = initOrderedTable[string, Module]()

proc defPrimitiveType*(scope: var Scope, generics: seq[string], typename: string, primname: string) =
  scope.module.addSymbol(
    newSymbol(newSNil(), scope, typename),
    newSemanticExpr(newSNil(), semanticPrimitiveType, primTypeGenerics: generics.mapIt(scope.getSymbol(newSNil(), it)), primTypeName: primname)
  )
proc defPrimitiveValue*(scope: var Scope, typename: string, valuename: string, value: string) =
  var semexpr =newSemanticExpr(newSNil(), semanticPrimitiveValue, primValue: value)
  semexpr.typesym = scope.getSymbol(newSNil(), typename)
  scope.module.addSymbol(
    newSymbol(newSNil(), scope, valuename),
    semexpr
  )
proc defPrimitiveFunc*(scope: var Scope, funcname: string, rettype: Symbol, kind: PrimitiveFuncKind, primname: string) =
  scope.module.addSymbol(
    newSymbol(newSNil(), scope, funcname),
    newSemanticExpr(newSNil(), semanticPrimitiveFunc, primFuncKind: kind, primFuncName: primname, primfuncRetType: rettype)
  )
proc defPrimitiveMacro*(scope: var Scope, macroname: string, macroproc: proc (scope: var Scope, sexpr: SExpr): SExpr) =
  scope.module.addSymbol(
    newSymbol(newSNil(), scope, macroname),
    newSemanticExpr(newSNil(), semanticPrimitiveMacro, macroproc: macroproc)
  )
proc defPrimitiveEval*(scope: var Scope, macroname: string, evalproc: proc (scope: var Scope, sexpr: SExpr): SemanticExpr) =
  scope.module.addSymbol(
    newSymbol(newSNil(), scope, macroname),
    newSemanticExpr(newSNil(), semanticPrimitiveEval, evalproc: evalproc)
  )

proc newCCodegenInfo*(): CCodegenInfo =
  result.headers = initOrderedTable[string, bool]()
  result.decls = @[]
proc addHeader*(info: var CCodegenInfo, name: string) =
  info.headers[name] = true

proc parseTypeAnnotation*(sexpr: SExpr): tuple[argtypes: seq[SExpr], rettype: SExpr, body: SExpr] =
  var argtypes = newSeq[SExpr]()
  var rettype: SExpr
  let body = sexpr.last
  for arg in sexpr.rest.list:
    if $arg.first == "->":
      rettype = arg.rest.first
      break
    elif arg.rest.kind == sexprNil:
      break
    else:
      argtypes.add(arg.first)
  if rettype == nil:
    rettype = ast(sexpr.span, newSIdent("Void"))
  result = (argtypes, rettype, body)
proc getSpecType*(scope: var Scope, sexpr: SExpr): Symbol =
  let typesym = scope.getSymbol(sexpr.first, $sexpr.first)
  var specs = newSeq[Symbol]()
  for spec in sexpr.rest:
    specs.add(scope.getSymbol(spec, $spec))
  let sym = newSymbol(sexpr, scope, typesym.name, specs.mapIt(getSymbolArg(it)))
  scope.addSymbol(sym, newSemanticExpr(sexpr, semanticType, semtype: SemType(
    sym: typesym,
    specs: specs,
  )))
  return sym

proc getTypeAnnotation*(scope: var Scope, sexpr: SExpr): tuple[argtypes: seq[Symbol], rettype: Symbol, body: SExpr] =
  let (argtypes, rettype, body) = parseTypeAnnotation(sexpr)
  var argtypesyms = newSeq[Symbol]()
  for argtype in argtypes:
    if argtype.kind == sexprList:
      argtypesyms.add(scope.getSpecType(argtype))
    else:
      argtypesyms.add(scope.getSymbol(argtype, $argtype))
  let rettypesym = if rettype.kind == sexprList:
                     scope.getSpecType(rettype)
                   else:
                     scope.getSymbol(sexpr, $rettype)
  result = (argtypesyms, rettypesym, body)

proc newModule*(modulename: string): Module =
  new result
  result.name = modulename
  result.semanticexprs = initOrderedTable[Symbol, SymbolGroup]()
  result.toplevelcalls = @[]
  result.exportedsymbols = @[]
  result.ccodegenInfo = newCCodegenInfo()

proc addSemanticExpr*(symgroup: var SymbolGroup, symbol: Symbol, value: SemanticExpr) =
  symgroup.symbols.add((symbol, value))

proc matchSymbol*(sym, gsym: Symbol): bool =
  for i in 0..<gsym.args.len:
    let arg = sym.args[i]
    let garg = gsym.args[i]
    if arg.kind == symbolargName and garg.kind == symbolargName:
      if arg.namesym != garg.namesym:
        return false
    elif garg.kind == symbolargGenerics:
      continue
    else:
      return false
  return true
proc equalSymbol*(sym, gsym: Symbol): bool =
  for i in 0..<gsym.args.len:
    let arg = sym.args[i]
    let garg = gsym.args[i]
    if arg.kind == symbolargName and garg.kind == symbolargName:
      if arg.namesym != garg.namesym:
        return false
    else:
      return false
  return true
proc hasSymbol*(semanticexprs: OrderedTable[Symbol, SymbolGroup], symbol: Symbol, spec: set[SemanticExprKind] = {semanticSymbol..semanticString}): bool =
  if semanticexprs.hasKey(symbol):
    let symgroup = semanticexprs[symbol]
    for gsym in symgroup.symbols:
      if gsym.sym.args.len != symbol.args.len:
        continue
      elif matchSymbol(symbol, gsym.sym) and gsym.value.kind in spec:
        return true
    return false
  else:
    return false
proc hasEqualSymbol*(semanticexprs: OrderedTable[Symbol, SymbolGroup], symbol: Symbol): bool =
  if semanticexprs.hasKey(symbol):
    let symgroup = semanticexprs[symbol]
    for gsym in symgroup.symbols:
      if gsym.sym.args.len != symbol.args.len:
        continue
      elif equalSymbol(symbol, gsym.sym):
        return true
    return false
  else:
    return false
proc getSemanticExpr*(semanticexprs: OrderedTable[Symbol, SymbolGroup], symbol: Symbol, spec: set[SemanticExprKind] = {semanticSymbol..semanticString}): SemanticExpr =
  if semanticexprs.hasSymbol(symbol):
    let symgroup = semanticexprs[symbol]
    for gsym in symgroup.symbols:
      if gsym.sym.args.len != symbol.args.len:
        continue
      if matchSymbol(symbol, gsym.sym) and gsym.value.kind in spec:
        return gsym.value
  else:
    symbol.raiseError("couldn't find symbol: $#" % symbol.repr)
proc addSymbol*(semanticexprs: var OrderedTable[Symbol, SymbolGroup], symbol: Symbol, value: SemanticExpr) =
  # if semanticexprs.hasSymbol(symbol):
  #   symbol.raiseError("couldn't redefine symbol: $#" % [symbol.name])
  if not semanticexprs.hasKey(symbol):
    semanticexprs[symbol] = newSymbolGroup(symbol.name)
  semanticexprs[symbol].addSemanticExpr(symbol, value)

proc hasSymbol*(scope: Scope, sym: Symbol, spec: set[SemanticExprKind] = {semanticSymbol..semanticString}): bool =
  scope.scopesymbols.hasSymbol(sym, spec) or scope.module.semanticexprs.hasSymbol(sym, spec)
proc hasEqualSymbol*(scope: Scope, sym: Symbol): bool =
  scope.scopesymbols.hasEqualSymbol(sym) or scope.module.semanticexprs.hasEqualSymbol(sym)
proc newScope*(module: Module): Scope =
  result.module = module
  result.scopesymbols = initOrderedTable[Symbol, SymbolGroup]()
proc addSymbol*(scope: var Scope, sym: Symbol, value: SemanticExpr) =
  scope.scopesymbols.addSymbol(sym, value)
proc getSymbol*(scope: Scope, sexpr: SExpr, name: string, spec: set[SemanticExprKind] = {semanticSymbol..semanticString}): Symbol =
  let sym = newSymbol(sexpr, scope, name)
  let globalsym = newSymbol(sexpr, globalScope, name)
  if scope.hasSymbol(sym, spec):
    return sym
  elif scope.hasSymbol(globalsym, spec):
    return globalsym
  else:
    sexpr.raiseError("couldn't find symbol: $#" % name)
proc checkSymbol*(scope: Scope, sym: Symbol) =
  if scope.scopesymbols.hasSymbol(sym):
    discard
  elif scope.module.semanticexprs.hasSymbol(sym):
    discard
  elif scope.scopesymbols.hasSymbol(sym):
    discard
  elif scope.module.semanticexprs.hasSymbol(sym):
    discard
  else:
    sym.raiseError("couldn't find symbol: $#" % $sym)
proc getSemanticExpr*(sym: Symbol, spec: set[SemanticExprKind] = {semanticSymbol..semanticString}): SemanticExpr =
  if sym.scope.scopesymbols.hasSymbol(sym, spec):
    return sym.scope.scopesymbols.getSemanticExpr(sym, spec)
  else:
    return sym.scope.module.semanticexprs.getSemanticExpr(sym, spec)
proc getSymbolArg*(sym: Symbol): SymbolArg =
  let semexpr = getSemanticExpr(sym)
  if semexpr.kind == semanticGenerics:
    return SymbolArg(kind: symbolargGenerics, genericssym: sym)
  else:
    return SymbolArg(kind: symbolargName, namesym: sym)

proc trySymbol*(scope: Scope, sexpr: SExpr, name: string): Option[Symbol] =
  let sym = newSymbol(sexpr, scope, name)
  let globalsym = newSymbol(sexpr, globalScope, name)
  if scope.hasSymbol(sym):
    return some(sym)
  elif scope.hasSymbol(globalsym):
    return some(globalsym)
  elif scope.module.semanticexprs.hasSymbol(sym):
    return some(sym)
  elif scope.module.semanticexprs.hasSymbol(globalsym):
    return some(globalsym)
  else:
    return none(Symbol)
proc trySemanticExpr*(sym: Symbol): Option[SemanticExpr] =
  if sym.scope.scopesymbols.hasSymbol(sym):
    return some(sym.scope.scopesymbols.getSemanticExpr(sym))
  elif sym.scope.module.semanticexprs.hasSymbol(sym):
    return some(sym.scope.module.semanticexprs.getSemanticExpr(sym))
  else:
    return none(SemanticExpr)
proc addArgSymbols*(scope: var Scope, argtypesyms: seq[Symbol], funcdef: SExpr) =
  if funcdef.rest.rest.first.len != argtypesyms.len:
    raise newException(SemanticError, "($#:$#) argument length is not equals type length" % [$funcdef.span.line, $funcdef.span.linepos])
  for i, arg in funcdef.rest.rest.first:
    var semexpr = newSemanticExpr(funcdef, semanticSymbol, symbol: newSymbol(arg, scope, $arg))
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
  scope.module.semanticexprs.addSymbol(sym, newSemanticExpr(funcsemexpr.sexpr, semanticFunction, function: f))

proc evalFuncCall*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let trysym = scope.trySymbol(sexpr, $sexpr.first)
  if trysym.isSome:
    let semexpr = trySemanticExpr(trysym.get)
    if semexpr.isSome and semexpr.get.kind == semanticPrimitiveMacro:
      return scope.evalSExpr(semexpr.get.macroproc(scope, sexpr))
    elif semexpr.isSome and semexpr.get.kind == semanticPrimitiveEval:
      return semexpr.get.evalproc(scope, sexpr)
    elif semexpr.isSome:
      let typesemexpr = trySemanticExpr(semexpr.get.typesym)
      if typesemexpr.isSome and typesemexpr.get.kind == semanticStruct:
        let fieldname = $sexpr.rest.first
        let callsemexpr = newSemanticExpr(
          sexpr,
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
    sexpr,
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
  result = newSemanticExpr(sexpr, semanticInt, intval: sexpr.intval)
  result.typesym = scope.getSymbol(sexpr, "Int32")
proc evalString*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  result = newSemanticExpr(sexpr, semanticString, strval: sexpr.strval)
  result.typesym = scope.getSymbol(sexpr, "CString")

proc evalSExpr*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  case sexpr.kind
  of sexprNil:
    return notTypeSemExpr
  of sexprList:
    return scope.evalFuncCall(sexpr)
  of sexprIdent:
    let sym = scope.getSymbol(sexpr, $sexpr)
    var semexpr = newSemanticExpr(sexpr, semanticSymbol, symbol: sym)
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
