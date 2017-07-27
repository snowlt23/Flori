
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
      sym*: Symbol
    else:
      discard
  Symbol* = object
    sexpr*: SExpr
    module*: Module
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
  SemanticExprKind* = enum
    semanticSymbol
    semanticVariable
    semanticGenerics
    semanticGenericsSpec
    semanticIfExpr
    semanticFunction
    semanticMacro
    semanticStruct
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
    of semanticGenericsSpec:
      genericssym*: Symbol
      genericsparams*: seq[Symbol]
    of semanticIfExpr:
      ifexpr*: IfExpr
    of semanticFunction:
      function*: Function
    of semanticMacro: # TODO:
      discard
    of semanticStruct:
      struct*: Struct
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
  IfExpr* = ref object
    cond*: SemanticExpr
    tbody*: SemanticExpr
    fbody*: SemanticExpr
  Function* = ref object
    name*: string
    argnames*: seq[string]
    argtypes*: seq[Symbol]
    rettype*: Symbol
    body*: seq[SemanticExpr]
  Struct* = ref object
    name*: string
    fields*: seq[tuple[name: string, typesym: Symbol]]
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
proc getSymbol*(scope: Scope, sexpr: SExpr, name: string): Symbol
proc getSemanticExpr*(scope: Scope, sym: Symbol): SemanticExpr
proc trySemanticExpr*(scope: Scope, sym: Symbol): Option[SemanticExpr]
proc addArgSymbols*(scope: var Scope, argtypesyms: seq[Symbol], funcdef: SExpr)
proc addSymbol*(semanticexprs: var OrderedTable[Symbol, SymbolGroup], symbol: Symbol, value: SemanticExpr)

let globalModule* = newModule("global")
let notTypeSym* = Symbol(module: globalModule, name: "not_type_symbol")
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

proc newSymbol*(sexpr: SExpr, module: Module, name: string, args = newSeq[SymbolArg]()): Symbol =
  Symbol(sexpr: sexpr, module: module, name: name, args: args)
proc globalSymbol*(name: string): Symbol =
  newSymbol(newSNil(), globalModule, name)
proc `$`*(symbol: Symbol): string =
  symbol.module.name & "_" & symbol.name
proc `==`*(a: Symbol, b: Symbol): bool =
  a.module.name == b.module.name and a.name == b.name
proc hash*(sym: Symbol): Hash =
  hash(sym.module.name & "_" & sym.name)

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
    newSymbol(newSNil(), globalModule, typename),
    newSemanticExpr(newSNil(), semanticPrimitiveType, primTypeGenerics: generics.mapIt(getSymbol(scope, newSNil(), it)), primTypeName: primname)
  )
proc defPrimitiveValue*(scope: var Scope, typename: string, valuename: string, value: string) =
  var semexpr =newSemanticExpr(newSNil(), semanticPrimitiveValue, primValue: value)
  semexpr.typesym = scope.getSymbol(newSNil(), typename)
  scope.module.addSymbol(
    newSymbol(newSNil(), scope.module, valuename),
    semexpr
  )
proc defPrimitiveFunc*(scope: var Scope, funcname: string, rettype: Symbol, kind: PrimitiveFuncKind, primname: string) =
  scope.module.addSymbol(
    newSymbol(newSNil(), globalModule, funcname),
    newSemanticExpr(newSNil(), semanticPrimitiveFunc, primFuncKind: kind, primFuncName: primname, primfuncRetType: rettype)
  )
proc defPrimitiveMacro*(scope: var Scope, macroname: string, macroproc: proc (scope: var Scope, sexpr: SExpr): SExpr) =
  scope.module.addSymbol(
    newSymbol(newSNil(), globalModule, macroname),
    newSemanticExpr(newSNil(), semanticPrimitiveMacro, macroproc: macroproc)
  )
proc defPrimitiveEval*(scope: var Scope, macroname: string, evalproc: proc (scope: var Scope, sexpr: SExpr): SemanticExpr) =
  scope.module.addSymbol(
    newSymbol(newSNil(), globalModule, macroname),
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
proc evalGenericsSpec*(scope: var Scope, sexpr: SExpr): Symbol =
  let genericsname = sexpr.first
  let genericssym = scope.getSymbol(sexpr, $genericsname)
  var genericsparams = newSeq[Symbol]()
  for generics in sexpr.rest:
    genericsparams.add(scope.getSymbol(sexpr, $generics))
  let sym = newSymbol(sexpr, scope.module, $genericsname & "_" & genericsparams.mapIt($it).join("_"))
  scope.addSymbol(sym, newSemanticExpr(sexpr, semanticGenericsSpec, genericssym: genericssym, genericsparams: genericsparams))
  return sym
proc getTypeAnnotation*(scope: var Scope, sexpr: SExpr): tuple[argtypes: seq[Symbol], rettype: Symbol, body: SExpr] =
  let (argtypes, rettype, body) = parseTypeAnnotation(sexpr)
  var argtypesyms = newSeq[Symbol]()
  for argtype in argtypes:
    if argtype.kind == sexprList:
      argtypesyms.add(evalGenericsSpec(scope, argtype))
    else:
      argtypesyms.add(scope.getSymbol(sexpr, $argtype))
  let rettypesym = if rettype.kind == sexprList:
                     evalGenericsSpec(scope, rettype)
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
    case garg.kind
    of symbolargName:
      if arg.sym != garg.sym:
        return false
    of symbolargGenerics:
      continue
    else:
      sym.raiseError("$# is not supported in currently" % $garg.kind)
  return true
proc hasSymbol*(semanticexprs: OrderedTable[Symbol, SymbolGroup], symbol: Symbol): bool =
  if semanticexprs.hasKey(symbol):
    let symgroup = semanticexprs[symbol]
    for gsym in symgroup.symbols:
      if gsym.sym.args.len != symbol.args.len:
        continue
      elif matchSymbol(symbol, gsym.sym):
        return true
    return false
  else:
    return false
proc getSemanticExpr*(semanticexprs: OrderedTable[Symbol, SymbolGroup], symbol: Symbol): SemanticExpr =
  if semanticexprs.hasSymbol(symbol):
    let symgroup = semanticexprs[symbol]
    for gsym in symgroup.symbols:
      if gsym.sym.args.len != symbol.args.len:
        continue
      if matchSymbol(symbol, gsym.sym):
        return gsym.value
  else:
    symbol.raiseError("couldn't find symbol: $#" % $symbol)
proc addSymbol*(semanticexprs: var OrderedTable[Symbol, SymbolGroup], symbol: Symbol, value: SemanticExpr) =
  if semanticexprs.hasSymbol(symbol):
    symbol.raiseError("couldn't redefine symbol: $#" % [symbol.name])
  if not semanticexprs.hasKey(symbol):
    semanticexprs[symbol] = newSymbolGroup(symbol.name)
  semanticexprs[symbol].addSemanticExpr(symbol, value)

proc hasSymbol*(scope: Scope, sym: Symbol): bool =
  scope.scopesymbols.hasSymbol(sym)
proc newScope*(module: Module): Scope =
  result.module = module
  result.scopesymbols = initOrderedTable[Symbol, SymbolGroup]()
proc addSymbol*(scope: var Scope, sym: Symbol, value: SemanticExpr) =
  scope.scopesymbols.addSymbol(sym, value)
proc getSymbol*(scope: Scope, sexpr: SExpr, name: string): Symbol =
  let sym = newSymbol(sexpr, scope.module, name)
  let globalsym = newSymbol(sexpr, globalModule, name)
  if scope.hasSymbol(sym):
    return sym
  elif scope.hasSymbol(globalsym):
    return globalsym
  elif scope.module.semanticexprs.hasSymbol(sym):
    return sym
  elif scope.module.semanticexprs.hasSymbol(globalsym):
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
proc getSemanticExpr*(scope: Scope, sym: Symbol): SemanticExpr =
  if scope.scopesymbols.hasSymbol(sym):
    return scope.scopesymbols.getSemanticExpr(sym)
  else:
    return scope.module.semanticexprs.getSemanticExpr(sym)
proc getSymbolArg*(scope: Scope, sym: Symbol): SymbolArg =
  let semexpr = scope.getSemanticExpr(sym)
  if semexpr.kind == semanticGenerics:
    return SymbolArg(kind: symbolargGenerics)
  else:
    return SymbolArg(kind: symbolargName, sym: sym)

proc trySymbol*(scope: Scope, sexpr: SExpr, name: string): Option[Symbol] =
  let sym = newSymbol(sexpr, scope.module, name)
  let globalsym = newSymbol(sexpr, globalModule, name)
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
proc trySemanticExpr*(scope: Scope, sym: Symbol): Option[SemanticExpr] =
  if scope.scopesymbols.hasSymbol(sym):
    return some(scope.scopesymbols.getSemanticExpr(sym))
  elif scope.module.semanticexprs.hasSymbol(sym):
    return some(scope.module.semanticexprs.getSemanticExpr(sym))
  else:
    return none(SemanticExpr)
proc addArgSymbols*(scope: var Scope, argtypesyms: seq[Symbol], funcdef: SExpr) =
  if funcdef.rest.rest.first.len != argtypesyms.len:
    raise newException(SemanticError, "($#:$#) argument length is not equals type length" % [$funcdef.span.line, $funcdef.span.linepos])
  for i, arg in funcdef.rest.rest.first:
    var semexpr = newSemanticExpr(funcdef, semanticSymbol, symbol: newSymbol(arg, scope.module, $arg))
    semexpr.typesym = argtypesyms[i]
    scope.addSymbol(newSymbol(funcdef.rest.rest, scope.module, $arg), semexpr)

proc getType*(scope: Scope, semexpr: SemanticExpr): Symbol =
  return semexpr.typesym
proc getRetType*(scope: Scope, sym: Symbol): Symbol =
  let semexpr = scope.getSemanticExpr(sym)
  if semexpr.kind == semanticFunction:
    return semexpr.function.rettype
  elif semexpr.kind == semanticPrimitiveFunc:
    return semexpr.primFuncRetType
  else:
    sym.raiseError("expression is not function: $#" % $sym)
    raise newException(SemanticError, "expression is not function")

proc evalFuncCall*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let trysym = scope.trySymbol(sexpr, $sexpr.first)
  if trysym.isSome:
    let semexpr = scope.trySemanticExpr(trysym.get)
    if semexpr.isSome and semexpr.get.kind == semanticPrimitiveMacro:
      return scope.evalSExpr(semexpr.get.macroproc(scope, sexpr))
    if semexpr.isSome and semexpr.get.kind == semanticPrimitiveEval:
      return semexpr.get.evalproc(scope, sexpr)

  var args = newSeq[SemanticExpr]()
  for arg in sexpr.rest:
    args.add(scope.evalSExpr(arg))
  let argtypes = args.mapIt(scope.getType(it))
  # TODO: support generics
  let sym = newSymbol(sexpr, scope.module, $sexpr.first, argtypes.mapIt(scope.getSymbolArg(it)))
  scope.checkSymbol(sym)
  result = newSemanticExpr(
    sexpr,
    semanticFuncCall,
    funccall: FuncCall(
      callfunc: sym,
      args: args,
    ),
  )
  result.typesym = scope.getRetType(sym)

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
    semexpr.typesym = scope.getSemanticExpr(sym).typesym
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
    if semexpr.kind == semanticSymbol and semexpr.symbol == notTypeSym:
      discard
    else:
      module.toplevelcalls.add(semexpr)
