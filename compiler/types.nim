
import options
import tables, hashes
import strutils, sequtils
import deques
import terminal
import dynlib

type
  Metadata* = ref object of RootObj
  FExprError* = object of Exception
  Span* = ref object
    filename*: string
    line*: int
    linepos*: int
    pos*: int
    internal*: tuple[filename: string, line: int]
  SymbolKind* = enum
    symbolDef
    symbolArg
    symbolType
    symbolGenerics
    symbolTypeGenerics
    symbolVar
    symbolRef
    symbolOnce
    symbolFunc
    symbolFuncGenerics
    symbolInfix
    symbolMacro
    symbolSyntax
    symbolInternal
    symbolIntLit
  Symbol* = ref object
    scope*: Scope
    name*: Name
    fexpr*: FExpr
    instance*: Option[Symbol]
    case kind*: SymbolKind
    of symbolArg:
      argpos*: int
    of symbolTypeGenerics:
      types*: seq[Symbol]
    of symbolVar, symbolRef, symbolOnce:
      wrapped*: Symbol
    of symbolSyntax, symbolMacro:
      macroproc*: MacroProc
    of symbolIntLit:
      intval*: int64
    else:
      discard
  Name* = object
    names*: seq[string]
  FExprKind* = enum
    fexprIdent = 0
    fexprPrefix
    fexprInfix

    fexprQuote
    fexprSymbol
    
    fexprIntLit
    fexprFloatLit
    fexprStrLit

    fexprSeq
    fexprArray
    fexprList
    fexprBlock

  CTRC* = ref object
    refcnt*: int
    depends*: seq[CTRC]
    dest*: bool
    exdest*: bool
    ret*: bool
    fieldbody*: Table[Name, FExpr]
    alias*: Option[CTRC]
    fuzzy*: bool
  Effect* = object
    trackings*: seq[Depend]
    resulttypes*: seq[Symbol]
    retctrc*: Option[CTRC]

  FExpr* = ref object
    span*: Span
    metadata*: Table[string, Metadata]
    case kind*: FExprKind
    of fexprIdent, fexprPrefix, fexprInfix:
      idname*: Name
      priority*: int
      isleft*: bool
    of fexprQuote:
      quoted*: FExpr
    of fexprSymbol:
      symbol*: Symbol
    of fexprIntLit:
      intval*: int64
    of fexprFloatLit:
      floatval*: float
    of fexprStrLit:
      strval*: string
    of fexprSeq, fexprArray, fexprList, fexprBlock:
      sons*: seq[FExpr]
  MacroProc* = ref object
    importname*: string
    call*: proc (fexpr: FExpr): FExpr {.cdecl.}
  PassProcType* = proc (scope: Scope, fexpr: var FExpr)
  InternalProcType* = proc (rootPass: PassProcType, scope: Scope, fexpr: var FExpr)
  ProcDecl* = object
    isInternal*: bool
    internalProc*: InternalProcType
    isSyntax*: bool
    isMacro*: bool
    macroproc*: MacroProc
    name*: Name
    argtypes*: seq[Symbol]
    generics*: seq[Symbol]
    returntype*: Symbol
    sym*: Symbol
    fexpr*: FExpr
  ProcName* = object
    name*: Name
    argtypes*: seq[Symbol]
    generics*: seq[Symbol]
  ProcDeclGroup* = object
    decls*: seq[ProcDecl]
  Depend* = object
    left*: FExpr
    right*: FExpr
  Scope* = ref object
    ctx*: SemanticContext
    path*: string
    name*: Name
    top*: Scope
    level*: int
    decls*: Table[Name, Symbol]
    procdecls*: Table[Name, ProcDeclGroup]
    importscopes*: OrderedTable[Name, Scope]
    exportscopes*: OrderedTable[Name, Scope]
    toplevels*: seq[FExpr]
    scopevalues*: seq[FExpr]
    scopedepends*: seq[Depend]
  SemanticContext* = ref object
    modules*: OrderedTable[Name, Scope]
    globaltoplevels*: seq[FExpr]
    macrolib*: LibHandle
    macroprocs*: seq[MacroProc]
    tmpcount*: int
    importpaths*: seq[string]
    ccoptions*: string
    expands*: seq[Span]

# globals
    
var gCtx*: SemanticContext
    
#
# Scope
#

proc `==`*(a, b: Scope): bool =
  a.name == b.name and a.level == b.level
  
#
# Name
#

proc name*(s: varargs[string]): Name = Name(names: @s)
proc hash*(name: Name): Hash = hash(name.names.join("_"))
proc `==`*(a, b: Name): bool =
  if a.names.len != b.names.len: return false
  for i in 0..<a.names.len:
    if a.names[i] != b.names[i]:
      return false
  return true
proc `$`*(name: Name): string = name.names.join(".")

#
# Symbol
#

proc symbol*(scope: Scope, name: Name, kind: SymbolKind, fexpr: FExpr): Symbol =
  result = Symbol(scope: scope, name: name, kind: kind, fexpr: fexpr)
  if kind == symbolTypeGenerics:
    result.types = @[]
proc `==`*(a, b: Symbol): bool =
  a.name == b.name and a.scope == b.scope
proc `$`*(sym: Symbol): string =
  case sym.kind
  of symbolTypeGenerics:
    $sym.scope.name & "." & $sym.name & "[" & sym.types.mapIt($it).join(",") & "]"
  of symbolRef:
    "ref " & $sym.wrapped
  of symbolOnce:
    "once " & $sym.wrapped
  of symbolIntLit:
    $sym.intval
  else:
    $sym.scope.name & "." & $sym.name
    
proc refsym*(scope: Scope, sym: Symbol): Symbol =
  result = scope.symbol(sym.name, symbolRef, sym.fexpr)
  result.wrapped = sym
proc varsym*(scope: Scope, sym: Symbol): Symbol =
  result = scope.symbol(sym.name, symbolVar, sym.fexpr)
  result.wrapped = sym
proc oncesym*(scope: Scope, sym: Symbol): Symbol =
  result = scope.symbol(sym.name, symbolOnce, sym.fexpr)
  result.wrapped = sym
proc symcopy*(sym: Symbol): Symbol =
  result = sym.scope.symbol(sym.name, sym.kind, sym.fexpr)
  result.instance = sym.instance
  if sym.kind == symbolArg:
    result.argpos = sym.argpos
  elif sym.kind == symbolTypeGenerics:
    result.types = sym.types
  elif sym.kind == symbolIntLit:
    result.intval = sym.intval
proc intsym*(scope: Scope, fexpr: FExpr): Symbol =
  result = scope.symbol(name("IntLit"), symbolIntLit, fexpr)
  result.intval = fexpr.intval

#
# is spec
#
  
proc isSpecSymbol*(sym: Symbol): bool =
  if sym.kind == symbolType:
    return true
  elif sym.kind == symbolGenerics:
    return false
  elif sym.kind in {symbolRef, symbolVar, symbolOnce}:
    return sym.wrapped.isSpecSymbol()
  elif sym.kind == symbolIntLit:
    return true
  elif sym.kind == symbolTypeGenerics:
    for t in sym.types:
      if not t.isSpecSymbol:
        return false
    return true
  else:
    echo "$# kind is not type." % $sym.kind
    assert(false)
proc isSpecTypes*(types: seq[Symbol]): bool =
  for t in types:
    if not t.isSpecSymbol:
      return false
  return true
proc isSpecTypes*(types: FExpr): bool =
  for t in types.sons:
    if t.kind != fexprSymbol: return false
    if not t.symbol.isSpecSymbol: return false
  return true
