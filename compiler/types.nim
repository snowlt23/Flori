
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
    symbolMove
    symbolFunc
    symbolFuncGenerics
    symbolFuncType
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
    marking*: Option[Marking]
    instmarking*: Option[Marking]
    case kind*: SymbolKind
    of symbolArg:
      argpos*: int
    of symbolTypeGenerics:
      types*: seq[Symbol]
    of symbolVar, symbolRef, symbolMove:
      wrapped*: Symbol
    of symbolSyntax, symbolMacro:
      macroproc*: MacroProc
    of symbolFuncType:
      argtypes*: seq[Symbol]
      rettype*: Symbol
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

  MarkingEffect* = ref object
    moved*: bool
    fieldbody*: Table[Name, MarkingEffect]
  FnEffect* = ref object
    argeffs*: seq[MarkingEffect]
  Marking* = ref object
    scope*: Scope
    typesym*: Symbol
    owned*: bool
    origin*: Marking
    fieldbody*: Table[Name, Marking]

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
      floatval*: float64
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
    nodestruct*: bool
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
proc toString*(sym: Symbol, desc: bool): string =
  case sym.kind
  of symbolTypeGenerics:
    if desc:
      $sym.scope.name & "." & $sym.name & "[" & sym.types.mapIt(toString(it, desc)).join(",") & "]"
    else:
      $sym.name & "[" & sym.types.mapIt(toString(it, desc)).join(",") & "]"
  of symbolVar:
    toString(sym.wrapped, desc)
  of symbolRef:
    "ref " & toString(sym.wrapped, desc)
  of symbolMove:
    "move " & toString(sym.wrapped, desc)
  of symbolFuncType:
    "Fn[$#] $#" % [sym.argtypes.mapIt(toString(it, desc)).join(", "), toString(sym.rettype, desc)]
  of symbolIntLit:
    $sym.intval
  else:
    if desc:
      $sym.scope.name & "." & $sym.name
    else:
      $sym.name
proc `$`*(sym: Symbol): string = toString(sym, false)

proc refsym*(scope: Scope, sym: Symbol): Symbol =
  result = scope.symbol(sym.name, symbolRef, sym.fexpr)
  result.wrapped = sym
  result.marking = none(Marking)
proc varsym*(scope: Scope, sym: Symbol): Symbol =
  result = scope.symbol(sym.name, symbolVar, sym.fexpr)
  result.wrapped = sym
  result.marking = none(Marking)
proc movesym*(scope: Scope, sym: Symbol): Symbol =
  result = scope.symbol(sym.name, symbolMove, sym.fexpr)
  result.wrapped = sym
  result.marking = none(Marking)
proc symcopy*(sym: Symbol): Symbol =
  result = sym.scope.symbol(sym.name, sym.kind, sym.fexpr)
  result.instance = sym.instance
  if sym.kind == symbolArg:
    result.argpos = sym.argpos
  elif sym.kind == symbolTypeGenerics:
    result.types = sym.types
  elif sym.kind == symbolIntLit:
    result.intval = sym.intval
  elif sym.kind == symbolFuncType:
    result.argtypes = sym.argtypes
    result.rettype = sym.rettype
  elif sym.kind in {symbolVar, symbolRef, symbolMove}:
    result.wrapped = sym.wrapped.symcopy
    result.marking = sym.marking
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
  elif sym.kind in {symbolRef, symbolVar, symbolMove}:
    return sym.wrapped.isSpecSymbol()
  elif sym.kind == symbolIntLit:
    return true
  elif sym.kind == symbolFuncType:
    for t in sym.argtypes:
      if not t.isSpecSymbol:
        return false
    if not sym.rettype.isSpecSymbol: return false
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

#
# Marking
#

proc `==`*(a, b: Marking): bool =
  if a.owned != b.owned:
    return false
  for key, value in a.fieldbody:
    if value != b.fieldbody[key]:
      return false
  return true
