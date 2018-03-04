
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
    symbolVar
    symbolType
    symbolGenerics
    symbolTypeGenerics
    symbolFunc
    symbolFuncGenerics
    symbolInfix
    symbolMacro
    symbolInternal
  Symbol* = ref object
    scope*: Scope
    name*: Name
    kind*: SymbolKind
    types*: seq[Symbol]
    fexpr*: FExpr
    instance*: Option[Symbol]
  Name* = object
    names*: seq[string]
  FExprKind* = enum
    fexprIdent = 0
    fexprPrefix
    fexprInfix

    fexprQuote
    fexprSymbol
    
    fexprIntLit
    fexprStrLit

    fexprSeq
    fexprArray
    fexprList
    fexprBlock

  CTRC* = ref object
    refcnt*: int
    link*: CTRC

  FExpr* = ref object
    span*: Span
    metadata*: Table[string, Metadata]
    case kind*: FExprKind
    of fexprIdent, fexprPrefix, fexprInfix:
      idname*: Name
      resolve*: FExpr
    of fexprQuote:
      quoted*: FExpr
    of fexprSymbol:
      symbol*: Symbol
    of fexprIntLit:
      intval*: int64
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
    isMacro*: bool
    macroproc*: MacroProc
    name*: Name
    argtypes*: seq[Symbol]
    generics*: seq[Symbol]
    returntype*: Symbol
    sym*: Symbol
  ProcName* = object
    name*: Name
    argtypes*: seq[Symbol]
    generics*: seq[Symbol]
  ProcDeclGroup* = object
    decls*: seq[ProcDecl]
  Scope* = ref object
    ctx*: SemanticContext
    name*: Name
    top*: Scope
    level*: int
    decls*: Table[Name, Symbol]
    procdecls*: Table[Name, ProcDeclGroup]
    importscopes*: OrderedTable[Name, Scope]
    toplevels*: seq[FExpr]
  SemanticContext* = ref object
    modules*: OrderedTable[Name, Scope]
    macrolib*: LibHandle
    macroprocs*: seq[MacroProc]
    tmpcount*: int

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
  Symbol(scope: scope, name: name, kind: kind, types: @[], fexpr: fexpr)
proc `==`*(a, b: Symbol): bool =
  a.name == b.name and a.scope == b.scope
proc `$`*(sym: Symbol): string =
  if sym.types.len == 0:
    $sym.scope.name & "." & $sym.name
  else:
    $sym.scope.name & "." & $sym.name & "[" & sym.types.mapIt($it).join(",") & "]"

#
# is spec
#
  
proc isSpecSymbol*(sym: Symbol): bool =
  if sym.kind == symbolType:
    return true
  elif sym.kind == symbolGenerics:
    return false
  else:
    for t in sym.types:
      if not t.isSpecSymbol:
        return false
    return true
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
