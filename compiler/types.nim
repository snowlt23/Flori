
import options
import tables, hashes
import strutils

type
  Metadata* = ref object of RootObj
  FExprError* = object of Exception
  Span* = object
    filename*: string
    line*: int
    linepos*: int
    pos*: int
    internal*: tuple[filename: string, line: int]
  SymbolKind* = enum
    symbolVar
    symbolType
    symbolFunc
    symbolMacro
    symbolInternal
  Symbol* = object
    scope*: Scope
    isImported*: bool
    name*: Name
    kind*: SymbolKind
    fexpr*: FExpr
  Name* = object
    names*: seq[string]
  FExprKind* = enum
    fexprIdent
    fexprAttr
    fexprSymbol
    fexprIntLit
    fexprStrLit
    fexprNil
    fexprList
    fexprArray
    fexprMap
  FExpr* = ref object
    span*: Span
    typ*: Option[Symbol]
    metadata*: Table[string, Metadata]
    reader*: Option[string]
    case kind*: FExprKind
    of fexprIdent:
      ident*: string
    of fexprAttr:
      attr*: string
    of fexprSymbol:
      symbol*: Symbol
    of fexprIntLit:
      intval*: int64
    of fexprStrLit:
      strval*: string
    of fexprNil:
      discard
    of fexprList:
      car*: FExpr
      cdr*: FExpr
    of fexprArray:
      sons*: seq[FExpr]
    of fexprMap:
      tbl*: Table[Name, FExpr]
  ProcDecl* = object
    isInternal*: bool
    internalProc*: proc (ctx: SemanticContext, scope: Scope, fexpr: FExpr)
    name*: Name
    argtypes*: seq[Symbol]
    returntype*: Symbol
    sym*: Symbol
  ProcName* = object
    name*: Name
    argtypes*: seq[Symbol]
  ProcDeclGroup* = object
    decls*: seq[ProcDecl]
  Scope* = ref object
    name*: Name
    top*: Scope
    level*: int
    decls*: Table[Name, Symbol]
    procdecls*: Table[Name, ProcDeclGroup]
    toplevels*: seq[FExpr]
  SemanticContext* = ref object
    modules*: Table[Name, Scope]
  
#
# Name
#
  
proc name*(s: seq[string]): Name = Name(names: s)
proc name*(s: string): Name = name(@[s])
proc hash*(name: Name): Hash = hash(name.names.join("_"))
proc `==`*(a, b: Name): bool =
  if a.names.len != b.names.len: return false
  for i in 0..<a.names.len:
    if a.names[i] != b.names[i]:
      return false
  return true
proc `$`*(name: Name): string = name.names.join(".")
