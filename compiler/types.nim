
import options
import tables, hashes
import strutils, sequtils

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
    symbolGenerics
    symbolTypeGenerics
    symbolFunc
    symbolFuncGenerics
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
    fexprIdent
    fexprPrefix
    fexprShort
    fexprInfix

    fexprQuote
    fexprSymbol
    
    fexprIntLit
    fexprStrLit

    fexprSeq
    fexprArray
    fexprList
    fexprBlock

  FExpr* = ref object
    span*: Span
    typ*: Option[Symbol]
    metadata*: Table[string, Metadata]
    case kind*: FExprKind
    of fexprIdent, fexprPrefix, fexprShort, fexprInfix:
      idname*: Name
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
    importscopes*: OrderedTable[Name, Scope]
    toplevels*: seq[FExpr]
  SemanticContext* = ref object
    modules*: OrderedTable[Name, Scope]
  
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

proc symbol*(scope: Scope, name: Name, kind: SymbolKind, fexpr: FExpr): Symbol =
  Symbol(scope: scope, name: name, kind: kind, types: @[], fexpr: fexpr)
proc `==`*(a, b: Symbol): bool =
  a.name == b.name and a.scope == b.scope
proc `$`*(sym: Symbol): string =
  if sym.types.len == 0:
    $sym.scope.name & "." & $sym.name
  else:
    $sym.scope.name & "." & $sym.name & "|(" & sym.types.mapIt($it).join(", ") & ")"
