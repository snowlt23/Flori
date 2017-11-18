
import options
import tables

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
    name*: string
    kind*: SymbolKind
    fexpr*: FExpr
  FExprKind* = enum
    fexprIdent
    fexprSymbol
    fexprPrefix
    fexprInfix
    fexprQuote
    fexprIntLit
    fexprStrLit
    fexprSeq
    fexprArray
    fexprList
    fexprBlock
    fexprCall
  FExpr* = ref object
    span*: Span
    typ*: Option[Symbol]
    metadata*: Table[string, Metadata]
    case kind*: FExprKind
    of fexprIdent:
      ident*: string
    of fexprSymbol:
      symbol*: Symbol
    of fexprPrefix:
      prefix*: string
    of fexprInfix:
      infix*: string
    of fexprQuote:
      quoted*: FExpr
    of fexprIntLit:
      intval*: int64
    of fexprStrLit:
      strval*: string
    of fexprSeq, fexprArray, fexprList, fexprBlock, fexprCall:
      sons*: seq[FExpr]
  Name* = object
    names*: seq[string]
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
  