
import options
import tables

type
  FExprError* = object of Exception
  Span* = object
    filename*: string
    line*: int
    linepos*: int
    pos*: int
    internal*: tuple[filename: string, line: int]
  SymbolKind* = enum
    symbolType
    symbolFunc
    symbolMacro
    symbolInternal
  Symbol* = object
    scope*: Scope
    name*: string
    case kind*: SymbolKind
    of symbolType:
      discard
    of symbolFunc:
      discard
    of symbolMacro:
      discard
    of symbolInternal:
      internalproc*: proc (ctx: SemanticContext, scope: Scope, fexpr: FExpr)
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
    name*: Name
    argtypes*: seq[Symbol]
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
  SemanticContext* = ref object
    modules*: Table[Name, Scope]
  