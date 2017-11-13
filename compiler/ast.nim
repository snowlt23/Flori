
import fexpr

type
  ASTExprKind* = enum
    astVar
    astNil
    astInt
    astStr
    astCall
    astOp
    astAssign
    astIf
    astWhile
    astFor
    # astArray
  ASTExpr* = ref object
    fexpr*: FExpr
    case kind*: ASTExprKind

  ASTDeclKind* = enum
    astFuncDecl
    astVarDecl
    astTypeDecl
  ASTDecl* = ref object
    span*: Span
    case kind*: ASTExprKind
