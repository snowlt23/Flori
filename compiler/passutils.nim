
import parser, types, fexpr, scope, metadata

import options
import strutils, sequtils
import tables

proc isEqualTypes*(types: seq[Symbol]): bool =
  var first = types[0]
  for t in types[1..^1]:
    if first != t:
      return false
  return true

proc replaceByTypesym*(fexpr: var FExpr, sym: Symbol) =
  fexpr = fsymbol(fexpr.span, sym)

proc voidtypeExpr*(span: Span): FExpr =
  return fident(span, name("Void"))

proc resolveByVoid*(scope: Scope, fexpr: FExpr) =
  let opt = scope.getDecl(name("Void"))
  if opt.isNone:
    fexpr.error("undeclared Void type, please import prelude.")
  fexpr.typ = opt.get

proc genCall*(name: FExpr, args: varargs[FExpr]): FExpr =
  fseq(name.span, @[name, flist(name.span, @args)])

proc genTmpName*(ctx: SemanticContext): Name =
  result = name("tmpid" & $ctx.tmpcount)
  ctx.tmpcount.inc

proc genManglingName*(name: Name, types: seq[Symbol]): Name =
  name($name & "_" & types.mapIt($it).join("_"))
