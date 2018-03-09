
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

proc parseTypeExpr*(fexpr: FExpr, pos: var int): tuple[typ: FExpr, generics: FExpr] =
  if fexpr.kind in {fexprIdent, fexprSymbol, fexprQuote}:
    result = (fexpr, farray(fexpr.span))
  elif fexpr.isParametricTypeExpr(pos):
    result = (fexpr[pos], fexpr[pos+1])
    pos += 2
  elif fexpr[pos].kind in {fexprIdent, fexprQuote}:
    result = (fexpr[pos], farray(fexpr[pos].span))
    pos += 1
  else:
    fexpr[pos].error("$# isn't type expression." % $fexpr[pos])

proc semType*(scope: Scope, typ: FExpr, generics: FExpr): Symbol =
  let opt = scope.getDecl(name(typ))
  if opt.isNone:
    typ.error("undeclared $# type." % $typ)
  if generics.len == 0:
    if opt.get.instance.isSome:
      return opt.get.instance.get
    else:
      return opt.get
  else:
    if opt.get.instance.isSome:
      return opt.get.instance.get
      
    var sym = opt.get.scope.symbol(opt.get.name, symbolTypeGenerics, opt.get.fexpr)
    for arg in generics.mitems:
      var pos = 0
      let argtyp = arg.parseTypeExpr(pos)
      sym.types.add(scope.semType(argtyp.typ, argtyp.generics))
    return sym

proc semTypeExpr*(scope: Scope, typ: FExpr): Symbol =
  var pos = 0
  let t = parseTypeExpr(typ, pos)
  return scope.semType(t.typ, t.generics)

proc genCall*(name: FExpr, args: varargs[FExpr]): FExpr =
  fseq(name.span, @[name, flist(name.span, @args)])

proc genTmpName*(ctx: SemanticContext): Name =
  result = name("tmpid" & $ctx.tmpcount)
  ctx.tmpcount.inc

proc genManglingName*(name: Name, types: seq[Symbol]): Name =
  name($name & "_" & types.mapIt($it).join("_"))
