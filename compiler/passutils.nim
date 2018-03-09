
import parser, types, fexpr, scope, metadata

import options
import strutils, sequtils
import tables

type
  ParsedType* = object
    typ*: FExpr
    generics*: FExpr
    isref*: bool

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

proc parseTypeExpr*(fexpr: FExpr, pos: var int): ParsedType =
  if fexpr.kind in {fexprIdent, fexprSymbol, fexprQuote}:
    result = ParsedType(typ: fexpr, generics: farray(fexpr.span), isref: false)
  elif fexpr.kind == fexprSeq:
    if $fexpr[pos] == "ref":
      result.isref = true
      pos += 1
    else:
      result.isref = false

    if fexpr.isParametricTypeExpr(pos):
      result.typ = fexpr[pos]
      result.generics = fexpr[pos+1]
      pos += 2
    else:
      result.typ = fexpr[pos]
      result.generics = farray(fexpr[pos].span)
      pos += 1
  else:
    fexpr[pos].error("$# isn't type expression." % $fexpr[pos])

proc semType*(scope: Scope, parsed: ParsedType): Symbol =
  let opt = scope.getDecl(name(parsed.typ))
  if opt.isNone:
    parsed.typ.error("undeclared $# type." % $parsed.typ)
  if parsed.generics.len == 0:
    if opt.get.instance.isSome:
      result = opt.get.instance.get
    else:
      result = opt.get
  else:
    if opt.get.instance.isSome:
      result = opt.get.instance.get
      
    var sym = opt.get.scope.symbol(opt.get.name, symbolTypeGenerics, opt.get.fexpr)
    for arg in parsed.generics.mitems:
      var pos = 0
      let argtyp = arg.parseTypeExpr(pos)
      sym.types.add(scope.semType(argtyp))
    result = sym
    
  if parsed.isref:
    result = scope.refsym(result)

proc semTypeExpr*(scope: Scope, typ: FExpr): Symbol =
  var pos = 0
  let t = parseTypeExpr(typ, pos)
  return scope.semType(t)

proc genCall*(name: FExpr, args: varargs[FExpr]): FExpr =
  fseq(name.span, @[name, flist(name.span, @args)])

proc genTmpName*(ctx: SemanticContext): Name =
  result = name("tmpid" & $ctx.tmpcount)
  ctx.tmpcount.inc

proc genManglingName*(name: Name, types: seq[Symbol]): Name =
  name($name & "_" & types.mapIt($it).join("_"))
