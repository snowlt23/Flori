
import parser, types, fexpr, scope, metadata
import expandpass

import options
import strutils, sequtils
import tables

type
  ParsedType* = object
    typ*: FExpr
    generics*: FExpr
    isref*: bool

proc parseTypeExpr*(fexpr: FExpr, pos: var int): ParsedType =
  if fexpr.kind in {fexprIdent, fexprSymbol, fexprQuote}:
    result = ParsedType(typ: fexpr, generics: farray(fexpr.span), isref: false)
  elif fexpr.kind == fexprIntLit:
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
  if parsed.typ.kind == fexprIntLit:
    return scope.intsym(parsed.typ)
  
  let opt = scope.getDecl(name(parsed.typ))
  if opt.isNone:
    parsed.typ.error("undeclared $# type." % $parsed.typ)
  if opt.get.fexpr.hasInternalMark and opt.get.fexpr.internalMark == internalConst:
    var pos = 0
    let parsed = parseTypeExpr(opt.get.fexpr.constvalue, pos)
    return scope.semType(parsed)
  elif parsed.generics.len == 0:
    if opt.get.instance.isSome:
      result = opt.get.instance.get
    else:
      result = opt.get
  else:
    # if opt.get.instance.isSome:
    #   result = opt.get.instance.get

    var sym = opt.get.scope.symbol(opt.get.name, symbolTypeGenerics, opt.get.fexpr)
    for arg in parsed.generics.mitems:
      var pos = 0
      let argtyp = arg.parseTypeExpr(pos)
      sym.types.add(scope.semType(argtyp))
    result = sym
    
    if result.types.isSpecTypes and result.fexpr.hasDeftype and result.fexpr.deftype.isGenerics:
      result = scope.expandDeftype(result.fexpr, result.types).symbol

  if parsed.isref:
    result = scope.refsym(result)

proc semTypeExpr*(scope: Scope, typ: FExpr): Symbol =
  var pos = 0
  let t = parseTypeExpr(typ, pos)
  return scope.semType(t)
