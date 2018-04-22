
import fexpr_core

import options
import strutils, sequtils
import tables

type
  ParsedType* = ref object
    prefix*: Option[FExpr]
    typ*: FExpr
    generics*: FExpr
    ret*: ParsedType

proc parseTypeExpr*(fexpr: FExpr, pos: var int): ParsedType =
  if fexpr.kind in {fexprIdent, fexprSymbol, fexprQuote}:
    result = ParsedType(typ: fexpr, generics: farray(fexpr.span), prefix: none(FExpr))
  elif fexpr.kind == fexprIntLit:
    result = ParsedType(typ: fexpr, generics: farray(fexpr.span), prefix: none(FExpr))
  elif fexpr.kind == fexprSeq:
    new result
    if $fexpr[pos] == "ref" or $fexpr[pos] == "move":
      result.prefix = some(fexpr[pos])
      pos += 1
    else:
      result.prefix = none(FExpr)

    if fexpr.isParametricTypeExpr(pos):
      result.typ = fexpr[pos]
      result.generics = fexpr[pos+1]
      pos += 2
    else:
      result.typ = fexpr[pos]
      result.generics = farray(fexpr[pos].span)
      pos += 1

    if $result.typ == "Fn":
      if pos < fexpr.len:
        result.ret = fexpr.parseTypeExpr(pos)
      else:
        result.ret = ParsedType(typ: fident(fexpr.span, name("Void")), generics: farray(fexpr.span), prefix: none(FExpr))
  else:
    fexpr[pos].error("$# isn't type expression." % $fexpr[pos])

proc semType*(scope: Scope, parsed: ParsedType): Symbol =
  if parsed.typ.kind == fexprIntLit:
    return scope.intsym(parsed.typ)

  if $parsed.typ == "Fn":
    let sym = scope.symbol(name("Fn"), symbolFuncType, parsed.typ)
    sym.argtypes = @[]
    for arg in parsed.generics.mitems:
      var pos = 1
      let argtyp = arg.parseTypeExpr(pos)
      sym.argtypes.add(scope.semType(argtyp))
    sym.rettype = scope.semType(parsed.ret)
    return sym
  
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

  if parsed.prefix.isSome:
    if $parsed.prefix.get == "ref":
      result = scope.refsym(result)
    elif $parsed.prefix.get == "move":
      result = scope.movesym(result)

proc semTypeExpr*(scope: Scope, typ: FExpr): Symbol =
  var pos = 0
  let t = parseTypeExpr(typ, pos)
  return scope.semType(t)
