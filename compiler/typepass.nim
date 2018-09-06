
import fexpr_core
import expand_templates

import options
import strutils, sequtils
import tables

type
  ParsedType* = ref object
    prefix*: Option[FExpr]
    typ*: FExpr
    generics*: FExpr
    ret*: FExpr

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
        result.ret = fexpr[pos]
      else:
        result.ret = fseq(fexpr.span, @[fident(fexpr.span, istring("Void"))])
  else:
    fexpr[pos].error("$# isn't type expression." % $fexpr[pos])

proc semType*(scope: Scope, fexpr: FExpr): Symbol =
  if fexpr.kind == fexprSymbol:
    return fexpr.symbol
  if fexpr.kind == fexprSeq and fexpr.len == 1 and fexpr[0].kind == fexprSymbol:
    return fexpr[0].symbol
  var pos = 0
  let parsed = parseTypeExpr(fexpr, pos)

  if parsed.typ.kind == fexprIntLit:
    return scope.intsym(parsed.typ)

  if $parsed.typ == "Fn":
    let sym = scope.symbol(istring("Fn"), symbolFuncType, parsed.typ)
    var argtypes = newSeq[Symbol]()
    for arg in parsed.generics.mitems:
      var pos = 1
      argtypes.add(scope.semType(arg))
    sym.argtypes = iarray(argtypes)
    sym.rettype = scope.semType(parsed.ret)
    return sym
  
  let opt = scope.getDecl(name(parsed.typ))
  if opt.isNone:
    parsed.typ.error("undeclared $# type." % $parsed.typ)
  if opt.get.fexpr.metadata.internal == internalConst:
    return scope.semType(opt.get.fexpr.metadata.constvalue)
  elif fexpr.kind == fexprSeq and fexpr[0].kind == fexprSymbol:
    result = fexpr[0].symbol
  elif fexpr.kind == fexprSymbol:
    result = fexpr.symbol
  elif parsed.generics.len == 0:
    # if opt.get.instance.isSome:
    #   result = opt.get.instance.get
    # else:
    #   result = opt.get
    result = opt.get
  else:
    var sym = opt.get.scope.symbol(opt.get.name, symbolTypeGenerics, opt.get.fexpr)
    var types = newSeq[Symbol]()
    for arg in parsed.generics.mitems:
      types.add(scope.semType(arg))
    sym.types = iarray(types)
    result = scope.expandDeftype(sym.obj.fexpr, toSeq(sym.types.items)).symbol.symcopy
    result.types = sym.types

  if parsed.prefix.isSome:
    if $parsed.prefix.get == "ref":
      result = scope.refsym(result)
