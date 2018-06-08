
import fcore, expand_templates

import options
import strutils, sequtils
import tables

type
  ParsedType* = object
    prefix*: Option[int]
    typ*: int
    generics*: Option[int]
    ret*: Option[int]

proc parseTypeExpr*(fexpr: FExpr, pos: var int): ParsedType =
  if fexpr.kind in {fexprIdent, fexprSymbol, fexprQuote}:
    result = ParsedType(typ: -1)
  elif fexpr.kind == fexprSeq:
    if $fexpr[pos] == "ref":
      result.prefix = some(pos)
      pos += 1

    if fexpr.isParametricTypeExpr(pos):
      result.typ = pos
      result.generics = some(pos+1)
      pos += 2
    else:
      result.typ = pos
      pos += 1

    if $fexpr[result.typ] == "Fn":
      if pos < fexpr.len:
        result.ret = some(pos)
        pos += 1
  else:
    fexpr[pos].error("$# isn't type expression." % $fexpr[pos])

proc semType*(scope: FScope, fexpr: FExpr, pos: int): Symbol =
  if fexpr.kind == fexprSymbol:
    return fexpr.symbol
  var pos = pos
  let parsed = parseTypeExpr(fexpr, pos)
  if fexpr.kind == fexprSeq and fexpr[parsed.typ].kind == fexprSymbol:
    return fexpr[parsed.typ].symbol

  if parsed.typ == -1:
    let opt = scope.getDecl($fexpr)
    if opt.isNone:
      fexpr.error("undeclared $# type." % $fexpr)
    return opt.get
  elif $fexpr[parsed.typ] == "Fn":
    let sym = scope.symbol(fntypeString, symbolFuncType, fexpr[parsed.typ])
    var argtypes = newSeq[Symbol]()
    for arg in fexpr[parsed.generics.get].mitems:
      argtypes.add(scope.semType(arg, 0))
    sym.obj.argtypes = iarray(argtypes)
    if parsed.ret.isSome:
      sym.obj.rettype = scope.semType(fexpr[parsed.ret.get], 0)
    else:
      sym.obj.rettype = voidtypeSymbol
    return sym

  let opt = scope.getDecl($fexpr[parsed.typ])
  if opt.isNone:
    fexpr[parsed.typ].error("undeclared $# type." % $fexpr[parsed.typ])
  elif parsed.generics.isSome:
    var sym = opt.get.scope.symbol(opt.get.name, symbolTypeGenerics, opt.get.fexpr)
    var types = newSeq[Symbol]()
    for arg in fexpr[parsed.generics.get]:
      types.add(scope.semType(arg, 0))
    sym.obj.types = iarray(types)
    result = scope.expandDeftype(sym.fexpr, toSeq(sym.types.items)).symbol
  else:
    result = opt.get

  if parsed.prefix.isSome:
    if $fexpr[parsed.prefix.get] == "ref":
      result = refsym(result)
