
import image, parser, fexpr, symbol, scope, localparser

import options
import strutils, sequtils
import tables

proc isGenerics*(fexpr: FExpr, defn: Defn): bool =
  defn.generics.isSome and not fexpr[defn.generics.get].isSpecTypes
proc isGenerics*(fexpr: FExpr, deftype: Deftype): bool =
  deftype.generics.isSome and not fexpr[deftype.generics.get].isSpecTypes

var voidtypeExpr: FExpr

proc initializePrimitive*() =
  voidtypeExpr = fident(internalSpan, "Void")

proc tmpsym*(): string =
  result = "tmpid" & $gCtx.tmpcount
  gCtx.tmpcount.inc

proc genManglingName*(name: string, types: seq[Symbol]): string =
  name & "_" & types.mapIt($it).join("_")

proc expandStart*(span: Span) =
  gCtx.expands.add(span)
proc expandEnd*() =
  gCtx.expands.del(gCtx.expands.high)

template expandBy*(span: Span, body: untyped) =
  try:
    expandStart(span)
    body
  finally:
    expandEnd()

proc hasSetter*(scope: FScope, dstvalue: Symbol, dstindex: Symbol, value: Symbol): bool =
  scope.getFunc(procname("!!", @[dstvalue, dstindex, value])).isSome

# proc checkArgsHastype*(args: FExpr) =
#   for arg in args:
#     if not arg.hasTyp:
#       arg.error("$# hasn't type." % $arg)
      
# proc genConvertedCall*(args: FExpr, matches: seq[Matched]): FExpr =
#   result = flist(args.span)
#   args.assert(args.len == matches.len)
#   for i in 0..<args.len:
#     case matches[i].kind
#     of matchConvert:
#       result.addSon(args[i].span.quoteFExpr("`embed(`embed)", [fsymbol(args[i].span, matches[i].convsym), args[i]]))
#     else:
#       result.addSon(args[i])
