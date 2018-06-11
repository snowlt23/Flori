
import image, parser, fexpr, symbol, scope, localparser

import options
import strutils, sequtils
import tables
import macros

var internals {.compileTime.} = newSeq[(NimNode, NimNode, NimNode, string, int)]()
macro defineInternalType*(name: untyped, s: string, size: int): untyped =
  let id = ident($name & "Ident")
  let sym = ident($name & "Symbol")
  let fe = ident($name & "FExpr")
  internals.add((id, sym, fe, s.strval, int(size.intval)))
  result = quote do:
    var `id`*: FExpr
    var `sym`*: Symbol
    var `fe`*: FExpr
macro instantiateInternalType*(scope: FScope): untyped =
  result = newStmtList()
  for internal in internals:
    let (id, sym, fe, s, size) = internal
    result.add(quote do:
      `id` = fident(internalSpan(), `s`)
      `id`.obj.internal = some(newInternalMarker())
      `id`.obj.internal.get.obj.internalsize = `size`
      `sym` = `scope`.symbol(`s`, symbolType, `id`)
      `fe` = fsymbol(internalSpan(), `sym`)
      `scope`.addDecl(istring(`s`), `sym`)
    )

defineInternalType(voidtype, "Void", 0)
defineInternalType(intlittype, "IntLit", 4)
defineInternalType(floatlittype, "FloatLit", 4)
defineInternalType(strlittype, "StrLit", 4)
var fntypeString*: IString
var internalScope*: FScope

proc initInternalPrimitive*(scope: FScope) =
  scope.instantiateInternalType()
  fntypeString = istring("Fn")

proc fntypesym*(scope: FScope, fexpr: FExpr, argtypes: openArray[Symbol], rettype: Symbol): Symbol =
  result = scope.symbol(fntypeString, symbolFuncType, fexpr)
  result.obj.argtypes = iarray(argtypes)
  result.obj.rettype = rettype
proc fntypesym*(scope: FScope, fexpr: FExpr, argtypes: openArray[Symbol]): Symbol =
  scope.fntypesym(fexpr, argtypes, voidtypeSymbol)

proc tmpsym*(): string =
  result = "tmpid" & $gCtx.tmpcount
  gCtx.tmpcount.inc

proc genManglingName*(name: string, types: seq[Symbol], generics: seq[Symbol]): string =
  name & "_" & types.mapIt($it).join("_") & "_" & generics.mapIt($it).join("_")

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

proc isGenerics*(fexpr: FExpr, defn: Defn): bool =
  defn.generics.isSome and not fexpr[defn.generics.get].isSpecTypes
proc isGenerics*(fexpr: FExpr, deftype: Deftype): bool =
  deftype.generics.isSome and not fexpr[deftype.generics.get].isSpecTypes

proc hasSetter*(scope: FScope, dstvalue: Symbol, dstindex: Symbol, value: Symbol): bool =
  scope.getFunc(procname("!!", @[dstvalue, dstindex, value])).isSome

proc resolveByVoid*(fexpr: FExpr) =
  fexpr.typ = some(voidtypeSymbol)

proc checkArgsHastype*(args: seq[FExpr]) =
  for arg in args:
    if arg.typ.isNone:
      arg.error("$# hasn't type." % $arg)
proc checkArgsHastype*(args: FExpr) =
  for arg in args:
    if arg.typ.isNone:
      arg.error("$# hasn't type." % $arg)

proc isEqualTypes*(types: seq[Symbol]): bool =
  let first = types[0]
  for i in 1..<types.len:
    if not first.spec(types[i]):
      return false
  return true
      
# proc genConvertedCall*(args: FExpr, matches: seq[Matched]): FExpr =
#   result = flist(args.span)
#   args.assert(args.len == matches.len)
#   for i in 0..<args.len:
#     case matches[i].kind
#     of matchConvert:
#       result.addSon(args[i].span.quoteFExpr("`embed(`embed)", [fsymbol(args[i].span, matches[i].convsym), args[i]]))
#     else:
#       result.addSon(args[i])

proc typesize*(fexpr: FExpr): int =
  if fexpr.obj.internal.isNone:
    fexpr.error("$# type hasn't size." % $fexpr)
  return fexpr.obj.internal.get.obj.internalsize
