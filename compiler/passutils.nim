
import image, parser, fexpr, symbol, scope

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

defineInternalType(voidtype, "void", 0)
defineInternalType(booltype, "bool", 4)
defineInternalType(intlittype, "intlit", 4)
defineInternalType(floatlittype, "floatlit", 4)
defineInternalType(strlittype, "strlit", 4)
defineInternalType(undeftype, "undef", 0)
defineInternalType(uniontype, "union", 0)
var fntypeString*: IString
var internalScope*: FScope

proc initInternalPrimitive*(scope: FScope) =
  scope.instantiateInternalType()
  fntypeString = istring("fn")

proc tmpsym*(): string =
  result = "tmpid" & $gCtx.tmpcount
  gCtx.tmpcount.inc

proc getReturnType*(pd: ProcDecl): Symbol =
  pd.sym.fexpr.obj.internal.get.obj.returntype.get

proc hasUnionType*(syms: seq[Symbol], s: Symbol): bool =
  for sym in syms:
    if sym.match(s):
      return true
  return false
proc unionsym*(syms: openArray[Symbol]): Symbol =
  if syms.len == 1:
    result = syms[0]
  else:
    result = syms[0].scope.symbol(uniontypeident.idname, symbolUnion, uniontypeIdent)
    var types = newSeq[Symbol]()
    for sym in syms:
      if types.hasUnionType(sym):
        continue
      types.add(sym)
    result.obj.uniontypes = iarray(types)

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

proc hasSetter*(scope: FScope, dstvalue: Symbol, dstindex: Symbol, value: Symbol): bool =
  scope.getFunc(procname("!!", @[dstvalue, dstindex, value])).isSome

proc resolveByVoid*(fexpr: FExpr) =
  fexpr.typ = some(voidtypeSymbol)

proc isEqualTypes*(types: seq[Symbol]): bool =
  let first = types[0]
  for i in 1..<types.len:
    if not first.spec(types[i]):
      return false
  return true

proc typesize*(sym: Symbol): int =
  if sym.kind == symbolLink:
    return sym.wrapped.typesize
  return sym.fexpr.internal.obj.internalsize
