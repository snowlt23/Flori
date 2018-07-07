
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
defineInternalType(intlittype, "int", 4)
defineInternalType(floatlittype, "float", 4)
defineInternalType(strlittype, "cstring", 4)
defineInternalType(pointertype, "pointer", 4)
defineInternalType(ptrtype, "ptr", 4)
defineInternalType(undeftype, "undef", 0)
defineInternalType(uniontype, "union", 0)
var fntypeString*: IString
var internalScope*: FScope

proc initInternalPrimitive*(scope: FScope) =
  scope.instantiateInternalType()
  fntypeString = istring("fn")

proc ptrtype*(sym: Symbol): Symbol =
  result = sym.scope.symbol(ptrtypeIdent.idname, symbolTypeGenerics, ptrtypeIdent)
  result.obj.types = iarray([sym])

proc tmpsym*(): string =
  result = "tmpid" & $gCtx.tmpcount
  gCtx.tmpcount.inc

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

# proc hasSetter*(scope: FScope, dstvalue: Symbol, dstindex: Symbol, value: Symbol): bool =
#   scope.getFunc(procname("!!", @[dstvalue, dstindex, value])).isSome

proc resolveByVoid*(fexpr: FExpr) =
  fexpr.typ = some(voidtypeSymbol)

proc isEqualTypes*(types: seq[Symbol]): bool =
  let first = types[0]
  for i in 1..<types.len:
    if $types[i] == "undef":
      continue
    if not first.match(types[i]):
      return false
  return true

proc getargtypes*(pd: ProcDecl): Option[IArray[Symbol]] =
  pd.sym.fexpr.internal.obj.argtypes
proc getReturnType*(pd: ProcDecl): Symbol =
  pd.sym.fexpr.obj.internal.get.obj.returntype
proc undecided*(pd: ProcDecl): bool =
  pd.sym.fexpr.obj.internal.get.obj.undecided
proc gettype*(f: FExpr): Symbol =
  if f.typ.isNone:
    f.error("expression hasn't type.")
  return f.typ.get

proc hasCopy*(scope: FScope, dstvalue: Symbol, value: Symbol): bool =
  for word in scope.getWords("copy"):
    if word.getargtypes.isNone: continue
    if word.getargtypes.get.len != 2: continue
    if word.getargtypes.get[0].match(dstvalue) and word.getargtypes.get[1].match(value):
      return true
  return false

proc filterWords*(words: openArray[ProcDecl], arglen: int): seq[ProcDecl] =
  result = @[]
  for word in words:
    if word.getargtypes.isNone or arglen == word.getargtypes.get.len or word.undecided:
      result.add(word)
proc argumentUnion*(words: openArray[ProcDecl], index: int): Option[Symbol] =
  var typs = newSeq[Symbol]()
  for word in words:
    if word.getargtypes.isNone:
      continue
    if word.undecided:
      continue
    typs.add(word.getargtypes.get[index])
  if typs.len == 0:
    return none(Symbol)
  else:
    return some(unionsym(typs))

proc typesize*(sym: Symbol): int =
  if sym.kind == symbolLink:
    return sym.wrapped.typesize
  return sym.fexpr.internal.obj.internalsize

proc linkinfer*(sym: var Symbol, by: Symbol): Option[string] =
  if sym.kind == symbolLink and $sym == "undef":
    sym.wrapped = by
    return none(string)
  if by.kind == symbolLink and $by == "undef":
    return none(string)
  if sym.kind == symbolLink:
    return sym.wrapped.linkinfer(by)
  if by.kind == symbolLink:
    return sym.linkinfer(by.wrapped)
  if sym.kind == symbolVar:
    return sym.wrapped.linkinfer(by)
  if by.kind == symbolVar:
    return sym.linkinfer(by.wrapped)
  if sym.kind == symbolUnion and by.kind == symbolType:
    let atypes = toSeq(sym.uniontypes.items)
    if not atypes.hasUnionType(by):
      return some("union type mismatch $#:$#" % [$sym, $by])
    sym = by
    return none(string)
  if sym.kind == symbolType and by.kind == symbolUnion:
    let atypes = toSeq(by.uniontypes.items)
    if not atypes.hasUnionType(sym):
      return some("union type mismatch $#:$#" % [$sym, $by])
    return none(string)
  if sym.kind == symbolUnion and by.kind == symbolUnion:
    var unify = newSeq[Symbol]()
    let atypes = toSeq(sym.uniontypes.items)
    for t in by.uniontypes:
      if atypes.hasUnionType(t):
        unify.add(t)
    if unify.len == 0:
      return some("union type mismatch $#:$#" % [$sym, $by])
    sym = unionsym(unify)
    return none(string)
  if not sym.match(by):
    return some("type mismatch $#:$#" % [$sym, $by])
  return none(string)
proc linkinfer*(sym: var Option[Symbol], by: Symbol): Option[string] =
  if sym.isSome:
    var s = sym.get
    result = s.linkinfer(by)
    sym = some(s)
  else:
    sym = some(by)
