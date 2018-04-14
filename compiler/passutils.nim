
import parser, types, fexpr, scope, metadata

import options
import strutils, sequtils
import tables

proc isIncludeRef*(argtypes: seq[Symbol]): bool =
  for argt in argtypes:
    if argt.kind == symbolRef:
      return true
  return false
proc isIncludeRef*(fexpr: seq[FExpr]): bool =
  for son in fexpr:
    if son.kind != fexprSymbol: return false
    if son.symbol.kind == symbolRef:
      return true
  return false
proc isResolveRef*(fexpr: seq[FExpr]): bool =
  for son in fexpr:
    if son.kind != fexprSymbol: return false
    if son.symbol.kind == symbolRef and son.symbol.marking.isNone:
      return false
  return true

# proc isBorrow*(sym: Symbol): bool =
#   if sym.kind == symbolDynamic and sym.marking.get.dynamic == dynBorrow:
#     return true
#   elif sym.kind == symbolTypeGenerics:
#     for t in sym.types:
#       if t.isBorrow:
#         return true
#     return false
#   else:
#     return false
# proc isBorrow*(deftype: Deftype): bool =
#   for sym in deftype.body.mapIt(it[1].symbol):
#     if sym.isBorrow:
#       return true
#   return false
# proc isShare*(sym: Symbol): bool =
#   if sym.kind == symbolDynamic and sym.marking.get.dynamic == dynShare:
#     return true
#   elif sym.kind == symbolTypeGenerics:
#     for t in sym.types:
#       if t.isShare:
#         return true
#     return false
#   else:
#     return false
# proc isShare*(deftype: Deftype): bool =
#   for sym in deftype.body.mapIt(it[1].symbol):
#     if sym.isShare:
#       return true
#   return false

proc isGenerics*(defn: Defn): bool = not defn.generics.isSpecTypes or defn.args.mapIt(it[1]).isIncludeRef
proc isGenerics*(deftype: Deftype): bool = not deftype.generics.isSpecTypes

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

proc genCall*(name: FExpr, args: varargs[FExpr]): FExpr =
  fseq(name.span, @[name, flist(name.span, @args)])

proc genTmpName*(ctx: SemanticContext): Name =
  result = name("tmpid" & $ctx.tmpcount)
  ctx.tmpcount.inc

proc genManglingName*(name: Name, types: seq[Symbol]): Name =
  name($name & "_" & types.mapIt($it).join("_"))

proc expandStart*(scope: Scope, span: Span) =
  scope.ctx.expands.add(span)
proc expandEnd*(scope: Scope) =
  scope.ctx.expands.del(scope.ctx.expands.high)

template expandBy*(scope: Scope, span: Span, body: untyped) =
  try:
    scope.expandStart(span)
    body
  finally:
    scope.expandEnd()

proc isDestructable*(scope: Scope, typ: Symbol): bool =
  scope.getFunc(procname(name("destruct"), @[typ])).isSome
