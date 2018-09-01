
import linmem, image, parser, fexpr, scope, symbol, fnmatch, localparser

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
  return true

proc isGenerics*(fexpr: FExpr): bool =
  fexpr.fnGenerics.isSpecTypes

proc isEqualTypes*(types: seq[Symbol]): bool =
  var first = types[0]
  for t in types[1..^1]:
    if first != t:
      return false
  return true

proc replaceByTypesym*(fexpr: var FExpr, sym: Symbol) =
  fexpr = fsymbol(fexpr.span, sym)

proc voidtypeExpr*(span: Span): FExpr =
  return fident(span, istring("Void"))

proc resolveByVoid*(scope: Scope, fexpr: FExpr) =
  let opt = scope.getDecl("Void")
  if opt.isNone:
    fexpr.error("undeclared Void type, please import prelude.")
  fexpr.metadata.typ = opt.get

proc genCall*(name: FExpr, args: varargs[FExpr]): FExpr =
  fseq(name.span, @[name, flist(name.span, @args)])

proc genTmpName*(ctx: var SemContext): string =
  result = "tmpid" & $ctx.tmpcount
  ctx.tmpcount.inc

proc genManglingName*(name: string, types: seq[Symbol]): string =
  name & "_" & types.mapIt($it).join("_")

proc expandStart*(ctx: var SemContext, span: Span) =
  ctx.expands.add(span)
proc expandEnd*(ctx: var SemContext) =
  ctx.expands.del(ctx.expands.high)

template expandBy*(scope: Scope, span: Span, body: untyped) =
  try:
    gCtx.expandStart(span)
    body
  finally:
    gCtx.expandEnd()

proc isCopyable*(scope: Scope, typ: Symbol): bool =
  scope.getFunc(procname("copy", @[typ])).isSome
proc isDestructable*(scope: Scope, typ: Symbol): bool =
  scope.getFunc(procname("destruct", @[typ])).isSome
proc isSetter*(scope: Scope, dstvalue: Symbol, dstindex: Symbol, value: Symbol): bool =
  scope.getFunc(procname("!!", @[dstvalue, dstindex, value])).isSome

proc checkArgsHastype*(args: FExpr) =
  for arg in args:
    if not arg.hasTyp:
      arg.error("$# hasn't type." % $arg)
      
proc genConvertedCall*(args: FExpr, matches: seq[Matched]): FExpr =
  args.assert(args.len == matches.len)
  var lst = newSeq[FExpr]()
  for i in 0..<args.len:
    case matches[i].kind
    of matchConvert:
      lst.add(args[i].span.quoteFExpr("`embed(`embed)", [fsymbol(args[i].span, matches[i].convsym), args[i]]))
    else:
      lst.add(args[i])
  return flist(args.span, lst)
