
import fexpr, parser
import scope

import tables
import options
import strutils, sequtils
import os

import types
export types.SemanticContext

import metadata

proc evalFExpr*(ctx: SemanticContext, scope: Scope, fexpr: FExpr)

proc name*(fexpr: FExpr): Name =
  case fexpr.kind
  of fexprIdent:
    return name($fexpr)
  else:
    fexpr.error("$# is not name." % $fexpr)
# proc typename*(fexpr: FExpr): Name =
#   if fexpr.kind == fexprList and $fexpr[0] == "type":
#     return name($fexpr[1])
#   else:
#     fexpr.error("$# is not typename." % $fexpr)

proc addInternalEval*(scope: Scope, n: Name, p: proc (ctx: SemanticContext, scope: Scope, fexpr: FExpr)) =
  let status = scope.addFunc(ProcDecl(isInternal: true, internalproc: p, name: n, argtypes: @[], sym: scope.symbol(n, symbolInternal, fident(internalSpan, "internal")))) # FIXME: returntype
  if not status:
    fnil(internalSpan).error("redefinition $# function." % $n)

proc voidtypeExpr*(span: Span): FExpr =
  flist(span, fident(span, "type"), fident(span, "void"))  

proc isTypeExpr*(fexpr: FExpr): bool =
  fexpr.kind == fexprList and $fexpr[0] == "type"
proc isPragmaExpr*(fexpr: FExpr): bool =
  fexpr.kind == fexprList and $fexpr[0] == "pragma" and fexpr[1].kind == fexprMap
proc isGenericsExpr*(fexpr: FExpr): bool =
  fexpr.kind == fexprList and $fexpr[0] == "pragma" and fexpr[1].kind == fexprArray

proc resolveByType*(scope: Scope, fexpr: FExpr, n: Name) =
  let opt = scope.getDecl(n)
  if opt.isNone:
    fexpr.error("$# type is undefined." % $n)
  fexpr.typ = opt
proc resolveByVoid*(scope: Scope, fexpr: FExpr) =
  scope.resolveByType(fexpr, name("void"))
  
proc evalTypeInside*(ctx: SemanticContext, scope: Scope, fexpr: var FExpr): Symbol =
  if fexpr.kind == fexprList:
    if fexpr.len <= 1:
      fexpr.error("type list requires 2 arguments.")
    let opt = scope.getDecl(name(fexpr[0]))
    if opt.isNone:
      fexpr.error("undeclared $# type." % $fexpr[0])
    var sym = opt.get
    for arg in fexpr[1..^1].mitems:
      sym.types.add(ctx.evalTypeInside(scope, arg))
    fexpr[0] = fsymbol(fexpr[0].span, sym)
    return sym
  else:
    let opt = scope.getDecl(name(fexpr))
    if opt.isNone:
      fexpr.error("undeclared $# type." % $fexpr)
    fexpr = fsymbol(fexpr.span, opt.get)
    return opt.get

proc evalType*(ctx: SemanticContext, scope: Scope, fexpr: var FExpr): Symbol =
  if fexpr.kind != fexprList or $fexpr[0] != "type":
    fexpr.error("$# isn't type." % $fexpr)
  result = ctx.evalTypeInside(scope, fexpr[1])
  fexpr = fsymbol(fexpr.span, result)

proc internalScope*(ctx: SemanticContext): Scope =
  ctx.modules[name("internal")]

proc evalFExpr*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  case fexpr.kind
  of fexprIdent:
    let opt = scope.getDecl(name(fexpr))
    if opt.isNone:
      fexpr.error("undeclared $# ident." % fexpr.ident)
    fexpr.typ = opt.get.fexpr.typ
  of fexprSymbol:
    discard
  of fexprIntLit:
    let opt = scope.getDecl(name("int"))
    if opt.isNone:
      fexpr.error("undeclared ^int type.")
    fexpr.typ = opt
  of fexprStrLit:
    let opt = scope.getDecl(name("cstring"))
    if opt.isNone:
      fexpr.error("undeclared ^cstring type.")
    fexpr.typ = opt
  of fexprList:
    let fn = fexpr[0]
    let internalopt = scope.getFunc(procname(name(fn), @[]))
    if internalopt.isSome:
      internalopt.get.internalproc(ctx, scope, fexpr)
    else:
      let fn = fexpr[0]
      for arg in fexpr[1..^1]:
        ctx.evalFExpr(scope, arg)
      let argtypes = fexpr[1..^1].mapIt(it.typ.get)

      let opt = scope.getFunc(procname(name(fn), argtypes))
      if opt.isNone:
        fexpr.error("undeclared ($# $#) function." % [$fn, argtypes.mapIt($it).join(", ")])
      fexpr.typ = some(opt.get.returntype)
      # symbol resolve
      fexpr[0] = fsymbol(fexpr[0].span, opt.get.sym)
  else:
    fexpr.error("$# is unsupported expression in eval." % $fexpr.kind)

proc evalModule*(ctx: SemanticContext, name: Name, fexprs: seq[FExpr]) =
  let scope = newScope(name)
  scope.importScope(ctx.internalScope)
  for f in fexprs:
    ctx.evalFExpr(scope, f)
    scope.toplevels.add(f)
  ctx.modules[name] = scope

proc evalFile*(ctx: SemanticContext, filepath: string) =
  let (dir, file, _) = filepath.splitFile()
  let n = name((dir / file).replace("/", "_").replace("\\", "_"))
  let fexprs = parseToplevel(filepath, readFile(filepath))
  ctx.evalModule(n, fexprs)
