
import ast
import semtree
import tables
import strutils
import sequtils
import options

proc addInternalFunc*(scope: SemScope, name: string, p: proc (ctx: SemContext, scope: SemScope, semexpr: SemExpr): SemExpr) =
  discard scope.addFunc(semname(name), SemExpr(fexpr: semexpr.fexpr, scope: scope, typ:  kind: sfInternal, internalproc: p))

proc importSemScope*(scope: SemScope, importscope: SemScope) =
  for key, sym in importscope.decls:
    if not sym.isimported:
      discard scope.addDecl(key, sym.createImportSymbol())
  for key, group in importscope.procdecls:
    for sym in group.decls:
      if not sym.isimported:
        discard scope.addFunc(key, sym.createImportSymbol())

proc internalFn*(ctx: SemContext, scope: SemScope, semexpr: SemExpr): SemExpr =
  return nil

proc initInternal*(internal: SemScope) =
  internal.addInternalFunc("fn", internalFn)

proc init*(ctx: SemContext) =
  let internal = newSemScope(semname("internal"))
  internal.initInternal()

  for module in ctx.modules.values:
    module.importSemScope(internal)

#
# Eval
#

proc resolveByType*(sym: SemSym) =
  if sym.name.kind == seIdent:
    let opt = sym.scope.getType(sym.name.toSemName())
    if opt.isNone:
      sym.name.fexpr.error("undeclared $# type." % sym.name.idname)
    sym.kind = symSemType
    sym.st = opt.get
  else:
    sym.name.fexpr.error("$# is not type expression." % $sym.name.fexpr)

proc evalExpr*(ctx: SemContext, scope: SemScope, semexpr: SemExpr): SemExpr =
  case semexpr.kind
  of seFuncCall:
    let args = semexpr.args.mapIt(evalExpr(ctx, scope, it).typ)
    let funcopt = scope.getProc(ProcName(name: semexpr.fn.name.toSemName(), args: args))
    if funcopt.isNone:
      semexpr.fexpr.error("undeclared $# func." % $semexpr.fn.name.toSemName)
    case funcopt.get.kind
    of sfFunc:
      result = semexpr
    of sfMacro:
      result = semexpr # TODO:
    of sfInternal:
      result = funcopt.get.internalproc(ctx, scope, semexpr)
  of seIntLit:
    resolveByType(semexpr.typ)
    result = semexpr
  else:
    result = semexpr

proc evalScope*(ctx: SemContext, scope: SemScope) =
  var newtoplevels = newSeq[SemExpr]()
  for topexpr in scope.toplevels.mitems:
    let retexpr = evalExpr(ctx, scope, topexpr)
    if retexpr != nil:
      newtoplevels.add(retexpr)
  scope.toplevels = newtoplevels

proc eval*(ctx: SemContext) =
  for module in ctx.modules.values:
    evalScope(ctx, module)
