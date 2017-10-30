
#
# Symbol and Typeinfer resolve
#

import sast
import semtree, sempass
import walker

import strutils, sequtils
import options

type
  ResolvePass* = ref object of SemPass

proc resolveExpr*(pass: ResolvePass, semexpr: SemExpr)

proc newResolvePass*(): ResolvePass =
  ResolvePass()

proc resolveByType*(pass: ResolvePass, sym: SemSym) =
  let opt = sym.scope.getType(TypeIdent(name: sym.name.nameid))
  if opt.isNone:
    sym.name.sexpr.error("undeclared $# type." % sym.name.nameid)
  sym.kind = symSemType
  sym.st = opt.get

proc resolveFuncType*(pass: ResolvePass, ft: FuncType) =
  for argtype in ft.argtypes:
    if argtype.name.kind == seIdent:
      pass.resolveByType(argtype)

proc resolveFunc*(pass: ResolvePass, semfunc: SemFunc) =
  case semfunc.kind
  of sfCFunc:
    pass.resolveFuncType(semfunc.cfunctype)
  of sfFunc:
    pass.resolveFuncType(semfunc.functype)

proc resolveFuncCall*(pass: ResolvePass, semexpr: SemExpr) =
  for arg in semexpr.args:
    pass.resolveExpr(arg)
  let argtypes = semexpr.args.mapIt(it.typ.get)
  let pid = ProcIdentDecl(name: semexpr.fn.name.nameid, args: argtypes)
  let opt = semexpr.fn.scope.getProc(pid)
  if opt.isNone:
    semexpr.sexpr.error("undeclared $# func." % semexpr.fn.name.nameid)
  semexpr.fn.kind = symSemFunc
  semexpr.fn.sf = opt.get

proc resolveExpr*(pass: ResolvePass, semexpr: SemExpr) =
  case semexpr.kind
  of seFuncCall:
    pass.resolveFuncCall(semexpr)
  of seInt:
    pass.resolveByType(semexpr.typ.get)
  of seString:
    pass.resolveByType(semexpr.typ.get)
  else:
    discard

method execute*(pass: ResolvePass, ctx: SemPassContext) =
  for fn in ctx.walkFunc:
    pass.resolveFunc(fn)
  for e in ctx.walkExpr:
    pass.resolveExpr(e)
