
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
  let opt = sym.scope.getType(TypeIdent(name: sym.name.idname))
  if opt.isNone:
    sym.name.sexpr.error("undeclared $# type." % sym.name.idname)
  sym.kind = symSemType
  sym.st = opt.get

proc resolveFuncType*(pass: ResolvePass, ft: FuncType): bool =
  for argtype in ft.argtypes:
    if argtype.name.kind == seIdent:
      pass.resolveByType(argtype)
    elif argtype.name.kind == seAttr: # generics
      discard
    elif argtype.name.kind == seFuncCall: # type generics
      discard
    else:
      return false
  pass.resolveByType(ft.returntype)
  return true

proc resolveFunc*(pass: ResolvePass, semfunc: SemFunc) =
  case semfunc.kind
  of sfCFunc:
    if not pass.resolveFuncType(semfunc.cfunctype):
      semfunc.sexpr.error("unresolve types of $# C func." % $semfunc.cfuncname)
  of sfFunc:
    if not pass.resolveFuncType(semfunc.functype):
      semfunc.sexpr.error("unresolve types of $# func." % $semfunc.funcname)
    for i in 0..<semfunc.funcargs.len:
      let argtype = semfunc.functype.argtypes[i]
      let funcarg = semfunc.funcargs[i]
      funcarg.typ = some(argtype)
      if funcarg.kind != seIdent:
        funcarg.sexpr.error("function argument should be ident.")
      let status = semfunc.scope.addVar(VarIdent(name: funcarg.idname), funcarg)
      if not status:
        funcarg.sexpr.error("redefinition $# variable" % funcarg.idname)

proc resolveFuncCall*(pass: ResolvePass, semexpr: SemExpr) =
  for arg in semexpr.args:
    pass.resolveExpr(arg)
  let argtypes = semexpr.args.mapIt(it.typ.get)
  let pid = ProcIdentDecl(name: semexpr.fn.name.idname, args: argtypes)
  let opt = semexpr.fn.scope.getProc(pid)
  if opt.isNone:
    semexpr.sexpr.error("undeclared $# func." % semexpr.fn.name.idname)
  semexpr.fn.kind = symSemFunc
  semexpr.fn.sf = opt.get
  semexpr.typ = some(semexpr.fn.sf.getReturnType())

proc resolveVar*(pass: ResolvePass, semexpr: SemExpr) =
  if not semexpr.vartoplevel:
    if not semexpr.scope.addVar(VarIdent(name: semexpr.varname), semexpr):
      semexpr.sexpr.error("redefinition $# variable." % semexpr.varname)
  pass.resolveExpr(semexpr.varvalue)
  semexpr.typ = semexpr.varvalue.typ

proc resolveIf*(pass: ResolvePass, semexpr: SemExpr) =
  pass.resolveExpr(semexpr.ifcond)
  if not semexpr.ifcond.typ.get.isBoolType():
    semexpr.ifcond.sexpr.error("if cond expression should be Bool type.")
  pass.resolveExpr(semexpr.iftrue)
  pass.resolveExpr(semexpr.iffalse)
  if semexpr.iftrue.typ.get != semexpr.iffalse.typ.get:
    semexpr.sexpr.error("if expression not equal return type: $# != $#" % [$semexpr.iftrue.typ.get, $semexpr.iffalse.typ.get])
  semexpr.typ = semexpr.iftrue.typ

proc resolveWhile*(pass: ResolvePass, semexpr: SemExpr) =
  pass.resolveExpr(semexpr.whilecond)
  if not semexpr.whilecond.typ.get.isBoolType():
    semexpr.whilecond.sexpr.error("while cond expression should be Bool type.")
  for b in semexpr.whilebody:
    pass.resolveExpr(b)
  let opt = semexpr.scope.getType(TypeIdent(name: "Void"))
  if opt.isNone:
    semexpr.sexpr.error("undeclared Void type.")
  pass.resolveByType(semexpr.typ.get)

proc resolveExpr*(pass: ResolvePass, semexpr: SemExpr) =
  case semexpr.kind
  of seIdent:
    let v = semexpr.scope.getVar(VarIdent(name: semexpr.idname))
    if v.isNone:
      semexpr.sexpr.error("undeclared $# ident." % semexpr.idname)
    semexpr.typ = v.get.typ
  of seFuncCall:
    pass.resolveFuncCall(semexpr)
  of seVar:
    pass.resolveVar(semexpr)
  of seIf:
    pass.resolveIf(semexpr)
  of seWhile:
    pass.resolveWhile(semexpr)
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
