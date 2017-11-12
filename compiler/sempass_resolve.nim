
#
# Symbol resolve, Type inference, Generics
#

import ast
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
  if sym.name.kind == seIdent:
    let opt = sym.scope.getType(TypeIdent(name: sym.name.idname))
    if opt.isNone:
      sym.name.fexpr.error("undeclared $# type." % sym.name.idname)
    sym.kind = symSemType
    sym.st = opt.get
  else:
    sym.name.fexpr.error("$# is not type expression." % $sym.name.fexpr)

proc resolveFuncType*(pass: ResolvePass, ft: FuncType): bool =
  for argtype in ft.argtypes:
    if argtype.name.kind == seIdent:
      pass.resolveByType(argtype)
    else:
      return false
  pass.resolveByType(ft.returntype)
  return true

proc resolveFunc*(pass: ResolvePass, semfunc: SemFunc) =
  case semfunc.kind
  of sfCFunc:
    if not pass.resolveFuncType(semfunc.cfunctype):
      semfunc.fexpr.error("unresolve types of $# C func." % $semfunc.cfuncname)
  of sfFunc:
    if not pass.resolveFuncType(semfunc.functype):
      semfunc.fexpr.error("unresolve types of $# func." % $semfunc.funcname)
    for i in 0..<semfunc.funcargs.len:
      let argtype = semfunc.functype.argtypes[i]
      let funcarg = semfunc.funcargs[i]
      funcarg.typ = some(argtype)
      if funcarg.kind != seIdent:
        funcarg.fexpr.error("function argument should be ident.")
      let status = semfunc.scope.addVar(VarIdent(name: funcarg.idname), funcarg)
      if not status:
        funcarg.fexpr.error("redefinition $# variable" % funcarg.idname)

proc resolveFuncCall*(pass: ResolvePass, semexpr: SemExpr) =
  for arg in semexpr.args:
    pass.resolveExpr(arg)
  let argtypes = semexpr.args.mapIt(it.typ.get)
  let pid = ProcIdentDecl(name: semexpr.fn.name.idname, args: argtypes)
  let opt = semexpr.fn.scope.getProc(pid)
  if opt.isNone:
    semexpr.fexpr.error("undeclared $# func." % semexpr.fn.name.idname)
  semexpr.fn.kind = symSemFunc
  semexpr.fn.sf = opt.get
  semexpr.typ = some(semexpr.fn.sf.getReturnType())

proc resolveVar*(pass: ResolvePass, semexpr: SemExpr) =
  if not semexpr.vartoplevel:
    if not semexpr.scope.addVar(VarIdent(name: semexpr.varname), semexpr):
      semexpr.fexpr.error("redefinition $# variable." % semexpr.varname)
  pass.resolveExpr(semexpr.varvalue)
  semexpr.typ = semexpr.varvalue.typ

proc resolveIf*(pass: ResolvePass, semexpr: SemExpr) =
  pass.resolveExpr(semexpr.ifcond)
  if not semexpr.ifcond.typ.get.isBoolType():
    semexpr.ifcond.fexpr.error("if cond expression should be Bool type.")
  pass.resolveExpr(semexpr.iftrue)
  if semexpr.iffalse.isSome:
    pass.resolveExpr(semexpr.iffalse.get)

    if semexpr.iftrue.typ.get != semexpr.iffalse.get.typ.get:
      semexpr.fexpr.error("if expression not equal return type: $# != $#" % [$semexpr.iftrue.typ.get, $semexpr.iffalse.get.typ.get])
    semexpr.typ = semexpr.iftrue.typ
  else:
    pass.resolveByType(semexpr.typ.get)

proc resolveWhile*(pass: ResolvePass, semexpr: SemExpr) =
  pass.resolveExpr(semexpr.whilecond)
  if not semexpr.whilecond.typ.get.isBoolType():
    semexpr.whilecond.fexpr.error("while cond expression should be Bool type.")
  if semexpr.whilebody.kind != seBlock:
    semexpr.whilebody.fexpr.error("while body should be FBlock.")
  for b in semexpr.whilebody.body:
    pass.resolveExpr(b)
  let opt = semexpr.scope.getType(TypeIdent(name: "Void"))
  if opt.isNone:
    semexpr.fexpr.error("undeclared Void type.")
  pass.resolveByType(semexpr.typ.get)

proc resolveExpr*(pass: ResolvePass, semexpr: SemExpr) =
  case semexpr.kind
  of seIdent:
    let v = semexpr.scope.getVar(VarIdent(name: semexpr.idname))
    if v.isNone:
      semexpr.fexpr.error("undeclared $# ident." % semexpr.idname)
    semexpr.typ = v.get.typ
  of seFuncCall:
    pass.resolveFuncCall(semexpr)
  of seArray, seList, seBlock:
    for se in semexpr.body:
      pass.resolveExpr(se)
    if semexpr.body.len == 1:
      semexpr.typ = semexpr.body[0].typ
    else:
      pass.resolveByType(semexpr.typ.get)
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
    semexpr.fexpr.error("couldn't resolve $# expression." % $semexpr.kind)

method execute*(pass: ResolvePass, ctx: SemPassContext) =
  for fn in ctx.walkFunc:
    pass.resolveFunc(fn)
  for e in ctx.walkExpr:
    pass.resolveExpr(e)
