
import sast
import semtree, sempass
import walker

import strutils, sequtils
import options

type
  SpecialPass* = ref object of SemPass

proc newSpecialPass*(): SpecialPass =
  SpecialPass()

proc checkCallLen*(e: SemExpr, name: string, len: int) =
  if e.args.len != len:
    e.sexpr.error("$# arguments length should be $#." % [name, $len])
proc checkCallMinLen*(e: SemExpr, name: string, minlen: int) =
  if e.args.len < minlen:
    e.sexpr.error("$# arguments length should be greater than $#." % [name, $minlen])

proc assignSpecialCall*(pass: SpecialPass, e: var SemExpr) =
  case e.fn.name.idname
  of "if":
    e.checkCallLen("if", 3)
    e = SemExpr(
      sexpr: e.sexpr,
      scope: e.scope,
      typ: none(SemSym),
      kind: seIf,
      ifcond: e.args[0],
      iftrue: e.args[1],
      iffalse: e.args[2],
    )
  of "while":
    e.checkCallMinLen("while", 1)
    e = SemExpr(
      sexpr: e.sexpr,
      scope: e.scope,
      typ: some(e.scope.semsym(e.scope.semident("Void"))),
      kind: seWhile,
      whilecond: e.args[0],
      whilebody: e.args[1..^1]
    )
  of "set!":
    e.checkCallLen("set!", 2)
    e = SemExpr(
      sexpr: e.sexpr,
      scope: e.scope,
      typ: none(SemSym),
      kind: seSet,
      setplace: e.args[0],
      setvalue: e.args[1],
    )
  of "var":
    e.checkCallLen("var", 2)
    if e.args[0].kind != seIdent:
      e.args[0].sexpr.error("variable name should be ident.")
    e = SemExpr(
      sexpr: e.sexpr,
      scope: e.scope,
      typ: none(SemSym),
      kind: seVar,
      varname: e.args[0].idname,
      varvalue: e.args[1],
      vartoplevel: false
    )

method execute*(pass: SpecialPass, ctx: SemPassContext) =
  for e in ctx.walkExpr:
    if e.kind == seFuncCall:
      if e.fn.name.kind == seIdent:
        pass.assignSpecialCall(e)
