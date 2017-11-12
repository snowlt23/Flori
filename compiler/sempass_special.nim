
import ast
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
    e.fexpr.error("$# arguments length should be $#." % [name, $len])
proc checkCallMinLen*(e: SemExpr, name: string, minlen: int) =
  if e.args.len < minlen:
    e.fexpr.error("$# arguments length should be greater than $#." % [name, $minlen])

proc assignSpecialCall*(pass: SpecialPass, e: var SemExpr) =
  if e.fn.name.kind != seIdent:
    return

  case e.fn.name.idname
  of "if":
    e.checkCallMinLen("if", 2)
    if e.args.len == 2:
      e = SemExpr(
        fexpr: e.fexpr,
        scope: e.scope,
        typ: some(e.scope.semsym(e.scope.semident("Void"))),
        kind: seIf,
        ifcond: e.args[0],
        iftrue: e.args[1],
        iffalse: none(SemExpr),
      )
    else:
      if e.args[2].kind != seIdent or e.args[2].idname != "else":
        e.args[2].fexpr.error("if branch should be else.")
      e = SemExpr(
        fexpr: e.fexpr,
        scope: e.scope,
        typ: none(SemSym),
        kind: seIf,
        ifcond: e.args[0],
        iftrue: e.args[1],
        iffalse: some(e.args[3]),
      )
  of "while":
    e.checkCallLen("while", 2)
    e = SemExpr(
      fexpr: e.fexpr,
      scope: e.scope,
      typ: some(e.scope.semsym(e.scope.semident("Void"))),
      kind: seWhile,
      whilecond: e.args[0],
      whilebody: e.args[1]
    )
  # of "set!":
  #   e.checkCallLen("set!", 2)
  #   e = SemExpr(
  #     fexpr: e.fexpr,
  #     scope: e.scope,
  #     typ: none(SemSym),
  #     kind: seSet,
  #     setplace: e.args[0],
  #     setvalue: e.args[1],
  #   )
  of "var":
    e.checkCallLen("def", 2)
    if e.args[0].kind != seIdent:
      e.args[0].fexpr.error("variable name should be ident.")
    if e.args[1].kind != seIdent or e.args[1].idname != "=":
      e.args[1].fexpr.error("variable require '= symbol.")
    e = SemExpr(
      fexpr: e.fexpr,
      scope: e.scope,
      typ: none(SemSym),
      kind: seVar,
      varname: e.args[0].idname,
      varvalue: e.args[2],
      vartoplevel: false
    )

method execute*(pass: SpecialPass, ctx: SemPassContext) =
  for e in ctx.walkExpr:
    if e.kind == seFuncCall:
      if e.fn.name.kind == seIdent:
        pass.assignSpecialCall(e)
