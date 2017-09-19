
import sast
import semtree
import sempass

import tables

proc evalSExpr*(scope: SemScope, sexpr: SExpr) =
  discard

proc evalModuleSExpr*(ctx: SemPassContext, modulename: string, topsexprs: seq[SExpr]) =
  let scope = newSemScope()
  for sexpr in topsexprs:
    scope.evalSExpr(sexpr)
  ctx.modules[ScopeIdent(name: modulename)] = scope
