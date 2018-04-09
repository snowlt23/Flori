
import fexpr_core

import strutils
import options
import algorithm

proc expandDestructor*(rootPass: PassProcType, scope: Scope, body: FExpr) =
  var tmpsym = none(Name)
  if body.len != 0 and not body[^1].typ.isVoidType: # escape ret value by destrutors
    tmpsym = some(scope.ctx.genTmpName())
    var tmpvar = body[^1].span.quoteFExpr("$# := `embed" % $tmpsym.get, [body[^1]])
    scope.rootPass(tmpvar)
    body[^1] = tmpvar
    
  for scopevalue in scope.scopevalues.reversed:
    if scopevalue.marking.owned:
      if scope.isDestructable(scopevalue.typ):
        var destcall = scopevalue.span.quoteFExpr("destruct(`embed)", [scopevalue])
        scope.rootPass(destcall)
        body.addSon(destcall)
    
  if tmpsym.isSome:
    body.addSon(fident(body.span, tmpsym.get))
    scope.rootPass(body[^1])

  # echo body
