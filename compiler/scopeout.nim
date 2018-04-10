
import fexpr_core, marking

import strutils
import options
import algorithm
import tables

proc expandDestructor*(rootPass: PassProcType, scope: Scope, body: FExpr) =
  var tmpsym = none(Name)
  if body.len != 0 and not body[^1].typ.isVoidType: # escape ret value by destrutors
    if body[^1].hasMarking:
      body[^1].marking.owned = false
    else:
      body[^1].marking = newMarking(body[^1].typ)
      body[^1].marking.owned = false
    tmpsym = some(scope.ctx.genTmpName())
    var tmpvar = body[^1].span.quoteFExpr("$# := `embed" % $tmpsym.get, [body[^1]])
    scope.rootPass(tmpvar)
    body[^1] = tmpvar
    
  for scopevalue in scope.scopevalues.reversed:
    if scopevalue.marking.owned:
      for fieldname, fieldmark in scopevalue.marking.fieldbody:
        var fieldaccess = scopevalue.span.quoteFExpr("`embed . `embed", [scopevalue, fident(scopevalue.span, fieldname)])
        scope.rootPass(fieldaccess)
        if fieldaccess.hasMarking and fieldaccess.marking.owned and scope.isDestructable(fieldaccess.typ):
          var destcall = scopevalue.span.quoteFExpr("destruct(`embed)", [fieldaccess])
          scope.rootPass(destcall)
          body.addSon(destcall)
      if scope.isDestructable(scopevalue.typ):
        var destcall = scopevalue.span.quoteFExpr("destruct(`embed)", [scopevalue])
        scope.rootPass(destcall)
        body.addSon(destcall)
    
  if tmpsym.isSome:
    body.addSon(fident(body.span, tmpsym.get))
    scope.rootPass(body[^1])
