
import fexpr_core, marking
import newpassmacro

import strutils
import options
import algorithm
import tables

proc expandDestructor*(scope: Scope, body: var FExpr) =
  var tmpsym = none(Name)
  if body.len != 0 and not body[^1].typ.isVoidType: # escape ret value by destrutors
    if body[^1].hasMarking:
      returnFrom(body[^1].marking)
    else:
      body[^1].marking = newMarking(scope, body[^1].typ)
      returnFrom(body[^1].marking)
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

proc expandDestructorPass*(scope: Scope, fexpr: var FExpr): bool =
  if fexpr.hasInternalMark:
    if fexpr.internalMark == internalWhile:
      fexpr.internalScope.expandDestructor(fexpr[2])
    elif fexpr.internalMark == internalBlock:
      fexpr.internalScope.expandDestructor(fexpr[1])
    elif fexpr.internalMark == internalDefn:
      if not fexpr.internalPragma.nodestruct and not fexpr.internalPragma.inline and not fexpr.defn.isGenerics:
        fexpr.internalScope.expandDestructor(fexpr.defn.body)

  return true
