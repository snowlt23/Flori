
import fexpr_core, marking
import newpassmacro

import strutils
import options
import algorithm
import tables

proc expandDestructorBody*(scope: Scope, body: var FExpr) =
  var tmpsym = none(Name)
  if body.len != 0 and not body[^1].typ.isVoidType: # escape ret value by destrutors
    if body[^1].hasMarkEffect:
      body[^1].markeffect.moved = true
    tmpsym = some(scope.ctx.genTmpName())
    var tmpvar = body[^1].span.quoteFExpr("$# := `embed" % $tmpsym.get, [body[^1]])
    scope.rootPass(tmpvar)
    body[^1] = tmpvar
    
  for scopevalue in scope.scopevalues.reversed:
    if scopevalue.hasMarkEffect and not scopevalue.markeffect.moved:
      if scope.isDestructable(scopevalue.typ):
        var destcall = scopevalue.span.quoteFExpr("destruct(`embed)", [scopevalue])
        scope.rootPass(destcall)
        body.addSon(destcall)
    
  if tmpsym.isSome:
    body.addSon(fident(body.span, tmpsym.get))
    scope.rootPass(body[^1])

proc expandDestructor*(scope: Scope, fexpr: var FExpr) =
  if fexpr.hasInternalMark and fexpr.internalMark == internalWhile:
    scope.expandDestructor(fexpr[2])
    scope.expandDestructorBody(fexpr[2])
  elif fexpr.hasInternalMark and fexpr.internalMark == internalBlock:
    scope.expandDestructor(fexpr[1])
    scope.expandDestructorBody(fexpr[1])
  elif fexpr.hasInternalMark and fexpr.internalMark == internalDef:
    scope.expandDestructor(fexpr.defexpr.value)
  elif fexpr.hasInternalMark and fexpr.internalMark == internalSet:
    scope.expandDestructor(fexpr.setexpr.dst)
    scope.expandDestructor(fexpr.setexpr.value)
  elif fexpr.hasInternalMark and fexpr.internalMark == internalInit:
    for e in fexpr[2].mitems:
      scope.expandDestructor(e)
  elif fexpr.hasInternalMark:
    discard
  elif fexpr.kind in {fexprArray, fexprList}:
    for son in fexpr.mitems:
      scope.expandDestructor(son)
  elif fexpr.kind == fexprBlock:
    for son in fexpr.mitems:
      scope.expandDestructor(son)
  elif fexpr.isFuncCall and fexpr[0].kind == fexprSymbol and not fexpr[0].symbol.fexpr.isExpanded and fexpr[0].symbol.fexpr.hasDefn:
    if not fexpr[0].symbol.fexpr.internalPragma.nodestruct and fexpr[0].symbol.fexpr.defn.body.len != 0 and fexpr[0].symbol.fexpr.defn.body[^1].hasTyp:
      fexpr[0].symbol.fexpr.isExpanded = true
      scope.expandDestructor(fexpr[0].symbol.fexpr.defn.body)
      fexpr[0].symbol.fexpr.internalScope.expandDestructorBody(fexpr[0].symbol.fexpr.defn.body)
    
proc expandDestructorPass*(scope: Scope, fexpr: var FExpr): bool =
  scope.expandDestructor(fexpr)
  return true
