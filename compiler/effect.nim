
import parser, types, fexpr, scope, metadata, ctrc
import passutils

import strutils
import options

# at scopeout
proc inferEffect*(fexpr: FExpr): Effect =
  fexpr.assert(fexpr.hasDefn)

  result.argcnts = @[]
  for arg in fexpr.defn.args:
    let argctrc = arg[0].symbol.fexpr.ctrc
    result.argcnts.add(argctrc.refcnt)

proc scopeoutCTRC*(scope: Scope) =
  for scopevalue in scope.scopevalues:
    scopevalue.ctrc.dec

proc expandDestructor*(rootPass: PassProcType, scope: Scope, body: FExpr) =
  var tmpsym: Name
  var isret = false
  if body.len != 0 and not body[^1].typ.isVoidType:
    tmpsym = scope.ctx.genTmpName()
    var tmpvar = body[^1].span.quoteFExpr("$# := `embed" % $tmpsym, [body[^1]])
    scope.rootPass(tmpvar)
    body[^1] = tmpvar
    isret = true
  for scopevalue in scope.scopevalues:
    if scopevalue.ctrc.destroyed:
      if scope.getFunc(procname(name("destructor"), @[scopevalue.typ])).isSome:
        var dcall = scopevalue.span.quoteFExpr("destructor(`embed)", [scopevalue])
        scope.rootPass(dcall)
        body.addSon(dcall)
  if isret:
    body.addSon(fident(body.span, tmpsym))
    scope.rootPass(body[^1])

proc fnScopeout*(rootPass: PassProcType, scope: Scope, fexpr: FExpr) =
  fexpr.assert(fexpr.hasDefn)
  scope.scopeoutCTRC()
  expandDestructor(rootPass, scope, fexpr.defn.body)
  let eff = inferEffect(fexpr)
  fexpr.effect = eff

proc applyEffect*(args: FExpr, eff: Effect) =
  for i, cnt in eff.argcnts:
    if args.kind != fexprSymbol and cnt != 0:
      args[i].error("unsupported tracked tmp value in currently: $#" % $args[i])
    if args.kind == fexprSymbol:
      args[i].symbol.fexpr.ctrc.refcnt += cnt
