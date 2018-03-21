
import parser, types, fexpr, scope, metadata, ctrc
import passutils

import strutils
import options
    
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
      if scope.getFunc(procname(name("destruct"), @[scopevalue.typ])).isSome:
        var dcall = scopevalue.span.quoteFExpr("destruct(`embed)", [scopevalue])
        scope.rootPass(dcall)
        body.addSon(dcall)
  if isret:
    body.addSon(fident(body.span, tmpsym))
    scope.rootPass(body[^1])
    body[^1].symbol.fexpr.ctrc.isret = true
    
proc expandScopeLiftingFn*(rootPass: PassProcType, scope: Scope, fexpr: FExpr): Effect =
  fexpr.assert(fexpr.hasDefn)

  result.ctrcargs = @[]
  result.resulttypes = @[]

  var ret: FExpr = nil
  if fexpr.defn.body.len != 0 and not fexpr.defn.body[^1].typ.isVoidType:
    ret = fexpr.defn.body[^1]
    fexpr.defn.body.delSon(fexpr.defn.body.len-1)

  var cnt = 0
  for scopevalue in scope.scopevalues:
    if scopevalue.ctrc.isret:
      continue
    
    if scopevalue.ctrc.cnt != 0:
      result.resulttypes.add(scopevalue.typ)
      let argf = fexpr.span.quoteFExpr("tmpresult" & $cnt, [])
      argf.ctrc = initCTRC()
      argf.ctrc.cnt = 0
      argf.typ = scopevalue.typ.scope.refsym(scopevalue.typ)
      let argsym = fsymbol(fexpr.span, scope.refsym(scope.symbol(name(argf), symbolVar, argf)))
      if not scope.addDecl(name(argf), argsym.symbol):
        argf.error("redefinition $# variable. (declaration by internal)" % $argf)
      fexpr.defn.args.addSon(fexpr.span.quoteFExpr("`embed `embed", [argsym, fsymbol(fexpr.span, argf.typ)]))
      var f = fexpr.span.quoteFExpr("`embed = `embed", [argsym, scopevalue])
      scope.rootPass(f)
      fexpr.defn.body.addSon(f)
      cnt.inc

  if not ret.isNil:
    fexpr.defn.body.addSon(ret)
      
  for arg in fexpr.defn.args:
    let argctrc = arg[0].symbol.fexpr.ctrc
    result.ctrcargs.add(argctrc)

proc expandEffectedArgs*(rootPass: PassProcType, scope: Scope, body: var FExpr, args: FExpr, eff: Effect) =
  for i, resulttype in eff.resulttypes:
    let tmpname = scope.ctx.genTmpName()
    var tmpvarf = args.span.quoteFExpr("var $# `embed" % $tmpname, [fsymbol(body.span, resulttype)])
    scope.rootPass(tmpvarf)
    body.addSon(tmpvarf)
    var tmpvarsym = args.span.quoteFExpr($tmpname, [])
    scope.rootPass(tmpvarsym)
    args.addSon(tmpvarsym)

proc applyEffect*(args: FExpr, eff: Effect) =
  for i, ctrc in eff.ctrcargs:
    # if args[i].kind != fexprSymbol and cnt != 0:
    #   args[i].error("unsupported tracked tmp value in currently: $#" % $args[i])
    if args[i].kind == fexprSymbol:
      let origcnt = args[i].symbol.fexpr.ctrc.cnt
      args[i].symbol.fexpr.ctrc.deepCopy(ctrc)
      args[i].symbol.fexpr.ctrc.cnt = ctrc.cnt + origcnt

proc expandEffectedCall*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  let eff = fexpr[0].symbol.fexpr.effect
  let args = if fexpr.isNormalFuncCall:
               fexpr[1]
             elif fexpr.isGenericsFuncCall:
               fexpr[2]
             else:
               fexpr.error("expandEffectedCall not supported infix call in currently.")
               fseq(fexpr.span)
  var body = fblock(fexpr.span)
  expandEffectedArgs(rootPass, scope, body, args, eff)
  applyEffect(args, eff)
  body.addSon(fexpr)
  body.typ = fexpr.typ
  fexpr = body

proc bodyScopeout*(rootPass: PassProcType, scope: Scope, fexpr: FExpr) =
  scope.scopeoutCTRC()
  expandDestructor(rootPass, scope, fexpr)

proc isPureEffect*(eff: Effect): bool =
  if eff.resulttypes.len != 0: return false
  for ctrc in eff.ctrcargs:
    if ctrc.cnt != 0: return false
  return true
  
proc fnScopeout*(rootPass: PassProcType, scope: Scope, fexpr: FExpr) =
  fexpr.assert(fexpr.hasDefn)
  scope.scopeoutCTRC()
  expandDestructor(rootPass, scope, fexpr.defn.body)
  let eff = expandScopeLiftingFn(rootPass, scope, fexpr)
  if not eff.isPureEffect:
    fexpr.effect = eff
