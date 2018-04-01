
import parser, types, fexpr, scope, metadata, ctrc
import passutils

import strutils
import options
import algorithm

proc initDepend*(left, right: FExpr): Depend =
  Depend(left: left, right: right)

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
  for scopevalue in scope.scopevalues.reversed:
    if scopevalue.ctrc.exdestroyed:
      continue
    if scopevalue.ctrc.destroyed:
      if scope.getFunc(procname(name("destruct"), @[scopevalue.typ])).isSome:
        var dcall = scopevalue.span.quoteFExpr("destruct(`embed)", [scopevalue])
        scope.rootPass(dcall)
        body.addSon(dcall)
  if isret:
    body.addSon(fident(body.span, tmpsym))
    scope.rootPass(body[^1])
    body[^1].ctrc.isret = true

proc genLiftName*(rootPass: PassProcType, scope: Scope, cnt: var int, args: FExpr, body: FExpr, resulttype: var seq[Symbol], fexpr: FExpr): int =
  if fexpr.isInfixFuncCall:
    if fexpr[1].symbol.kind == symbolArg:
      fexpr.ctrc.argcnt = some(fexpr[1].symbol.argpos)
      return fexpr.symbol.argpos
  else:
    if fexpr.symbol.kind == symbolArg:
      fexpr.ctrc.argcnt = some(fexpr.symbol.argpos)
      return fexpr.symbol.argpos
      
  let n = if fexpr.isInfixFuncCall:
            fexpr.ctrc.argcnt
          else:
            fexpr.ctrc.argcnt
  if n.isSome:
    result = n.get
  else:
    result = cnt
    resulttype.add(fexpr.typ)
    if fexpr.isInfixFuncCall:
      fexpr.ctrc.argcnt = some(result)
    else:
      fexpr.ctrc.argcnt = some(result)
    
    let argf = fident(fexpr.span, name("tmpresult" & $cnt))
    argf.ctrc = initCTRC(cnt = 0)
    argf.typ = fexpr.typ.scope.refsym(fexpr.typ)
    let argsym = fsymbol(fexpr.span, scope.refsym(scope.symbol(name(argf), symbolArg, argf)))
    argsym.symbol.wrapped.argpos = args.len
    if not scope.addDecl(name(argf), argsym.symbol):
      argf.error("redefinition $# variable. (declaration by internal)" % $argf)
    args.addSon(fexpr.span.quoteFExpr("`embed `embed", [argsym, fsymbol(fexpr.span, argf.typ)]))
    
    var f = fexpr.span.quoteFExpr("returnset(`embed, `embed)", [argsym, fexpr])
    scope.rootPass(f)
    body.addSon(f)
      
    cnt += 1
    
proc expandScopeLiftingFn*(rootPass: PassProcType, scope: Scope, fexpr: FExpr): Effect =
  fexpr.assert(fexpr.hasDefn)

  result.trackings = @[]
  result.resulttypes = @[]

  var ret: FExpr = nil
  if fexpr.defn.body.len != 0 and not fexpr.defn.body[^1].typ.isVoidType:
    ret = fexpr.defn.body[^1]
    fexpr.defn.body.delSon(fexpr.defn.body.len-1)
  
  var cnt = 0
  for depend in scope.scopedepends:
    if depend.left.isInfixFuncCall and not depend.left.ctrc.destroyed:
      if depend.right.ctrc.isret:
        continue
      discard genLiftName(rootPass, scope, cnt, fexpr.defn.args, fexpr.defn.body, result.resulttypes, depend.right)
      result.trackings.add(depend)
    elif not depend.left.ctrc.destroyed:
      if not depend.left.ctrc.isret:
        discard genLiftName(rootPass, scope, cnt, fexpr.defn.args, fexpr.defn.body, result.resulttypes, depend.left)
      if not depend.right.ctrc.isret:
        discard genLiftName(rootPass, scope, cnt, fexpr.defn.args, fexpr.defn.body, result.resulttypes, depend.right)
      result.trackings.add(depend)

  if not ret.isNil:
    # if ret.kind == fexprSymbol and ret.ctrc.argcnt.isSome:
    #   var f = ret.span.quoteFExpr("returnset(tmpresult$#, `embed)" % $ret.ctrc.argcnt.get, [ret])
    #   scope.rootPass(f)
    #   fexpr.defn.body.addSon(f)
    # else:
    #   fexpr.defn.body.addSon(ret)
    fexpr.defn.body.addSon(ret)

  # echo fexpr.defn.name, fexpr.defn.args, fexpr.defn.body

proc expandEffectedArgs*(rootPass: PassProcType, scope: Scope, body: var FExpr, args: FExpr, eff: Effect) =
  var tmpnames = newSeq[FExpr]()
  for arg in args:
    tmpnames.add(arg)
  for i, resulttype in eff.resulttypes:
    let tmpname = scope.ctx.genTmpName()
    tmpnames.add(fident(body.span, tmpname))
    var tmpvarf = args.span.quoteFExpr("var $# `embed" % $tmpname, [fsymbol(body.span, resulttype)])
    scope.rootPass(tmpvarf)
    body.addSon(tmpvarf)
    var tmpvarsym = args.span.quoteFExpr($tmpname, [])
    scope.rootPass(tmpvarsym)
    args.addSon(tmpvarsym)
  for track in eff.trackings:
    if track.left.isInfixFuncCall:
      discard
    else:
      if track.left.ctrc.isret or track.right.ctrc.isret:
        continue
      let left = tmpnames[track.left.ctrc.argcnt.get]
      let right = tmpnames[track.right.ctrc.argcnt.get]
      body.addSon(body.span.quoteFExpr("track(`embed -> `embed)", [left, right]))
  scope.rootPass(body)

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
  body.addSon(fexpr)
  body.typ = body[^1].typ
  fexpr = body

proc bodyScopeout*(rootPass: PassProcType, scope: Scope, fexpr: FExpr) =
  scope.scopeoutCTRC()
  expandDestructor(rootPass, scope, fexpr)

proc isPureEffect*(eff: Effect): bool =
  if eff.resulttypes.len != 0: return false
  if eff.trackings.len != 0: return false
  return true
  
proc fnScopeout*(rootPass: PassProcType, scope: Scope, fexpr: FExpr) =
  fexpr.assert(fexpr.hasDefn)
  scope.scopeoutCTRC()
  expandDestructor(rootPass, scope, fexpr.defn.body)
  let eff = expandScopeLiftingFn(rootPass, scope, fexpr)
  if not eff.isPureEffect:
    fexpr.effect = eff
