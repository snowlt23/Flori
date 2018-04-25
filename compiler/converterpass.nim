
import fexpr_core
import newpassmacro

import strutils, sequtils
import options

proc isConvEnd(args: FExpr, convindexes: seq[int]): bool =
  for i, arg in args:
    if arg.typ.fexpr.hasConverters and convindexes[i] >= arg.typ.fexpr.converters.converters.len:
      return true
  return false

proc findMatchFn*(scope: Scope, fn: FExpr, args: FExpr, convindexes: seq[int]): Option[FExpr] =
  if isConvEnd(args, convindexes):
    return none(FExpr)
  
  let newargs = flist(args.span)
  for i, arg in args:
    if arg.typ.fexpr.hasConverters:
      let convname = arg.typ.fexpr.converters.converters[convindexes[i]].defn.name
      var conv = arg.span.quoteFExpr("`embed(`embed)", [convname, arg])
      scope.rootPass(conv)
      newargs.addSon(conv)
    else:
      newargs.addSon(args[i])
  checkArgsHastype(newargs)
  let opt = scope.getFunc(procname(name(fn), newargs.mapIt(it.typ)))
  if opt.isSome:
    return some(newargs)
  for i in 0..<convindexes.len:
    if args[i].typ.fexpr.hasConverters and convindexes[i] < args[i].typ.fexpr.converters.converters.len:
      var newconvindexes = convindexes
      newconvindexes[i] += 1
      let opt = findMatchFn(scope, fn, args, newconvindexes)
      if opt.isSome:
        return opt
  return none(FExpr)

proc converterPass*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  
  if fexpr.isNormalFuncCall and fexpr[0].kind != fexprSymbol:
    let fnident = fexpr[0]
    checkArgsHastype(fexpr[1])
    let opt = scope.findMatchFn(fnident, fexpr[1], fexpr[1].mapIt(0))
    if opt.isSome:
      fexpr = fexpr.span.quoteFExpr("`embed `embed", [fnident, opt.get])
      scope.rootPass(fexpr)
      return false
    else:
      return true
  elif fexpr.isGenericsFuncCall and fexpr[0].kind != fexprSymbol:
    let fnident = fexpr[0]
    checkArgsHastype(fexpr[2])
    let opt = scope.findMatchFn(fnident, fexpr[2], fexpr[2].mapIt(0))
    if opt.isNone:
      fexpr.error("undeclared $#($#) function." % [$fnident, fexpr[2].mapIt($it.typ).join(", ")])
    fexpr = fexpr.span.quoteFExpr("`embed `embed `embed", [fnident, fexpr[1], opt.get])
    scope.rootPass(fexpr)
    return false
  elif fexpr.isInfixFuncCall and fexpr[0].kind != fexprSymbol:
    let fnident = fexpr[0]
    let args = flist(fexpr.span, @[fexpr[1], fexpr[2]])
    checkArgsHastype(args)
    let opt = scope.findMatchFn(fnident, args, args.mapIt(0))
    if opt.isNone:
      fexpr.error("undeclared $#($#, $#) function." % [$fnident, $fexpr[1].typ, $fexpr[2].typ])
    fexpr = fexpr.span.quoteFExpr("`embed `embed `embed", [fnident, opt.get[0], opt.get[1]])
    scope.rootPass(fexpr)
    return false
  else:
    return true
