
import fexpr_core
import newpassmacro

import sequtils
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
