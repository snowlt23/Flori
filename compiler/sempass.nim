
import fcore
import passmacro

import options
import strutils, sequtils
import tables

proc internalPass*(scope: FScope, fexpr: var FExpr): bool =
  # echo fexpr
  case fexpr.kind
  of fexprCalls:
    let internalopt = scope.getFunc(procname($fexpr.call, @[]))
    if internalopt.isSome and internalopt.get.internalproc.isSome:
      (internalopt.get.internalproc.get)(scope, fexpr)
      return false
    else:
      return true
  else:
    return true

proc symbolResolve*(scope: FScope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  case fexpr.kind
  of fexprIdent:
    let opt = scope.getDecl($fexpr)
    if opt.isSome:
      fexpr = fsymbol(fexpr.span, opt.get)
    elif scope.word.isSome and scope.word.get.internal.obj.argnames.isNone:
      # argument inference
      let sym = scope.word.get.scope.get.symbol(fexpr.idname, symbolDef, fexpr)
      let typ = linksym(undeftypeSymbol)
      sym.fexpr.typ = some(typ)
      scope.word.get.scope.get.addDecl(fexpr.idname, sym)
      scope.word.get.internal.obj.inferargnames.add(sym)
      scope.word.get.internal.obj.inferargtypes.add(typ)
      fexpr = fsymbol(fexpr.span, sym)
    else:
      fexpr.error("undeclared $# ident." % $fexpr)
    return true
  else:
    return true

proc typeInfer*(scope: FScope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  case fexpr.kind
  of fexprIdent:
    fexpr.error("unresolved $# ident by symbolResolve pass." % $fexpr)
  of fexprSymbol:
    if fexpr.symbol.fexpr.typ.isSome:
      fexpr.typ = some(fexpr.symbol.fexpr.typ.get)
    elif fexpr.symbol.kind in {symbolType, symbolTypeGenerics}:
      fexpr.typ = some(fexpr.symbol)
    else:
      fexpr.error("$# hasn't type." % $fexpr)
    return true
  of fexprIntLit:
    fexpr.typ = some(intlittypeSymbol)
    return true
  of fexprFloatLit:
    fexpr.typ = some(floatlittypeSymbol)
    return true
  of fexprStrLit:
    fexpr.typ = some(strlittypeSymbol)
    return true
  else:
    return true

proc callResolve*(scope: FScope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  if fexpr.kind in fexprCalls:
    if fexpr.call.kind == fexprIdent:
      let words = scope.getWords($fexpr.call).filterIt(it.argtypes.len == fexpr.args.len)
      if words.len == 0:
        fexpr.error("undeclared $# function" % $fexpr.call)
      let calltyp = unionsym(words.mapIt(it.getReturnType()))
      fexpr.call = fsymbol(fexpr.call.span, unionsym(words.mapIt(it.sym)))
      fexpr.typ = some(calltyp)
      for i, arg in fexpr.args.mpairs:
        scope.rootPass(arg)
        let argtyp = unionsym(words.mapIt(it.argtypes[i]))
        if arg.typ.isSome and arg.typ.get.kind == symbolLink:
          arg.typ.get.wrapped = argtyp
        elif arg.typ.isSome:
          if not argtyp.match(arg.typ.get):
            arg.error("type mismatch $#:$# in $#" % [$argtyp, $arg.typ.get, $fexpr])
        else:
          arg.typ = some(argtyp)

  return true

proc finalPass*(scope: FScope, fexpr: var FExpr): bool =
  return true

definePass processSemPass, rootPass, (FScope, var FExpr):
  internalPass
  # expandMacro
  symbolResolve
  typeInfer
  callResolve
  # expandDefnPass
  finalPass
