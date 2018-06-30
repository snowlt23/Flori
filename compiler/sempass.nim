
import fcore
import passmacro

import options
import strutils, sequtils
import tables

proc internalPass*(scope: FScope, fexpr: var FExpr): bool =
  # echo fexpr
  case fexpr.kind
  of fexprIf:
    let internalopt = scope.getWords("if")
    if internalopt.len != 0 and internalopt[0].internalproc.isSome:
      (internalopt[0].internalproc.get)(scope, fexpr)
      return false
    else:
      return true
  of fexprCalls:
    let internalopt = scope.getWords($fexpr.call)
    if internalopt.len != 0 and internalopt[0].internalproc.isSome:
      (internalopt[0].internalproc.get)(scope, fexpr)
      return false
    else:
      return true
  else:
    return true

proc blockPass*(scope: FScope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  if fexpr.kind == fexprBlock:
    for son in fexpr.sons.mitems:
      scope.rootPass(son)
    fexpr.typ = some(fexpr.sons[fexpr.sons.len-1].gettype)
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
      let words = scope.getWords($fexpr.call).filterWords(fexpr.args.len)
      if words.len == 0:
        fexpr.error("undeclared $# word" % $fexpr.call)
      let calltyp = unionsym(words.mapIt(it.getReturnType()))
      fexpr.call = fsymbol(fexpr.call.span, unionsym(words.mapIt(it.sym)))
      fexpr.typ = some(calltyp)
      for i, arg in fexpr.args.mpairs:
        scope.rootPass(arg)
        let argtyp = argumentUnion(words, i)
        if argtyp.isNone:
          continue
        if arg.typ.isSome and arg.typ.get.kind == symbolLink:
          arg.typ.get.wrapped = argtyp.get
        elif arg.typ.isSome:
          if not argtyp.get.match(arg.typ.get):
            arg.error("type mismatch $#:$# in $#" % [$argtyp, $arg.typ.get, $fexpr])
        else:
          arg.typ = some(argtyp.get)

  return true

proc finalPass*(scope: FScope, fexpr: var FExpr): bool =
  return true

definePass processSemPass, rootPass, (FScope, var FExpr):
  internalPass
  blockPass
  # expandMacro
  symbolResolve
  typeInfer
  callResolve
  # expandDefnPass
  finalPass
