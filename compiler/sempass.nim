
import fcore
import passmacro

import options
import strutils, sequtils
import tables

proc internalPass*(scope: FScope, fexpr: var FExpr): bool =
  case fexpr.kind
  of fexprIdent:
    let internalopt = scope.getWords($fexpr)
    if internalopt.len != 0 and internalopt[0].internalproc.isSome:
      (internalopt[0].internalproc.get)(scope, fexpr)
      return false
    else:
      return true
  of fexprIf:
    let internalopt = scope.getWords("if")
    if internalopt.len != 0 and internalopt[0].internalproc.isSome:
      (internalopt[0].internalproc.get)(scope, fexpr)
      return false
    else:
      return true
  of fexprWhile:
    let internalopt = scope.getWords("while")
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

proc instantiateWord*(scope: FScope, fexpr: FExpr, argtypes: seq[Symbol]): FExpr {.discardable.} =
  result = fexpr.span.quoteFExpr("`embed => `embed `embed", [fexpr.args[0], fcall(fexpr.span, fident(fexpr.span, "$typed"), argtypes.mapIt(fsymbol(fexpr.span, it))), fexpr.args[1]])
  scope.rootPass(result)

proc callResolve*(scope: FScope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  if fexpr.kind in fexprCalls:
    if fexpr.call.kind == fexprIdent:
      let words = scope.getWords($fexpr.call).filterWords(fexpr.args.len)
      if words.len == 0:
        fexpr.error("undeclared $# word" % $fexpr.call)
      var argtypes = newSeq[Symbol]()
      for i, arg in fexpr.args.mpairs:
        scope.rootPass(arg)
        argtypes.add(arg.gettype)
        var argtyp = argumentUnion(words, i)
        if argtyp.isNone:
          continue
        let opt1 = argtyp.linkinfer(arg.typ.get)
        if opt1.isSome:
          arg.error(opt1.get & " in " & $fexpr)
        let opt2 = arg.typ.linkinfer(argtyp.get)
        if opt2.isSome:
          arg.error(opt2.get & " in " & $fexpr)
      let specwords = words.filterWords(argtypes)
      let calltyp = unionsym(specwords.mapIt(it.getReturnType()))
      fexpr.call = fsymbol(fexpr.call.span, unionsym(specwords.mapIt(it.sym)))
      fexpr.typ = some(calltyp)
      if fexpr.call.symbol.kind != symbolUnion and fexpr.args.mapIt(it.gettype).isSpecTypes and fexpr.call.symbol.fexpr.internal.obj.argtypes.isSome and not toSeq(fexpr.call.symbol.fexpr.internal.obj.argtypes.get.items).isSpecTypes:
        let instance = fexpr.call.symbol.scope.instantiateWord(fexpr.call.symbol.fexpr, fexpr.args.mapIt(it.gettype))
        fexpr.typ = some(instance.internal.obj.returntype)
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
