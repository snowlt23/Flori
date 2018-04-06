
import parser, types, fexpr, scope, metadata, ctrc, effect
import passutils

import options
import strutils, sequtils
import tables

# proc expandFExpr*(scope: Scope, fexpr: FExpr, argdecls: FExpr, args: FExpr): FExpr =
#   case fexpr.kind
#   of fexprContainer:
#     let cont = fcontainer(fexpr.span, fexpr.kind)
#     cont.metadata = fexpr.metadata
#     for son in fexpr:
#       cont.addSon(scope.expandFExpr(son, argdecls, args))
#     result = cont
#   of fexprIdent:
#     for i in 0..<argdecls.len:
#       if $fexpr == $argdecls[i][0]:
#         return args[i]
#     result.deepCopy(fexpr)
#   else:
#     result.deepCopy(fexpr)
proc replaceIdent*(fexpr: FExpr, ident: FExpr, by: FExpr): FExpr =
  if fexpr.isInfixFuncCall and $fexpr[0] == ".":
    let cont = fcontainer(fexpr.span, fexpr.kind)
    cont.metadata = fexpr.metadata
    cont.addSon(fexpr[0])
    cont.addSon(replaceIdent(fexpr[1], ident, by))
    cont.addSon(fexpr[2])
    return cont
  
  case fexpr.kind
  of fexprContainer:
    let cont = fcontainer(fexpr.span, fexpr.kind)
    cont.metadata = fexpr.metadata
    for son in fexpr:
      cont.addSon(replaceIdent(son, ident, by))
    result = cont
  of fexprIdent:
    if $fexpr == $ident:
      result = by
    else:
      result = fexpr
  else:
    result = fexpr
proc collectVarnames*(varnames: var seq[FExpr], fexpr: FExpr) =
  if fexpr.kind == fexprSeq and fexpr.len == 3 and $fexpr[0] == ":=":
    varnames.add(fexpr[1])
  elif fexpr.kind in fexprContainer:
    for son in fexpr:
      collectVarnames(varnames, son)

proc expandInlineFunc*(scope: Scope, fexpr: var FExpr) =
  let fn = fexpr[0].symbol.fexpr
  let args = if fexpr.isNormalFuncCall:
               fexpr[1]
             elif fexpr.isGenericsFuncCall:
               fexpr[2]
             else:
               fseq(fexpr.span, @[fexpr[1], fexpr[2]])
  var f: FExpr
  f.deepCopy(fn.defn.body)
  fexpr = f
  for i, arg in fn.defn.args:
    # if arg[1].symbol.kind == symbolRef:
    #   echo arg[1].symbol, ":", arg[1].symbol.scope.varsym(arg[1].symbol.wrapped)
    #   arg[1].symbol = arg[1].symbol.scope.varsym(arg[1].symbol.wrapped)
    fexpr = fexpr.replaceIdent(arg[0], args[i])
    
  var varnames = newSeq[FExpr]()
  collectVarnames(varnames, fexpr)
  for varname in varnames:
    fexpr = fexpr.replaceIdent(varname, fident(varname.span, scope.ctx.genTmpName()))
  # echo fexpr
