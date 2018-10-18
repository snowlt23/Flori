
import fexpr_core, expand_templates
import passmacro

import options
import strutils, sequtils
import tables

proc isMatchMacro*(scope: Scope, args: var seq[FExpr], pd: ProcDecl): Option[seq[Matched]] =
  var s = newSeq[Matched]()
  if args.len != pd.argtypes.len: return none(seq[Matched])
  for i in 0..<args.len:
    if $pd.argtypes[i].name == "fexpr":
      continue
    elif $pd.argtypes[i].name == "fseq":
      if args[i].kind != fexprSeq:
        return none(seq[Matched])
    elif $pd.argtypes[i].name == "farray":
      if args[i].kind != fexprArray:
        return none(seq[Matched])
    elif $pd.argtypes[i].name == "flist":
      if args[i].kind != fexprList:
        return none(seq[Matched])
    elif $pd.argtypes[i].name == "fblock":
      if args[i].kind != fexprBlock:
        return none(seq[Matched])
    elif $pd.argtypes[i].name == "fident":
      if args[i].kind != fexprIdent:
        return none(seq[Matched])
    elif $pd.argtypes[i].name == "fsymbol":
      if args[i].kind != fexprSymbol:
        return none(seq[Matched])
    elif $pd.argtypes[i].name == "fintlit":
      if args[i].kind != fexprIntLit:
        return none(seq[Matched])
    elif $pd.argtypes[i].name == "fstrlit":
      if args[i].kind != fexprStrLit:
        return none(seq[Matched])
    elif $pd.argtypes[i].name == "texpr":
      scope.rootPass(args[i])
      let opt = args[i].metadata.typ.match(pd.argtypes[i].types[0])
      if not opt.isMatch:
        return none(seq[Matched])
      s.add(opt)
      continue
    elif $pd.argtypes[i].name == "tarray":
      scope.rootPass(args[i])
      if args[i].kind != fexprArray:
        return none(seq[Matched])
    elif $pd.argtypes[i].name == "tseq":
      scope.rootPass(args[i])
      if args[i].kind != fexprSeq:
        return none(seq[Matched])
    elif $pd.argtypes[i].name == "tarray":
      scope.rootPass(args[i])
      if args[i].kind != fexprArray:
        return none(seq[Matched])
    elif $pd.argtypes[i].name == "tlist":
      scope.rootPass(args[i])
      if args[i].kind != fexprList:
        return none(seq[Matched])
    elif $pd.argtypes[i].name == "tblock":
      scope.rootPass(args[i])
      if args[i].kind != fexprBlock:
        return none(seq[Matched])
    elif $pd.argtypes[i].name == "tident":
      scope.rootPass(args[i])
      if args[i].kind != fexprIdent:
        return none(seq[Matched])
    elif $pd.argtypes[i].name == "tsymbol":
      scope.rootPass(args[i])
      if args[i].kind != fexprSymbol:
        return none(seq[Matched])
    elif $pd.argtypes[i].name == "tintlit":
      scope.rootPass(args[i])
      if args[i].kind != fexprIntLit:
        return none(seq[Matched])
    elif $pd.argtypes[i].name == "tstrlit":
      scope.rootPass(args[i])
      if args[i].kind != fexprStrLit:
        return none(seq[Matched])
    else:
      args[i].error("macro argument type should be ^fexpr or ^texpr[^t]")
    s.add(Matched(kind: matchType))
  return some(s)
proc matchMacro*(scope: Scope, curscope: Scope, n: FExpr, args: var seq[FExpr], issyntax: bool, importscope = true): Option[(ProcDecl, seq[Matched])] =
  if n.kind notin fexprNames:
    return none((ProcDecl, seq[Matched]))
  
  let opt = scope.procdecls.find($n)
  if opt.isSome:
    let group = opt.get
    for pd in group.decls:
      if (not issyntax) and pd.isMacro:
        let opt = isMatchMacro(curscope, args, pd)
        if opt.isSome:
          return some((pd, opt.get))
      elif issyntax and pd.sym.fexpr.metadata.isSyntax:
        let opt = isMatchMacro(curscope, args, pd)
        if opt.isSome:
          return some((pd, opt.get))

  if importscope:
    for s in scope.imports:
      let match = matchMacro(s.value, curscope, n, args, issyntax, importscope = false)
      if match.isSome:
        return match

  if not scope.isTop:
    return scope.parent.matchMacro(curscope, n, args, issyntax, importscope)
  else:
    return none((ProcDecl, seq[Matched]))

proc isFExprName*(name: string): bool =
  case name
  of "fexpr", "fseq", "farray", "flist", "fblock", "fstrlit", "fintlit", "fident", "fsymbol":
    true
  else:
    false
proc isTExprName*(name: string): bool =
  case name
  of "texpr", "tseq", "tarray", "tlist", "tblock", "tstrlit", "tintlit", "tident", "tsymbol":
    true
  else:
    false
    
proc getMacroArgs*(scope: Scope, pd: ProcDecl, args: seq[FExpr]): seq[Symbol] =
  result = @[]
  for i in 0..<args.len:
    if ($pd.argtypes[i].name).isFExprName:
      let opt = scope.getDecl($pd.argtypes[i].name)
      if opt.isNone:
        args[i].error("undeclared $# type." % $pd.argtypes[i].name)
      result.add(opt.get)
    elif ($pd.argtypes[i].name) == "texpr":
      let opt = scope.getDecl("texpr")
      if opt.isNone:
        args[i].error("undeclared texpr type.")
      let sym = symcopy(opt.get)
      sym.types = iarray(@[args[i].metadata.typ])
      result.add(sym)
    elif ($pd.argtypes[i].name).isTExprName:
      let opt = scope.getDecl($pd.argtypes[i].name)
      if opt.isNone:
        args[i].error("undeclared $# type." % $pd.argtypes[i].name)
      result.add(opt.get)

proc expandMacro*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  if fexpr.isNormalFuncCall:
    scope.expandBy(fexpr.span):
      var args = toSeq(fexpr[1].items)
      let opt = matchMacro(scope, scope, fexpr[0], args, false)
      let fargs = flist(fexpr[1].span, args)
      if opt.isSome:
        let (pd, convs) = opt.get # converter at macroexpand
        if not pd.sym.fexpr.fnGenerics.isSpecTypes:
          fexpr[0] = expandMacrofn(scope, pd.sym.obj.fexpr, scope.getMacroArgs(pd, args))
          var expanded = (fexpr[0].symbol.macroproc.call)(fargs)
          if fexpr.metadata.isToplevel:
            expanded.metadata.isToplevel = true
          scope.rootPass(expanded)
          fexpr = expanded
        else:
          var expanded = (pd.macroproc.call)(fargs)
          if fexpr.metadata.isToplevel:
            expanded.metadata.isToplevel = true
          scope.rootPass(expanded)
          fexpr = expanded
        return false
      
  if fexpr.kind == fexprSeq:
    scope.expandBy(fexpr.span):
      var args = toSeq(fexpr.items)[1..^1]
      let fargs = flist(fexpr.span, args)
      let opt = matchMacro(scope, scope, fexpr[0], args, false)
      if opt.isSome:
        let (pd, convs) = opt.get # converter at macroexpand
        if not pd.sym.fexpr.fnGenerics.isSpecTypes:
          fexpr[0] = expandMacrofn(scope, pd.sym.obj.fexpr, scope.getMacroArgs(pd, args))
          var expanded = (fexpr[0].symbol.macroproc.call)(fargs)
          if fexpr.metadata.isToplevel:
            expanded.metadata.isToplevel = true
          scope.rootPass(expanded)
          fexpr = expanded
        else:
          var expanded = (pd.macroproc.call)(fargs)
          if fexpr.metadata.isToplevel:
            expanded.metadata.isToplevel = true
          scope.rootPass(expanded)
          fexpr = expanded
        return false
    
  return true
