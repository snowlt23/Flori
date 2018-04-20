
import fexpr_core

import options
import strutils
import tables

proc isMatchMacro*(rootPass: PassProcType, scope: Scope, args: FExpr, pd: ProcDecl): bool =
  if args.len != pd.argtypes.len: return false
  for i in 0..<args.len:
    if $pd.argtypes[i].name == "FExpr":
      continue
    elif $pd.argtypes[i].name == "FSeq":
      if args[i].kind != fexprSeq:
        return false
    elif $pd.argtypes[i].name == "FArray":
      if args[i].kind != fexprArray:
        return false
    elif $pd.argtypes[i].name == "FList":
      if args[i].kind != fexprList:
        return false
    elif $pd.argtypes[i].name == "FBlock":
      if args[i].kind != fexprBlock:
        return false
    elif $pd.argtypes[i].name == "FIdent":
      if args[i].kind != fexprIdent:
        return false
    elif $pd.argtypes[i].name == "FSymbol":
      if args[i].kind != fexprSymbol:
        return false
    elif $pd.argtypes[i].name == "FIntLit":
      if args[i].kind != fexprIntLit:
        return false
    elif $pd.argtypes[i].name == "FStrLit":
      if args[i].kind != fexprStrLit:
        return false
    elif $pd.argtypes[i].name == "TExpr":
      scope.rootPass(args[i])
      if not args[i].typ.match(pd.argtypes[i].types[0]):
        return false
    else:
      args[i].error("macro argument type should be FExpr or TExpr[T]")
  return true
proc matchMacro*(rootPass: PassProcType, scope: Scope, curscope: Scope, n: FExpr, args: FExpr, issyntax: bool, importscope = true): Option[ProcDecl] =
  if not scope.procdecls.hasKey(name(n)):
    if importscope:
      for scopename, s in scope.importscopes:
        let match = matchMacro(rootPass, s, curscope, n, args, issyntax, importscope = false)
        if match.isSome:
          return match
      return none(ProcDecl)
    else:
      return none(ProcDecl)

  for pd in scope.procdecls[name(n)].decls:
    if (not issyntax) and pd.isMacro and isMatchMacro(rootPass, curscope, args, pd):
      return some(pd)
    elif issyntax and pd.fexpr.internalPragma.isSyntax and isMatchMacro(rootPass, curscope, args, pd):
      return some(pd)

  if importscope:
    for scopename, s in scope.importscopes:
      let match = matchMacro(rootPass, s, curscope, n, args, issyntax, importscope = false)
      if match.isSome:
        return match
    return none(ProcDecl)
  else:
    return none(ProcDecl)

proc isFExprName*(name: Name): bool =
  case $name
  of "FExpr", "FSeq", "FArray", "FList", "FBlock", "FStrLit", "FIntLit", "FIdent", "FSymbol":
    true
  else:
    false
    
proc getMacroArgs*(scope: Scope, pd: ProcDecl, args: FExpr): seq[Symbol] =
  result = @[]
  for i in 0..<args.len:
    if pd.argtypes[i].name.isFExprName:
      let opt = scope.getDecl(pd.argtypes[i].name)
      if opt.isNone:
        args[i].error("undeclared $# type." % $pd.argtypes[i].name)
      result.add(opt.get)
    elif $pd.argtypes[i].name == "TExpr":
      let opt = scope.getDecl(name("TExpr"))
      if opt.isNone:
        args[i].error("undeclared TExpr type.")
      let sym = symcopy(opt.get)
      sym.types.add(args[i].typ)
      result.add(sym)
