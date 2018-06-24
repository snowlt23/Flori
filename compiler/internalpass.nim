
import fcore, typepass, sempass
import passmacro
# import compileutils, macroffi

import options
import strutils, sequtils
import tables
import os

proc semFile*(filepath: string): Option[FScope]

#
# Parser
#

proc getFieldType*(fexpr: FExpr, fieldname: string): Option[Symbol] =
  let parsed = parseDeftype(fexpr)
  if parsed.body.isSome:
    for field in fexpr[parsed.body.get]:
      if $field[0] == fieldname:
        return some(field[1].symbol)
  return none(Symbol)

#
# Pragmas
#

# proc semConverter*(scope: FScope, fexpr: var FExpr) =
  # if fexpr[1].defn.args.len != 1:
  #   fexpr.error("converter fn should be 1 argument.")
  # let fromtype = fexpr[1].defn.args[0][1].symbol
  # if not fromtype.fexpr.hasConverters:
  #   fromtype.fexpr.converters = Converters(converters: @[])
  # fromtype.fexpr.converters.converters.add(fexpr[1])

proc semInternalOpPragma*(scope: FScope, fexpr: var FExpr) =
  if fexpr[0].len != 2 or fexpr[0][1].kind != fexprStrLit:
    fexpr.error("illegal internal pragma: $#" % $fexpr[0])
  let internalname = $fexpr[0][1].strval
  case internalname
  of "+":
    fexpr[1].obj.internal.get.obj.internalop = internalAdd
  of "-":
    fexpr[1].obj.internal.get.obj.internalop = internalSub
  of "<":
    fexpr[1].obj.internal.get.obj.internalop = internalLess
  else:
    fexpr[0].error("couldn't find internal operation: $#" % internalname)
proc semInternalSizePragma*(scope: FScope, fexpr: var FExpr) =
  if fexpr[0].len != 2 or fexpr[0][1].kind != fexprIntLit:
    fexpr.error("illegal internal pragma: $#" % $fexpr[0])
  let size = fexpr[0][1].intval
  fexpr[1].obj.internal.get.obj.internalsize = int(size)

proc semPragma*(scope: FScope, fexpr: FExpr, pragma: FExpr) =
  if pragma.kind != fexprArray:
    pragma.error("$# isn't pragma." % $pragma)
  fexpr.obj.internal = some(newInternalMarker())

  for p in pragma.mitems:
    let pname = if p.kind == fexprIdent:
                 p
               else:
                 p[0]
    var pcall = if p.kind == fexprIdent:
                  p.span.quoteFExpr("`embed(`embed)", [p, fexpr])
                else:
                  p.span.quoteFExpr("`embed `embed", [p, fexpr])
    let internalopt = scope.getFunc(procname($pname, @[]))
    if internalopt.isSome:
      (internalopt.get.internalproc.get)(scope, pcall)
    else:
      pname.error("undeclared $# pragma." % $pname)

#
# Evaluater
#

proc declGenerics*(scope: FScope, fexpr: FExpr): seq[Symbol] =
  result = @[]
  for g in fexpr.mitems:
    if g.kind == fexprSymbol:
      result.add(g.symbol)
    else:
      let gname = istring($g)
      let sym = scope.symbol(gname, symbolGenerics, g)
      scope.addDecl(gname, sym)
      g = fsymbol(g.span, sym)
      result.add(sym)

proc declArgtypes*(scope: FScope, fexpr: FExpr): seq[Symbol] =
  result = @[]
  for i, arg in fexpr.mpairs:
    if arg[1].kind == fexprSymbol:
      result.add(arg[1].symbol)
    else:
      let typesym = scope.semType(arg, 1)
      arg[1] = fsymbol(arg[1].span, typesym)
      result.add(typesym)

proc semFunc*(scope: FScope, fexpr: var FExpr, defsym: SymbolKind, decl: bool): (FScope, seq[Symbol], seq[Symbol], Symbol, Symbol) =
  let parsed = parseDefn(fexpr)
  let fnscope = scope.extendFScope()
  let generics = if parsed.generics.isSome:
                   fnscope.declGenerics(fexpr[parsed.generics.get])
                 else:
                   @[]
  let argtypes = fnscope.declArgtypes(fexpr[parsed.argdecls])
  let rettype = if parsed.ret.isSome:
                  fnscope.semType(fexpr, parsed.ret.get)
                else:
                  voidtypeSymbol

  let symkind = if fexpr[parsed.name].kind == fexprQuote:
                  symbolInfix
                else:
                  defsym

  let name = istring(if fexpr[parsed.name].kind == fexprQuote: $fexpr[parsed.name].obj.quoted else: $fexpr[parsed.name])
  let sym = scope.symbol(name, symkind, fexpr)

  if decl:
    let pd = ProcDecl(name: name, argtypes: iarray(argtypes), generics: iarray(generics), returntype: rettype, sym: sym)
    scope.addFunc(pd)
    fnscope.addFunc(pd)

  let fsym = fsymbol(fexpr[0].span, sym)
  fexpr[parsed.name] = fsym
  fexpr.scope = some(fnscope)

  for i, arg in fexpr[parsed.argdecls]:
    let argname = istring($arg[0])
    let argsym = scope.symbol(argname, symbolArg, arg[0])
    argsym.fexpr.typ = some(argtypes[i])
    fnscope.addDecl(argname, argsym)
    # replace by symbol
    arg[0] = fsymbol(arg[0].span, argsym)
    arg[1] = fsymbol(arg[1].span, argtypes[i])
  if parsed.ret.isSome:
    fexpr[parsed.ret.get] = fsymbol(fexpr[parsed.ret.get].span, rettype)

  if parsed.pragma.isSome:
    scope.semPragma(fexpr, fexpr[parsed.pragma.get])

  if parsed.generics.isSome and fexpr.isGenerics(parsed):
    return (fnscope, generics, argtypes, rettype, sym)

  if parsed.body.isSome:
    fnscope.rootPass(fexpr[parsed.body.get])
    if fexpr[parsed.body.get].len != 0:
      if not fexpr[parsed.body.get].typ.get.spec(rettype):
        fexpr[parsed.body.get].error("function expect $# return type, actually $#" % [$rettype, $fexpr[parsed.body.get].typ.get])

  return (fnscope, generics, argtypes, rettype, sym)

proc semDefn*(scope: FScope, fexpr: var FExpr) =
  let (fnscope, generics, argtypes, rettype, sym) = semFunc(scope, fexpr, symbolFunc, true)

proc semDeftype*(scope: FScope, fexpr: var FExpr) =
  let parsed = parseDeftype(fexpr)

  let typename = if fexpr[parsed.name].kind == fexprIdent:
                   fexpr[parsed.name].idname
                 else:
                   fexpr[parsed.name].symbol.name
  let sym = scope.symbol(typename, if parsed.generics.isSome: symbolTypeGenerics else: symbolType, fexpr)
  if parsed.generics.isSome and not fexpr.isGenerics(parsed):
    sym.obj.types = iarray(fexpr[parsed.generics.get].mapIt(it.symbol))
  scope.addDecl(typename, sym)

  let fsym = fsymbol(fexpr[0].span, sym)
  let typescope = scope.extendFScope()
  fexpr[parsed.name] = fsym
  fexpr.scope = some(typescope)

  if fexpr.isGenerics(parsed):
    return

  if parsed.body.isSome:
    for field in fexpr[parsed.body.get]:
      let fieldtypesym = typescope.semType(field, 1)
      field[1] = fsymbol(field[1].span, fieldtypesym)

proc semIf*(scope: FScope, fexpr: var FExpr) =
  let parsed = parseIf(fexpr)
  var branchtypes = newSeq[Symbol]()
  var isret = true

  for branch in parsed.elifbranches:
    let (condi, bodyi) = branch
    let bscope = scope.extendFScope()
    bscope.rootPass(fexpr[condi])
    if not fexpr[condi].typ.get.isBoolType:
      fexpr[condi].error("if expression cond type should be Bool.")
    bscope.rootPass(fexpr[bodyi])
    if fexpr[bodyi].len != 0:
      branchtypes.add(fexpr[bodyi].typ.get)
    else:
      isret = false

  if parsed.elsebody.isSome:
    let bscope = scope.extendFScope()
    bscope.rootPass(fexpr[parsed.elsebody.get])
    if fexpr[parsed.elsebody.get].len != 0:
      branchtypes.add(fexpr[parsed.elsebody.get].typ.get)
    else:
      isret = false
  else:
    isret = false

  if isret and branchtypes.isEqualTypes:
    fexpr.typ = some(branchtypes[0])
  else:
    resolveByVoid(fexpr)

proc semWhile*(scope: FScope, fexpr: var FExpr) =
  if fexpr.len != 3 or fexpr[1].kind != fexprList or fexpr[1].len != 1 or fexpr[2].kind != fexprBlock:
    fexpr.error("invalid while syntax.")
  scope.rootPass(fexpr[1])
  if not fexpr[1].typ.get.isBoolType:
    fexpr[1].error("while statement cond type chould be Bool.")
  let bodyscope = scope.extendFScope()
  bodyscope.rootPass(fexpr[2])
  resolveByVoid(fexpr)

proc semDef*(scope: FScope, fexpr: var FExpr) =
  if fexpr.len != 3:
    fexpr.error("invalid `:= syntax.")

  let name = fexpr[1]
  if name.kind != fexprIdent:
    name.error("variable name should be FIdent.")

  scope.rootPass(fexpr[2])
  if fexpr[2].typ.get.isVoidType:
    fexpr[2].error("value is Void.")
  resolveByVoid(fexpr)

  let varname = istring($name)
  let varsym = scope.symbol(varname, symbolDef, name)
  fexpr[1].typ = some(fexpr[2].typ.get.varsym())
  fexpr[1] = fsymbol(fexpr[1].span, varsym)
  scope.addDecl(varname, varsym)

proc semSet*(scope: FScope, fexpr: var FExpr) =
  if fexpr.len != 3:
    fexpr.error("invalid `= syntax.")

  resolveByVoid(fexpr)
  scope.rootPass(fexpr[1])
  scope.rootPass(fexpr[2])

  if fexpr[1].typ.isNone:
    fexpr[1].error("target hasn't type.")
  if fexpr[2].typ.isNone:
    fexpr[2].error("value hasn't type.")
  if fexpr[1].typ.get.kind != symbolVar and fexpr[1].typ.get.kind != symbolRef:
    fexpr[1].error("ref value expected.")

  if fexpr[1].isInfixFuncCall and $fexpr[1][0] == "!" and scope.hasSetter(fexpr[1][1].typ.get, fexpr[1][2].typ.get, fexpr[2].typ.get):
    fexpr = fexpr.span.quoteFExpr("`!!(`embed, `embed, `embed)", [fexpr[1][1], fexpr[1][2], fexpr[2]])
    scope.rootPass(fexpr)
    return

proc semFieldAccess*(scope: FScope, fexpr: var FExpr) =
  if fexpr.len != 3:
    fexpr.error("illegal `. syntax.")
  let fieldname = fexpr[2]
  if fieldname.kind != fexprIdent:
    fieldname.error("field name should be FIdent.")
  scope.rootPass(fexpr[1])
  if fexpr[1].typ.isNone:
    fexpr[1].error("value hasn't type.")
  let fieldopt = getFieldType(fexpr[1].typ.get.fexpr, $fieldname)
  if fieldopt.isNone:
    fieldname.error("$# hasn't $# field." % [$fexpr[1].typ, $fieldname])
  if fexpr[1].typ.get.kind in {symbolRef, symbolVar}:
    fexpr.typ = some(varsym(fieldopt.get))
  else:
    fexpr.typ = some(fieldopt.get)

proc semDot*(scope: FScope, fexpr: var FExpr) =
  scope.semFieldAccess(fexpr)

proc semInit*(scope: FScope, fexpr: var FExpr) =
  if fexpr.len != 3:
    fexpr.error("init require 2 arguments.")
  if fexpr[1].kind != fexprList:
    fexpr.error("init type should be FList.")
  if fexpr[1].len != 1:
    fexpr.error("init type should be single argument.")
  if fexpr[2].kind != fexprBlock:
    fexpr.error("init body should be FBlock.")

  let typesym = scope.semType(fexpr[1][0], 0)
  fexpr.typ = some(typesym)
  scope.rootPass(fexpr[2])

proc semImport*(scope: FScope, fexpr: var FExpr) =
  if fexpr.len != 2:
    fexpr.error("import syntax require filepath.")
  if fexpr[1].kind != fexprStrLit:
    fexpr.error("import syntax filepath should be FStrLit.")
  let importname = fexpr[1].strval
  let filepath = ($importname).replace(".", "/")
  var importmod = semFile(filepath & ".flori")
  if importmod.isNone:
    importmod = semFile(filepath / "root.flori")
    if importmod.isNone:
      fexpr.error("couldn't import $#" % $fexpr[1])
  scope.obj.top.importFScope(importname, importmod.get)

# proc collectQuotedItems*(fexpr: FExpr, collected: var seq[FExpr]) =
#   for son in fexpr:
#     if son.kind == fexprQuote and son.quoted.kind == fexprIdent:
#       collected.add(son.quoted)
#     elif son.kind in fexprContainer:
#       collectQuotedItems(son, collected)

# proc semQuote*(scope: FScope, fexpr: var FExpr) =
#   if fexpr.len != 2:
#     fexpr.error("quote expected fblock.")
#   if fexpr[1].kind != fexprBlock:
#     fexpr[1].error("quote expected FBlock, actually $#" % $fexpr[1].kind)
#   let fstr = fstrlit(fexpr[1].span, ($fexpr[1]).replace("\n", ";").replace("\"", "\\\"").replace("\\n", "\\\\n"))

#   let ret = fblock(fexpr.span)
#   let tmpid = fident(fexpr.span, scope.ctx.genTmpName())
#   ret.addSon(fexpr.span.quoteFExpr("`embed := new_farray()", [tmpid]))
#   var collected = newSeq[FExpr]()
#   collectQuotedItems(fexpr[1], collected)
#   for c in collected:
#     ret.addSon(genCall(fident(fexpr.span, name("push")), tmpid, c)) # push(tmpid, c)

#   ret.addSon(fexpr.span.quoteFExpr("quote_expand(parse(\"$#\", $#, $#, `embed), `embed)" % [fexpr.span.filename, $fexpr.span.line, $fexpr.span.linepos], [fstr, tmpid]))
#   fexpr = ret
#   scope.rootPass(fexpr)

proc semBlock*(scope: FScope, fexpr: var FExpr) =
  if fexpr.len != 2:
    fexpr.error("invalid block syntax.")
  if fexpr[1].kind != fexprBlock:
    fexpr.error("invalid block syntax.")
  let blockscope = scope.extendFScope()
  blockscope.rootPass(fexpr[1])

#
# Internal
#

proc addInternalEval*(scope: FScope, n: string, p: InternalProcType) =
  internals.add(n)
  scope.addFunc(ProcDecl(
    internalproc: some(p),
    name: istring(n),
    argtypes: iarray[Symbol](),
    generics: iarray[Symbol]()
  ))

proc initInternalEval*(scope: FScope) =
  scope.addInternalEval("fn", semDefn)
  scope.addInternalEval("type", semDeftype)
  scope.addInternalEval("if", semIf)
  scope.addInternalEval("while", semWhile)
  scope.addInternalEval(":=", semDef)
  scope.addInternalEval("=", semSet)
  scope.addInternalEval(".", semDot)
  scope.addInternalEval("init", semInit)
  scope.addInternalEval("import", semImport)
  scope.addInternalEval("block", semBlock)

  # pragmas
  scope.addInternalEval("internalop", semInternalOpPragma)
  scope.addInternalEval("internalsize", semInternalSizePragma)

proc initInternalScope*() =
  let scope = newFScope("internal", "internal")
  scope.initInternalPrimitive()
  scope.initInternalEval()
  internalScope = scope

proc initRootScope*() =
  rootScope = newFScope("root", "root")
  initInternalScope()
  rootScope.importFScope(internalScope.obj.name, internalScope)
  rootPass = processSemPass

proc semModule*(name: IString, scope: FScope, fexprs: var seq[FExpr]) =
  let opt = scope.imports.find($name)
  if opt.isSome:
    return
  scope.importFScope(internalScope.obj.name, internalScope)
  for f in fexprs.mitems:
    scope.rootPass(f)
  rootScope.importFScope(name, scope)

proc semFile*(filepath: string): Option[FScope] =
  for importpath in gImage.importpaths:
    let (dir, n, _) = filepath.splitFile()
    let path = $importpath / dir / filepath & ".flori"
    let rootpath = $importpath / dir / filepath / "root.flori"
    if existsFile(path):
      var fexprs = parseToplevel(filepath, readFile(path))
      let modname = if dir != "":
                      dir.replace("/", ".") & "." & n
                    else:
                      n
      let scope = newFScope(modname, path)
      semModule(istring(modname), scope, fexprs)
      return some(scope)
    elif existsFile(rootpath):
      var fexprs = parseToplevel(filepath, readFile(rootpath))
      let modname = if dir != "":
                      dir.replace("/", ".") & "." & n
                    else:
                      n
      let scope = newFScope(modname, rootpath)
      semModule(istring(modname), scope, fexprs)
      return some(scope)
  return none(FScope)
