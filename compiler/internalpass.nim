
import fcore, sempass
import passmacro

import options
import strutils, sequtils
import algorithm
import tables
import os

proc semFile*(filepath: string): Option[FScope]

#
# Parser
#

#
# Pragmas
#

proc semInternalOp*(scope: FScope, fexpr: var FExpr) =
  let op = fexpr.args[0]
  if op.kind != fexprStrLit:
    fexpr.error("internalop argument should be strlit.")
  if scope.word.isNone:
    fexpr.error("internalop should be declaration in word.")
  case $op.strval
  of "int_add":
    scope.word.get.internal.obj.internalop = internalAdd
    fexpr.typ = some(intlittypeSymbol)
  of "int_sub":
    scope.word.get.internal.obj.internalop = internalSub
    fexpr.typ = some(intlittypeSymbol)
  of "int_lesser":
    scope.word.get.internal.obj.internalop = internalLess
    fexpr.typ = some(booltypeSymbol)
  else:
    fexpr.error("couldn't find internal operation: $#" % $op)

#
# Evaluater
#

proc semTyped*(scope: FScope, fexpr: var FExpr) =
  if scope.word.isNone:
    fexpr.error("$typed should be declaration in word.")

  var argtypes = newSeq[Symbol]()
  for arg in fexpr.args:
    let typ = scope.getDecl($arg)
    if typ.isNone:
      arg.error("undeclared $# type." % $arg)
    argtypes.add(typ.get)
  scope.word.get.internal.obj.argtypes = some(iarray(argtypes))
  resolveByVoid(fexpr)
proc semReturned*(scope: FScope, fexpr: var FExpr) =
  if scope.word.isNone:
    fexpr.error("$returned should be declaration in word.")
  let typ = scope.getDecl($fexpr.args[0])
  if typ.isNone:
    fexpr.args[0].error("undeclared $# type." % $fexpr.args[0])
  scope.word.get.internal.obj.returntype.wrapped = typ.get
  fexpr.typ = some(typ.get)
proc semTemplate*(scope: FScope, fexpr: var FExpr) =
  if scope.word.isNone:
    fexpr.error("$template should be declaration in word.")
  scope.word.get.internal.obj.isTemplate = true

proc semWord*(scope: FScope, fexpr: var FExpr) =
  let fnscope = scope.extendFScope()
  fnscope.word = some(fexpr)
  fexpr.obj.internal = some(newInternalMarker())

  let name = if fexpr.args[0].kind == fexprIdent:
               fexpr.args[0].idname
             else:
               fexpr.args[0].quoted
  let sym = scope.symbol(name, symbolWord, fexpr)

  let fsym = fsymbol(fexpr.call.span, sym)
  fexpr.args[0] = fsym
  fexpr.scope = some(fnscope)

  let pd = ProcDecl(name: name, sym: sym, undecided: true)
  scope.addWord(pd)
  fnscope.addWord(pd)
  fexpr.internal.obj.returntype = linksym(undeftypeSymbol)

  fnscope.rootPass(fexpr.args.mget(1))
  let opt = fexpr.obj.internal.get.obj.returntype.linkinfer(fexpr.args[1].gettype)
  if opt.isSome:
    fexpr.args[1].error(opt.get)

  if fexpr.internal.obj.argnames.isNone:
    fexpr.internal.obj.argnames = some(iarray(toSeq(fexpr.internal.obj.inferargnames.items).reversed()))
    fexpr.internal.obj.inferargnames = ilistNil[Symbol]()
  if fexpr.internal.obj.argtypes.isNone:
    var argtypes = toSeq(fexpr.internal.obj.inferargtypes.items).reversed()
    # var i = 0
    # for argtype in argtypes:
    #   if argtype.kind == symbolLink and $argtype.wrapped == "undef":
    #     argtype.wrapped = fnscope.symbol("T" & $i, symbolGenerics, fident(fexpr.span, "T" & $i))
    #     i.inc
    if fexpr.internal.obj.isTemplate: # FIXME:
      var i = 0
      for argtype in argtypes:
        if argtype.kind == symbolLink and $argtype.wrapped == "undef":
          argtype.wrapped = fnscope.symbol("T" & $i, symbolGenerics, fident(fexpr.span, "T" & $i))
          i.inc
    fexpr.internal.obj.argtypes = some(iarray(argtypes))
    fexpr.internal.obj.inferargtypes = ilistNil[Symbol]()

proc semStruct*(scope: FScope, fexpr: var FExpr) =
  if scope.word.isNone:
    fexpr.error("$struct should be declaration in word.")
  let typename = scope.word.get.args[0].symbol.name
  let sym = scope.symbol(typename, symbolType, fexpr)
  scope.obj.top.addDecl(typename, sym)

  for field in fexpr.args:
    let fieldtyp = linksym(undeftypeSymbol)
    let fieldtypf = fsymbol(field.span, fieldtyp)
    scope.word.get.internal.obj.inferargnames.add(scope.symbol(field.idname, symbolDef, field))
    scope.word.get.internal.obj.inferargtypes.add(fieldtyp)
    var fieldword = quoteFExpr(fexpr.span, """
`embed =>
  $typed(`embed)
  $field(`embed)
""", [field, scope.word.get.args[0], fieldtypf])
    scope.obj.top.rootPass(fieldword)
  fexpr.typ = some(sym)
proc semField*(scope: FScope, fexpr: var FExpr) =
  if scope.word.isNone:
    fexpr.error("$field should be declaration in word.")
  if fexpr.args[0].kind != fexprSymbol:
    fexpr.args[0].error("$field argument should be fsymbol.")
  fexpr.typ = some(fexpr.args[0].symbol)

proc semIf*(scope: FScope, fexpr: var FExpr) =
  var rettypes = newSeq[Symbol]()
  scope.rootPass(fexpr.ifcond)
  let ifscope = scope.extendFScope()
  ifscope.rootPass(fexpr.ifbody)
  rettypes.add(fexpr.ifbody.gettype)
  for elifbranch in fexpr.elifbranches.mitems:
    scope.rootPass(elifbranch.cond)
    let elifscope = scope.extendFScope()
    elifscope.rootPass(elifbranch.body)
    rettypes.add(elifbranch.body.gettype)
  let elsescope = scope.extendFScope()
  elsescope.rootPass(fexpr.elsebody)
  rettypes.add(fexpr.elsebody.gettype)

  if rettypes.isEqualTypes:
    fexpr.typ = some(rettypes[0])
  else:
    resolveByVoid(fexpr)

proc semWhile*(scope: FScope, fexpr: var FExpr) = discard
proc semDef*(scope: FScope, fexpr: var FExpr) = discard
proc semSet*(scope: FScope, fexpr: var FExpr) = discard
proc semDot*(scope: FScope, fexpr: var FExpr) =
  fexpr = quoteFExpr(fexpr.span, "`embed(`embed)", [fexpr.args[1], fexpr.args[0]])
  scope.rootPass(fexpr)

proc semImport*(scope: FScope, fexpr: var FExpr) = discard
  # if fexpr[1].kind != fexprStrLit:
  #   fexpr.error("import syntax filepath should be FStrLit.")
  # let importname = fexpr[1].strval
  # let filepath = ($importname).replace(".", "/")
  # var importmod = semFile(filepath & ".flori")
  # if importmod.isNone:
  #   importmod = semFile(filepath / "root.flori")
  #   if importmod.isNone:
  #     fexpr.error("couldn't import $#" % $fexpr[1])
  # scope.obj.top.importFScope(importname, importmod.get)

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

#
# Internal
#

proc addInternalEval*(scope: FScope, n: string, p: InternalProcType) =
  internals.add(n)
  scope.addWord(ProcDecl(
    internalproc: some(p),
    name: istring(n),
  ))

proc initInternalEval*(scope: FScope) =
  scope.addInternalEval("=>", semWord)
  scope.addInternalEval("$typed", semTyped)
  scope.addInternalEval("$returned", semReturned)
  scope.addInternalEval("$template", semTemplate)
  scope.addInternalEval("$struct", semStruct)
  scope.addInternalEval("$field", semField)

  scope.addInternalEval("if", semIf)
  scope.addInternalEval("while", semWhile)
  scope.addInternalEval(":=", semDef)
  scope.addInternalEval("=", semSet)
  scope.addInternalEval(".", semDot)

  # pragmas
  scope.addInternalEval("internalop", semInternalOp)

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
