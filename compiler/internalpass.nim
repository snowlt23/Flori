
import fcore, sempass
import passmacro
import codegen.jit

import options
import strutils, sequtils
import algorithm
import tables
import os

proc semFile*(filepath: string): Option[FScope]

#
# Pragmas
#

proc semInternalOp*(scope: FScope, fexpr: var FExpr) =
  let op = fexpr.args[0]
  if op.kind != fexprStrLit:
    fexpr.error("internalop argument should be strlit.")
  if scope.word.isNone:
    fexpr.error("internalop should be declaration in word.")
  scope.word.get.internal.obj.argnames = some(iarray[Symbol]())
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
  of "addr":
    scope.word.get.internal.obj.internalop = internalAddr
    # fexpr.typ = some(ptrtype(scope.word.get.internal.obj.argtypes.get[0]))
    fexpr.typ = some(pointertypeSymbol)
  of "deref":
    scope.word.get.internal.obj.internalop = internalDeref
    fexpr.typ = some(scope.word.get.internal.obj.argtypes.get[0])
  else:
    fexpr.error("couldn't find internal operation: $#" % $op)

proc semCFFI*(scope: FScope, fexpr: var FExpr) =
  if scope.word.isNone:
    fexpr.error("$cffi should be declaration in word.")
  if fexpr.args.len != 1 or fexpr.args[0].kind != fexprStrLit:
    fexpr.error("$cffi argument should be fstrlit.")
  scope.word.get.internal.obj.argnames = some(iarray[Symbol]())
  scope.word.get.internal.obj.cffi = some(fexpr.args[0].strval)
  resolveByVoid(fexpr)
proc semDLL*(scope: FScope, fexpr: var FExpr) =
  if scope.word.isNone:
    fexpr.error("$dll should be declaration in word.")
  if fexpr.args.len != 1 or fexpr.args[0].kind != fexprStrLit:
    fexpr.error("$dll argument should be fstrlit.")
  scope.word.get.internal.obj.dll = some(fexpr.args[0].strval)
  resolveByVoid(fexpr)
proc semCdecl*(scope: FScope, fexpr: var FExpr) =
  if scope.word.isNone:
    fexpr.error("$dll should be declaration in word.")
  scope.word.get.internal.obj.callconv = convCdecl
  resolveByVoid(fexpr)
proc semStdcall*(scope: FScope, fexpr: var FExpr) =
  if scope.word.isNone:
    fexpr.error("$dll should be declaration in word.")
  scope.word.get.internal.obj.callconv = convStdcall
  resolveByVoid(fexpr)
proc semInternalFFI*(scope: FScope, fexpr: var FExpr) =
  if scope.word.isNone:
    fexpr.error("$dll should be declaration in word.")
  scope.word.get.internal.obj.internalffi = true
  resolveByVoid(fexpr)

#
# Evaluater
#

proc semTyped*(scope: FScope, fexpr: var FExpr) =
  if scope.word.isNone:
    fexpr.error("$typed should be declaration in word.")

  var argtypes = newSeq[Symbol]()
  for arg in fexpr.args:
    let typ = scope.getDecl($arg)
    if typ.isSome:
      argtypes.add(typ.get)
    else:
      argtypes.add(scope.symbol(arg.idname, symbolGenerics, arg))
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

proc semWord*(scope: FScope, fexpr: var FExpr) =
  # echo fexpr
  let fnscope = scope.extendFScope()
  fnscope.word = some(fexpr)
  fexpr.obj.internal = some(newInternalMarker())

  let name = if fexpr.args[0].kind == fexprIdent:
               fexpr.args[0].idname
             elif fexpr.args[0].kind == fexprSymbol:
               fexpr.args[0].symbol.name
             else:
               fexpr.args[0].quoted
  let sym = scope.symbol(name, symbolWord, fexpr)

  let fsym = fsymbol(fexpr.call.span, sym)
  fexpr.args[0] = fsym
  fexpr.scope = some(fnscope)

  let pd = ProcDecl(name: name, sym: sym)
  scope.addWord(pd)
  fnscope.addWord(pd)
  fexpr.internal.obj.returntype = linksym(undeftypeSymbol)
  fexpr.internal.obj.undecided = true

  fnscope.rootPass(fexpr.args.mget(1))
  let opt = fexpr.internal.obj.returntype.linkinfer(fexpr.args[1].gettype)
  if opt.isSome:
    fexpr.args[1].error(opt.get)

  if fexpr.internal.obj.argnames.isNone:
    fexpr.internal.obj.argnames = some(iarray(toSeq(fexpr.internal.obj.inferargnames.items).reversed()))
    fexpr.internal.obj.inferargnames = ilistNil[Symbol]()
    if fexpr.internal.obj.argtypes.isSome:
      if fexpr.internal.obj.argnames.get.len != fexpr.internal.obj.argtypes.get.len:
        fexpr.error("unmatched $$typed($#) to $$named($#)" % [fexpr.internal.obj.argtypes.get.mapIt($it).join(", "), fexpr.internal.obj.argnames.get.mapIt($it).join(", ")])
      for i in 0..<fexpr.internal.obj.argnames.get.len:
        let opt = fexpr.internal.obj.argnames.get[i].fexpr.typ.linkinfer(fexpr.internal.obj.argtypes.get[i])
        if opt.isSome:
          fexpr.error(opt.get)
  if fexpr.internal.obj.argtypes.isNone:
    var argtypes = toSeq(fexpr.internal.obj.inferargtypes.items).reversed()
    var i = 0
    for argtype in argtypes:
      if argtype.kind == symbolLink and $argtype.wrapped == "undef":
        argtype.wrapped = fnscope.symbol("T" & $i, symbolGenerics, fident(fexpr.span, "T" & $i))
        i.inc
    fexpr.internal.obj.argtypes = some(iarray(argtypes))
    fexpr.internal.obj.inferargtypes = ilistNil[Symbol]()
  fexpr.internal.obj.undecided = false

proc semStruct*(scope: FScope, fexpr: var FExpr) =
  if scope.word.isNone:
    fexpr.error("$struct should be declaration in word.")
  let typename = scope.word.get.args[0].symbol.name
  let sym = scope.symbol(typename, symbolType, fexpr)
  scope.obj.top.addDecl(typename, sym)
  scope.word.get.internal.obj.argnames = some(iarray[Symbol]())

  var structsize = some(0)
  for field in fexpr.args.mitems:
    if field.kind == fexprSymbol:
      continue
    let (fieldname, fieldtyp) = if field.kind == fexprIdent:
                                  (field, linksym(undeftypeSymbol))
                                else:
                                  if field.kind != fexprInfix or $field.call != ":":
                                    field.error("please specific type of field.")
                                  if field.args[0].kind != fexprIdent:
                                    field.error("$struct field name should be fident.")
                                  let opt = scope.getDecl($field.args[1])
                                  if opt.isNone:
                                    field.args[1].error("undeclared $# type." % $field.args[1])
                                  (field.args[0], opt.get)
    let fieldtypf = fsymbol(field.span, fieldtyp)
    let fieldsym = scope.symbol(fieldname.idname, symbolDef, fieldname)
    if field.kind == fexprIdent:
      field = fsymbol(field.span, fieldsym)
      field.typ = some(fieldtyp)
    else:
      field.args[0] = fsymbol(field.args[0].span, fieldsym)
      field.args[0].typ = some(fieldtyp)
    scope.word.get.internal.obj.inferargnames.add(fieldsym)
    scope.word.get.internal.obj.inferargtypes.add(fieldtyp)
    var fieldword = quoteFExpr(fexpr.span, """
`embed =>
  $typed(`embed)
  $field(struc, `embed, `embed)
""", [fieldname, scope.word.get.args[0], fieldname, fieldtypf])
    scope.obj.top.rootPass(fieldword)
    gCtx.notevals.add(fieldword)
    if field.kind != fexprIdent and structsize.isSome:
      structsize = some(structsize.get + fieldtyp.typesize)
    else:
      structsize = none(int)
  if structsize.isSome:
    fexpr.obj.internal = some(newInternalMarker())
    fexpr.internal.obj.internalsize = structsize.get
  fexpr.internal.obj.isStruct = true
  scope.word.get.internal.obj.isStruct = true
  fexpr.typ = some(sym)
proc semField*(scope: FScope, fexpr: var FExpr) =
  if scope.word.isNone:
    fexpr.error("$field should be declaration in word.")
  if fexpr.args.len != 3:
    fexpr.error("expected: $field(struc, fieldname, fieldtype).")
  if fexpr.args[2].kind != fexprSymbol:
    fexpr.args[2].error("$field argument should be fsymbol.")
  scope.rootPass(fexpr.args.mget(0))
  fexpr.typ = some(fexpr.args[2].symbol)

proc semIf*(scope: FScope, fexpr: var FExpr) =
  var rettypes = newSeq[Symbol]()
  scope.rootPass(fexpr.ifbranch.args.mget(0))
  let ifbopt = fexpr.ifbranch.args[0].typ.linkinfer(booltypeSymbol)
  if ifbopt.isSome:
    fexpr.ifbranch.args[0].error(ifbopt.get & " in " & $fexpr.ifbranch.args[0])
  let ifscope = scope.extendFScope()
  ifscope.rootPass(fexpr.ifbranch.args.mget(1))
  rettypes.add(fexpr.ifbranch.args[1].gettype)
  for elifbranch in fexpr.elifbranches.mitems:
    scope.rootPass(elifbranch.args.mget(0))
    let elifbopt = elifbranch.args[0].typ.linkinfer(booltypeSymbol)
    if elifbopt.isSome:
      elifbranch.args[0].error(elifbopt.get & " in " & $elifbranch.args[0])
    let elifscope = scope.extendFScope()
    elifscope.rootPass(elifbranch.args.mget(1))
    rettypes.add(elifbranch.args[1].gettype)
  let elsescope = scope.extendFScope()
  elsescope.rootPass(fexpr.elsebody)
  rettypes.add(fexpr.elsebody.gettype)

  if rettypes.isEqualTypes:
    fexpr.typ = some(rettypes[0])
  else:
    resolveByVoid(fexpr)

proc semWhile*(scope: FScope, fexpr: var FExpr) =
  scope.rootPass(fexpr.whilebranch.args.mget(0))
  let whilebopt = fexpr.whilebranch.args[0].typ.linkinfer(booltypeSymbol)
  if whilebopt.isSome:
    fexpr.whilebranch.args[0].error(whilebopt.get & " in " & $fexpr.whilebranch.args[0])
  let whilescope = scope.extendFScope()
  whilescope.rootPass(fexpr.whilebranch.args.mget(1))
  resolveByVoid(fexpr)

proc semDef*(scope: FScope, fexpr: var FExpr) =
  let defname = fexpr.args[0].idname
  let sym = scope.symbol(defname, symbolDef, fexpr.args[0])
  fexpr.args[0] = fsymbol(fexpr.args[0].span, sym)
  scope.rootPass(fexpr.args.mget(1))
  fexpr.args[0].symbol.fexpr.typ = some(fexpr.args[1].gettype)
  scope.addDecl(defname, sym)
  resolveByVoid(fexpr)

proc semSet*(scope: FScope, fexpr: var FExpr) =
  scope.rootPass(fexpr.args.mget(0))
  scope.rootPass(fexpr.args.mget(1))
  let opt1 = fexpr.args[0].typ.linkinfer(fexpr.args[1].gettype)
  if opt1.isSome:
    fexpr.error(opt1.get)
  let opt2 = fexpr.args[1].typ.linkinfer(fexpr.args[0].gettype)
  if opt2.isSome:
    fexpr.error(opt2.get)
  if scope.hasCopy(ptrtype(fexpr.args[0].gettype), fexpr.args[1].gettype):
    fexpr = fexpr.span.quoteFExpr("copy(addr(`embed), `embed)", [fexpr.args[0], fexpr.args[1]])
    scope.rootPass(fexpr)
  else:
    resolveByVoid(fexpr)

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
proc relocInternalEval*(scope: FScope, n: string, p: InternalProcType) =
  var cur = scope.procdecls
  while true:
    if cur.isNil:
      break
    if $cur.value.name == n:
      cur.value.value.decls.value.internalproc = some(p)
    cur = cur.next

proc initInternalEval*(scope: FScope) =
  scope.addInternalEval("=>", semWord)
  scope.addInternalEval("$typed", semTyped)
  scope.addInternalEval("$returned", semReturned)
  scope.addInternalEval("$struct", semStruct)
  scope.addInternalEval("$field", semField)

  scope.addInternalEval("if", semIf)
  scope.addInternalEval("while", semWhile)
  scope.addInternalEval(":=", semDef)
  scope.addInternalEval("=", semSet)
  scope.addInternalEval(".", semDot)

  # pragmas
  scope.addInternalEval("internalop", semInternalOp)
  scope.addInternalEval("$cffi", semCFFI)
  scope.addInternalEval("$dll", semDLL)
  scope.addInternalEval("$cdecl", semCdecl)
  scope.addInternalEval("$stdcall", semStdcall)
  scope.addInternalEval("$internalffi", semInternalFFI)

proc relocInternalEval*(scope: FScope) =
  scope.relocInternalEval("=>", semWord)
  scope.relocInternalEval("$typed", semTyped)
  scope.relocInternalEval("$returned", semReturned)
  scope.relocInternalEval("$struct", semStruct)
  scope.relocInternalEval("$field", semField)

  scope.relocInternalEval("if", semIf)
  scope.relocInternalEval("while", semWhile)
  scope.relocInternalEval(":=", semDef)
  scope.relocInternalEval("=", semSet)
  scope.relocInternalEval(".", semDot)

  # pragmas
  scope.relocInternalEval("internalop", semInternalOp)
  scope.relocInternalEval("$cffi", semCFFI)
  scope.relocInternalEval("$dll", semDLL)
  scope.relocInternalEval("$cdecl", semCdecl)
  scope.relocInternalEval("$stdcall", semStdcall)
  scope.relocInternalEval("$internalffi", semInternalFFI)

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
proc relocRootScope*() =
  rootScope = fscopeRoot()

proc initFlori*(linmemsize = 1024*1024, jitsize = 1024*1024) =
  initLinmem(linmemsize)
  gImage = initFImage(initJitBuffer(jitsize))
  gCtx = initSemContext()
  initRootScope()

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
