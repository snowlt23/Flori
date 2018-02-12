
import fexpr, parser
import scope, ctrc

import tables
import options
import strutils, sequtils
import os
import deques

import types
export types.SemanticContext

import metadata

proc evalFExpr*(ctx: SemanticContext, scope: Scope, fexpr: var FExpr)

proc name*(fexpr: FExpr): Name =
  case fexpr.kind
  of fexprNames:
    return name($fexpr)
  of fexprQuote:
    return name(fexpr.quoted)
  of fexprSymbol:
    return fexpr.symbol.name
  else:
    fexpr.error("$# is not name." % $fexpr)

proc getType*(fexpr: FExpr): Symbol =
  if fexpr.typ.isNone:
    fexpr.error("this expression undecide type.")
  return fexpr.typ.get

proc addInternalEval*(scope: Scope, n: Name, p: proc (ctx: SemanticContext, scope: Scope, fexpr: var FExpr)) =
  let status = scope.addFunc(ProcDecl(
    isInternal: true,
    internalproc: p,
    name: n,
    argtypes: @[],
    generics: @[],
    sym: scope.symbol(n, symbolInternal, fident(internalSpan, name("internal")))
  )) # FIXME: returntype
  if not status:
    fseq(internalSpan).error("redefinition $# function." % $n)

proc voidtypeExpr*(span: Span): FExpr =
  return fident(span, name("Void"))

proc parseTypeExpr*(fexpr: FExpr, pos: var int): tuple[typ: FExpr, generics: Option[FExpr]] =
  if fexpr.kind in {fexprIdent, fexprSymbol, fexprQuote}:
    result = (fexpr, none(FExpr))
  elif fexpr.isParametricTypeExpr(pos):
    result = (fexpr[pos], some(fexpr[pos+1]))
    pos += 2
  elif fexpr[pos].kind in {fexprIdent, fexprQuote}:
    result = (fexpr[pos], none(FExpr))
    pos += 1
  else:
    fexpr[pos].error("$# isn't type expression." % $fexpr[pos])

proc resolveByType*(scope: Scope, fexpr: FExpr, n: Name) =
  let opt = scope.getDecl(n)
  if opt.isNone:
    fexpr.error("$# type is undefined." % $n)
  fexpr.typ = opt
proc resolveByVoid*(scope: Scope, fexpr: FExpr) =
  scope.resolveByType(fexpr, name("Void"))

proc evalType*(ctx: SemanticContext, scope: Scope, typ: FExpr, generics: Option[FExpr]): Symbol =
  if generics.isSome:
    let opt = scope.getDecl(name(typ))
    if opt.isNone:
      typ.error("undeclared $# type." % $typ[1])
    var sym = opt.get.scope.symbol(opt.get.name, opt.get.kind, opt.get.fexpr)

    for arg in generics.get.mitems:
      var pos = 0
      let argtyp = arg.parseTypeExpr(pos)
      sym.types.add(ctx.evalType(scope, argtyp.typ, argtyp.generics))

    return sym
  else:
    let opt = scope.getDecl(name(typ))
    if opt.isNone:
      typ.error("undeclared $# type." % $typ)
    return opt.get
proc replaceByTypesym*(fexpr: var FExpr, sym: Symbol) =
  fexpr = fsymbol(fexpr.span, sym)

proc genCall*(name: FExpr, args: varargs[FExpr]): FExpr =
  fseq(name.span, @[name, flist(name.span, @args)])

proc genDestructorCall*(value: FExpr): FExpr =
  genCall(fident(value.span, name("destructor")), value)

proc expandDestructor*(ctx: SemanticContext, scope: Scope, body: FExpr) =
  for v in scope.scopevalues:
    v.ctrc.dec
    if v.ctrc.destroyed:
      let opt = scope.getFunc(procname(name("destructor"), @[v.typ.get]))
      if opt.isSome:
        var dcall = genDestructorCall(v)
        ctx.evalFExpr(scope, dcall)
        body.addSon(dcall)

# Instantiation
include instantiate

proc evalInfixCall*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  let fn = fexpr[0]
  var left = fexpr[1]
  var right = fexpr[2]
  ctx.evalFExpr(scope, left)
  ctx.evalFExpr(scope, right)

  let argtypes = @[left.getType, right.getType]

  let opt = scope.getFunc(procname(name(fn), argtypes))
  if opt.isNone:
    fexpr.error("undeclared $#($#) function." % [$fn, argtypes.mapIt($it).join(", ")])
  fexpr.typ = some(opt.get.returntype)
  # symbol resolve
  fexpr[0] = fsymbol(fexpr[0].span, opt.get.sym)

proc evalFuncCall*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  let fn = fexpr[0]
  for arg in fexpr[1].mitems:
    ctx.evalFExpr(scope, arg)
  let argtypes = fexpr[1].mapIt(it.getType)
  
  let opt = scope.getFunc(procname(name(fn), argtypes))
  if opt.isNone:
    fexpr.error("undeclared $#($#) function." % [$fn, argtypes.mapIt($it).join(", ")])

  # generics
  if opt.get.sym.fexpr.hasDefn and opt.get.sym.fexpr.defn.isGenerics:
    # symbol resolve
    ctx.expandBy(fexpr.span):
      fexpr[0] = ctx.instantiateDefn(scope, opt.get.sym.fexpr, argtypes)
      fexpr.typ = some(fexpr[0].defn.ret.symbol)
  else:
    fexpr.typ = some(opt.get.returntype)
    # symbol resolve
    fexpr[0] = fsymbol(fexpr[0].span, opt.get.sym)

proc evalGenericsFuncCall*(ctx: SemanticContext, scope: Scope, fexpr: Fexpr) =
  let fn = fexpr[0]
  for arg in fexpr[2].mitems:
    ctx.evalFExpr(scope, arg)
  let argtypes = fexpr[2].mapIt(it.getType)

  var gtypes = newSeq[Symbol]()
  for g in fexpr[1].mitems:
    var pos = 0
    let gtype = parseTypeExpr(g, pos)
    let t = ctx.evalType(scope, gtype.typ, gtype.generics)
    gtypes.add(t)
    g = fsymbol(g.span, t)

  let opt = scope.getFunc(procname(name(fn), argtypes))
  if opt.isNone:
    fexpr.error("undeclared $#($#) function." % [$fn, argtypes.mapIt($it).join(", ")])

  # generics 
  if opt.get.sym.fexpr.hasDefn and opt.get.sym.fexpr.defn.isGenerics:
    let generics = opt.get.sym.fexpr.defn.generics
    if generics.len < gtypes.len:
      fexpr.error("wrong of generics length: requires $#" % $generics)
    for i, gtyp in gtypes:
      generics[i].typ.get.instance = some(gtyp)
    ctx.expandBy(fexpr.span):
      fexpr[0] = ctx.instantiateDefn(scope, opt.get.sym.fexpr, argtypes)
      fexpr.typ = some(fexpr[0].defn.ret.symbol)
  else:
    fexpr.error("wrong function call, $# isn't generics function." % $fn)

proc internalScope*(ctx: SemanticContext): Scope =
  ctx.modules[name("internal")]

proc evalFExpr*(ctx: SemanticContext, scope: Scope, fexpr: var FExpr) =
  if not fexpr.hasInternalScope:
    fexpr.internalScope = scope
  case fexpr.kind
  of fexprIdent:
    let opt = scope.getDecl(name(fexpr))
    if opt.isNone:
      fexpr.error("undeclared $# ident." % $fexpr)
    if opt.get.instance.isSome:
      fexpr.typ = opt.get.instance
    else:
      fexpr.typ = opt.get.fexpr.typ
    fexpr.ctrc = initCTRC()
    fexpr.ctrc.link = opt.get.fexpr.ctrc
    # fexpr = fsymbol(fexpr.span, opt.get)
    fexpr.resolve = fsymbol(fexpr.span, opt.get)
  of fexprSymbol:
    discard
  of fexprIntLit:
    let opt = scope.getDecl(name("Int"))
    if opt.isNone:
      fexpr.error("undeclared Int type.")
    fexpr.typ = opt
  of fexprStrLit:
    let opt = scope.getDecl(name("CString"))
    if opt.isNone:
      fexpr.error("undeclared CString type.")
    fexpr.typ = opt
  of fexprArray, fexprList, fexprBlock:
    if fexpr.len == 0:
      return
    for son in fexpr.mitems:
      ctx.evalFExpr(scope, son)
    fexpr.typ = fexpr[^1].typ
  of fexprSeq:
    let fn = fexpr[0]
    let internalopt = scope.getFunc(procname(name(fn), @[]))
    if internalopt.isSome and internalopt.get.isInternal:
      internalopt.get.internalproc(ctx, scope, fexpr)
    elif internalopt.isSome and internalopt.get.isMacro:
      var called = internalopt.get.macroproc.call(fexpr)
      ctx.evalFExpr(scope, called)
      fexpr = called
    elif fexpr.len == 2 and fexpr[1].kind == fexprList: # function call
      ctx.evalFuncCall(scope, fexpr)
    elif fexpr.isGenericsFuncCall: # generics function call
      ctx.evalGenericsFuncCall(scope, fexpr)
    elif fexpr[0].kind == fexprSymbol and fexpr[0].symbol.kind == symbolInfix:
      ctx.evalInfixCall(scope, fexpr)
    elif fexpr[0].kind == fexprInfix:
      ctx.evalInfixCall(scope, fexpr)
    else:
      fexpr.error("unsupported `$#` FSeq call." % $fexpr)
  else:
    fexpr.error("$# is unsupported expression in eval." % $fexpr.kind)

proc evalModule*(ctx: SemanticContext, name: Name, fexprs: var seq[FExpr]) =
  let scope = newScope(name)
  scope.importScope(name("internal"), ctx.internalScope)
  for f in fexprs.mitems:
    f.internalToplevel = true
    ctx.evalFExpr(scope, f)
    scope.toplevels.add(f)
  ctx.modules[name] = scope

proc evalFile*(ctx: SemanticContext, filepath: string): Name {.discardable.} =
  let (dir, file, _) = filepath.splitFile()
  let modname = name((dir & "." & file).split("."))
  if ctx.modules.hasKey(modname):
    return modname
  var fexprs = parseToplevel(filepath, readFile(filepath))
  ctx.evalModule(modname, fexprs)
  return modname
