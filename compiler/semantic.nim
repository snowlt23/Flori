
import fexpr, parser
import scope

import tables
import options
import strutils, sequtils
import os
import deques

import types
export types.SemanticContext

import metadata

type
  InternalMarkKind* = enum
    internalDefn
    internalDeftype
    internalIf
    internalWhile
    internalDef
    internalFieldAccess
    internalInit
    internalImport
  InternalPragma* = object
    importc*: Option[string]
    header*: Option[string]
    infixc*: bool
  TypeExpr* = object
    name*: FExpr
    generics*: FExpr

defMetadata(internalScope, Scope)
defMetadata(internalToplevel, bool)
defMetadata(internalMark, InternalMarkKind)
defMetadata(internalPragma, InternalPragma)

proc isToplevel*(fexpr: FExpr): bool = fexpr.hasInternalToplevel

proc evalFExpr*(ctx: SemanticContext, scope: Scope, fexpr: FExpr)

proc name*(fexpr: FExpr): Name =
  case fexpr.kind
  of fexprNames:
    return name($fexpr)
  of fexprQuote:
    return name(fexpr.quoted)
  else:
    fexpr.error("$# is not name." % $fexpr)

proc getType*(fexpr: FExpr): Symbol =
  if fexpr.typ.isNone:
    fexpr.error("this expression undecide type.")
  return fexpr.typ.get

proc addInternalEval*(scope: Scope, n: Name, p: proc (ctx: SemanticContext, scope: Scope, fexpr: FExpr)) =
  let status = scope.addFunc(ProcDecl(isInternal: true, internalproc: p, name: n, argtypes: @[], sym: scope.symbol(n, symbolInternal, fident(internalSpan, name("internal"))))) # FIXME: returntype
  if not status:
    fseq(internalSpan).error("redefinition $# function." % $n)

proc voidtypeExpr*(span: Span): FExpr =
  return fident(span, name("Void"))

proc parseType*(fexpr: FExpr): TypeExpr =
  if fexpr.kind == fexprSeq:
    if fexpr[0].kind != fexprIdent:
      fexpr[0].error("$# isn't type name." % $fexpr[0])
    if fexpr[1].kind == fexprArray:
      result.name = fexpr[0]
      result.generics = fexpr[1]
    else:
      result.name = fexpr[0]
      result.generics = farray(fexpr.span)
  elif fexpr.kind == fexprIdent:
    result.name = fexpr
    result.generics = farray(fexpr.span)
  else:
    fexpr.error("$# isn't type." % $fexpr)

proc isParametricTypeExpr*(fexpr: FExpr): bool =
  fexpr.kind == fexprSeq and $fexpr[0] == "|" and fexpr[1].kind == fexprIdent and fexpr[2].kind in {fexprIdent, fexprList}
proc isTypeExpr*(fexpr: FExpr): bool =
  fexpr.kind == fexprIdent or isParametricTypeExpr(fexpr)
proc isPragmaPrefix*(fexpr: FExpr): bool =
  fexpr.kind == fexprPrefix and $fexpr == "$"

proc isSpecSymbol*(sym: Symbol): bool =
  if sym.kind == symbolType:
    return true
  elif sym.kind == symbolGenerics:
    return false
  else:
    for t in sym.types:
      if t.kind != symbolType:
        return false
    return true
proc isSpecTypes*(types: seq[Symbol]): bool =
  for t in types:
    if not t.isSpecSymbol:
      return false
  return true

proc resolveByType*(scope: Scope, fexpr: FExpr, n: Name) =
  let opt = scope.getDecl(n)
  if opt.isNone:
    fexpr.error("$# type is undefined." % $n)
  fexpr.typ = opt
proc resolveByVoid*(scope: Scope, fexpr: FExpr) =
  scope.resolveByType(fexpr, name("Void"))

proc evalType*(ctx: SemanticContext, scope: Scope, fexpr: var FExpr): Symbol =
  if fexpr.kind == fexprIdent:
    let opt = scope.getDecl(name(fexpr))
    if opt.isNone:
      fexpr.error("undeclared $# type." % $fexpr)
    fexpr = fsymbol(fexpr.span, opt.get)
    return opt.get
  elif fexpr.isParametricTypeExpr: # parametric type (generics)
    if fexpr.len <= 1:
      fexpr.error("parametric type requires greater than 2 arguments.")
    let opt = scope.getDecl(name(fexpr[1]))
    if opt.isNone:
      fexpr.error("undeclared $# type." % $fexpr[1])
    var sym = opt.get.scope.symbol(opt.get.name, opt.get.kind, opt.get.fexpr)
    if fexpr[2].kind == fexprIdent:
      sym.types.add(ctx.evalType(scope, fexpr[2]))
    elif fexpr[2].kind == fexprList:
      for arg in fexpr[2].mitems:
        sym.types.add(ctx.evalType(scope, arg))
    else:
      fexpr.error("$# is not parametric type expression." % $fexpr)
    fexpr = fsymbol(fexpr[0].span, sym)
    return sym
  else:
    fexpr.error("$# isn't type. ($#)" % [$fexpr, $fexpr.kind])

proc evalInfixCall*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  let fn = fexpr[0]
  let left = fexpr[1]
  let right = fexpr[2]
  ctx.evalFExpr(scope, left)
  ctx.evalFExpr(scope, right)

  let argtypes = @[left.getType, right.getType]

  let opt = scope.getFunc(procname(name(fn), argtypes))
  if opt.isNone:
    fexpr.error("undeclared $#($#) function." % [$fn, argtypes.mapIt($it).join(", ")])
  fexpr.typ = some(opt.get.returntype)
  # symbol resolve
  fexpr[0] = fsymbol(fexpr[0].span, opt.get.sym)

# Instantiation
include instantiate

proc evalFuncCall*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  let fn = fexpr[0]
  for arg in fexpr[1]:
    ctx.evalFExpr(scope, arg)
  let argtypes = fexpr[1].mapIt(it.getType)

  let opt = scope.getFunc(procname(name(fn), argtypes))
  if opt.isNone:
    fexpr.error("undeclared $#($#) function." % [$fn, argtypes.mapIt($it).join(", ")])
  if opt.get.sym.fexpr.hasDefn and opt.get.sym.fexpr.defn.generics.isSome: # generics
    # symbol resolve
    ctx.expandBy(fexpr.span):
      fexpr[0] = ctx.instantiateDefn(scope, opt.get.sym.fexpr, argtypes)
      fexpr.typ = some(fexpr[0].defn.ret.symbol)
  else:
    fexpr.typ = some(opt.get.returntype)
    # symbol resolve
    fexpr[0] = fsymbol(fexpr[0].span, opt.get.sym)

proc internalScope*(ctx: SemanticContext): Scope =
  ctx.modules[name("internal")]

proc evalFExpr*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  fexpr.internalScope = scope
  case fexpr.kind
  of fexprIdent:
    let opt = scope.getDecl(name(fexpr))
    if opt.isNone:
      fexpr.error("undeclared $# ident." % $fexpr)
    fexpr.typ = opt.get.fexpr.typ
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
    for son in fexpr:
      ctx.evalFExpr(scope, son)
    fexpr.typ = fexpr[^1].typ
  of fexprSeq:
    let fn = fexpr[0]
    let internalopt = scope.getFunc(procname(name(fn), @[]))
    if internalopt.isSome:
      internalopt.get.internalproc(ctx, scope, fexpr)
    elif fexpr.len == 2 and fexpr[1].kind == fexprList: # function call
      ctx.evalFuncCall(scope, fexpr)
    elif fexpr[0].kind == fexprInfix:
      ctx.evalInfixCall(scope, fexpr)
    else:
      fexpr.error("unsupported FSeq call.")
  else:
    fexpr.error("$# is unsupported expression in eval." % $fexpr.kind)

proc evalModule*(ctx: SemanticContext, name: Name, fexprs: seq[FExpr]) =
  let scope = newScope(name)
  scope.importScope(name("internal"), ctx.internalScope)
  for f in fexprs:
    f.internalToplevel = true
    ctx.evalFExpr(scope, f)
    scope.toplevels.add(f)
  ctx.modules[name] = scope

proc evalFile*(ctx: SemanticContext, filepath: string): Name {.discardable.} =
  let (dir, file, _) = filepath.splitFile()
  let modname = name((dir & "." & file).split("."))
  let fexprs = parseToplevel(filepath, readFile(filepath))
  ctx.evalModule(modname, fexprs)
  return modname
