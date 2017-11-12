
import sast
import sparser
import semantic
import ccodegen
import compile

import strutils
import sequtils
import tables
import options
import os
import dynlib

var ctsharedHandle*: LibHandle = nil

proc evalSExpr*(scope: var Scope, sexpr: SExpr): SemanticExpr
proc evalFile*(context: SemanticContext, modulepath: string): Module

proc evalFieldAccess*(scope: var Scope, sexpr: SExpr, value: SemanticExpr): SemanticExpr =
  if ($sexpr.first)[0] != '.':
    sexpr.rest.first.span.raiseError("$# is not field access syntax" % $sexpr.rest.first)
  let fieldname = ($sexpr.first)[1..^1]
  
  var fasemexpr = newSemanticExpr(
    sexpr.span,
    semanticFieldAccess,
    notTypeSym,
    fieldaccess: FieldAccess(
      valuesym: value,
      fieldname: fieldname,
    ),
  )
  var rettype = notTypeSym
  for field in value.typesym.getSemExpr().struct.fields:
    if field.name == fieldname:
      rettype = field.typesym
      break
  if rettype.kind == typesymVoid:
    sexpr.rest.first.span.raiseError("undeclared field: $#" % fieldname)
  fasemexpr.typesym = rettype
  return fasemexpr

proc evalFuncCall*(scope: var Scope, span: Span, funcname: string, args: seq[SemanticExpr], argtypes: seq[TypeSymbol]): SemanticExpr =
  let tryfuncsym = scope.tryType(span, funcname, argtypes)
  if tryfuncsym.isNone:
    span.raiseError("undeclared function call: ($# $#)" % [funcname, argtypes.mapIt(it.debug).join(" ")])
  let typesym = tryfuncsym.get
  let semexpr = newSemanticExpr(
    span,
    semanticFuncCall,
    typesym.getSemexpr().typesym,
    funccall: FuncCall(
      callfunc: tryfuncsym.get,
      args: args,
    )
  )
  return semexpr

proc evalMacroCall*(scope: var Scope, span: Span, typesym: TypeSymbol, sexpr: SExpr): SemanticExpr =
  if ctsharedHandle.isNil:
    span.raiseError("ctsharedHandle is nil")
  let macrohandle = cast[proc (sexpr: SExpr): SExpr {.nimcall.}](ctsharedHandle.checkedSymAddr(typesym.genSymHash() & "_macro"))
  let expandsexpr = macrohandle(sexpr)
  let semexpr = newSemanticExpr(
    span,
    semanticSExpr,
    scope.getVoidSym(span),
    sexpr: expandsexpr
  )
  return semexpr

proc evalFuncCall*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  var args = newSeq[SemanticExpr]()
  for arg in sexpr.rest:
    args.add(scope.evalSExpr(arg))
  let argtypes = args.mapIt(it.typesym)
  let funcname = $sexpr.first
  let tryfuncsym = scope.tryType(sexpr.span, funcname, argtypes)
  if tryfuncsym.isNone:
    sexpr.span.raiseError("undeclared function call: ($# $#)" % [funcname, argtypes.mapIt(it.debug).join(" ")])
  if tryfuncsym.get.getSemExpr().kind == semanticMacro:
    return scope.evalMacroCall(sexpr.span, tryfuncsym.get, sexpr)
  else:
    return scope.evalFuncCall(sexpr.span, funcname, args, argtypes)

proc evalCall*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let trysym = scope.trySymbol(scope.newSemanticIdent(sexpr.first))
  if trysym.isSome:
    let semexpr = trysym.get.semexpr
    if semexpr.kind == semanticPrimitiveEval: # primitive eval
      let retsemexpr = semexpr.evalproc(scope, sexpr)
      if retsemexpr.kind == semanticSExpr:
        return scope.evalSExpr(retsemexpr.sexpr)
      else:
        return retsemexpr
    else:
      let retsemexpr = scope.evalFuncCall(sexpr)
      if retsemexpr.kind == semanticSExpr:
        return scope.evalSExpr(retsemexpr.sexpr)
      else:
        return retsemexpr
  elif ($sexpr.first)[0] == '.': # field access syntax
    return scope.evalFieldAccess(sexpr, scope.evalSExpr(sexpr.rest.first))
  else:
    let retsemexpr = scope.evalFuncCall(sexpr)
    if retsemexpr.kind == semanticSExpr:
      return scope.evalSExpr(retsemexpr.sexpr)
    else:
      return retsemexpr

#
# Eval
#

proc evalIdent*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let opt = scope.trySymbol(scope.newSemanticIdent(sexpr))
  if opt.isNone:
    sexpr.span.raiseError("undeclared ident: $#" % $sexpr)
  var semexpr = newSemanticExpr(sexpr.span, semanticSymbol, opt.get.semexpr.typesym, symbol: opt.get)
  return semexpr

proc evalAttr*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  let semid = scope.newSemanticIdent(sexpr)
  let opt = scope.trySymbol(semid)
  if opt.isSome:
    if opt.get.semexpr.kind == semanticGenerics:
      let semexpr = newSemanticExpr(sexpr.span, semanticGenericsChild, getTypeSymbol(opt.get), parent: opt.get)
      opt.get.semexpr.generics.children.add(semexpr)
      return semexpr
    else:
      return opt.get.semexpr
  else:
    sexpr.span.raiseError("couldn't find $# attr" % $sexpr)

proc evalInt*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  result = newSemanticExpr(
    sexpr.span, 
    semanticInt, 
    scope.tryType(sexpr.span, "Int32", @[]).get, 
    intval: sexpr.intval
  )
proc evalString*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  result = newSemanticExpr(
    sexpr.span, 
    semanticString, 
    scope.tryType(sexpr.span, "CString", @[]).get,
    strval: sexpr.strval
  )

proc evalSExpr*(scope: var Scope, sexpr: SExpr): SemanticExpr =
  # echo sexpr
  case sexpr.kind
  of sexprNil:
    sexpr.span.raiseError("can't eval nil")
  of sexprList:
    return scope.evalCall(sexpr)
  of sexprIdent:
    return scope.evalIdent(sexpr)
  of sexprAttr:
    return scope.evalAttr(sexpr)
  of sexprInt:
    return scope.evalInt(sexpr)
  of sexprString:
    return scope.evalString(sexpr)
  else:
    sexpr.span.raiseError("couldn't eval: $#" % $sexpr.kind)

include semantic_predefines

proc evalModule*(context: SemanticContext, modulename: string, sexpr: seq[SExpr]): Module =
  let modulename = modulename.replace("/", "_").replace("\\", "_").replace("-", "_")
  var module = newModule(context, modulename)
  var scope = newScope(module)
  scope.predefine()
  context.modules[modulename] = module
  for e in sexpr:
    let semexpr = scope.evalSExpr(e)
    if semexpr.kind == semanticSymbol:
      discard
    else:
      module.toplevelcalls.add(semexpr)
  return module

proc evalFile*(context: SemanticContext, modulepath: string): Module =
  let modulename = modulepath.replace(".flori")
  if context.modules.hasKey(modulename):
    return context.modules[modulename]

  var specfilepath = ""
  for includepath in context.includepaths:
    let filepath = includepath / modulename.replace(".", "/") & ".flori"
    if existsFile(filepath):
      specfilepath = filepath
      break
  if not existsFile(specfilepath):
    raise newException(SemanticError, "couldn't find file: $#" % modulename)
  let sexpr = parseToplevel(specfilepath, readFile(specfilepath))
  return context.evalModule(modulename, sexpr)

proc evalTopfile*(context: SemanticContext, modulepath: string) =
  context.topmodule = context.evalFile(modulepath)
