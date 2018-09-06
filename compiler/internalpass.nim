
import fexpr_core
import passutils, typepass, sempass
import compileutils, macroffi
import passmacro

import options
import strutils, sequtils
import tables
import os

proc semFile*(ctx: var SemContext, filepath: string): Option[IString]

#
# Parser
#

proc getFieldType*(fexpr: FExpr, fieldname: string): Option[Symbol] =
  if fexpr.metadata.internal != internalDeftype:
    fexpr.error("$# isn't structure type."  % $fexpr)
  for field in fexpr.typeBody:
    if $field[0] == fieldname:
      return some(field[1].symbol)
  return none(Symbol)

proc expandDefn*(fexpr: FExpr): FExpr =
  let newfexpr = fseq(fexpr.span)
  newfexpr.metadata = fexpr.metadata
  newfexpr.addSon(fexpr[0])
  var pos = 1

  # name
  if pos >= fexpr.len or fexpr[pos].kind notin {fexprIdent, fexprSymbol, fexprQuote}:
    fexpr.error("fn syntax expect function name.")
  newfexpr.addSon(fexpr[pos])
  pos.inc

  # generics
  if fexpr[pos].kind == fexprArray:
    newfexpr.addSon(fexpr[pos])
    pos.inc
  else:
    newfexpr.addSon(farray(fexpr.span))

  # args
  if pos >= fexpr.len or fexpr[pos].kind != fexprList:
    fexpr.error("fn syntax expect function arguments.")
  newfexpr.addSon(fexpr[pos])
  pos.inc

  let ret = fseq(fexpr.span)
  # ret ref
  if pos < fexpr.len and ($fexpr[pos] == "ref" or $fexpr[pos] == "dynamic"):
    ret.addSon(fexpr[pos])
    pos.inc

  # ret
  if pos < fexpr.len and fexpr[pos].kind in {fexprIdent, fexprSymbol}:
    ret.addSon(fexpr[pos])
    pos.inc
  else:
    ret.addSon(fident(fexpr.span, istring("Void")))

  # ret generics
  if pos < fexpr.len and fexpr[pos].kind == fexprArray:
    ret.addSon(fexpr[pos])
    newfexpr.addSon(ret)
    pos.inc
  else:
    newfexpr.addSon(ret)

  # pragma
  if pos < fexpr.len and $fexpr[pos] == "$":
    newfexpr.addSon(fexpr[pos])
    pos.inc
    if pos >= fexpr.len or fexpr[pos].kind != fexprArray:
      fexpr.error("fn syntax expect function pragma after `$.")
    newfexpr.addSon(fexpr[pos])
    pos.inc
  else:
    newfexpr.addSon(fprefix(fexpr.span, istring("$")))
    newfexpr.addSon(farray(fexpr.span))

  # body
  if pos < fexpr.len and fexpr[pos].kind == fexprBlock:
    newfexpr.addSon(fexpr[pos])
    pos.inc
  else:
    newfexpr.addSon(fblock(fexpr.span))

  return newfexpr

proc expandDeftype*(fexpr: FExpr): FExpr =
  let newfexpr = fseq(fexpr.span)
  newfexpr.metadata = fexpr.metadata
  newfexpr.addSon(fexpr[0])
  var pos = 1

  # name
  if pos >= fexpr.len or fexpr[pos].kind notin {fexprIdent, fexprSymbol}:
    fexpr.error("type syntax expect name.")
  newfexpr.addSon(fexpr[pos])
  pos.inc

  # generics
  if pos < fexpr.len and fexpr[pos].kind == fexprArray:
    newfexpr.addSon(fexpr[pos])
    pos.inc
  else:
    newfexpr.addSon(farray(fexpr.span))

  # pragma
  if pos < fexpr.len and $fexpr[pos] == "$":
    newfexpr.addSon(fexpr[pos])
    pos.inc
    if pos >= fexpr.len or fexpr[pos].kind != fexprArray:
      fexpr.error("type syntax expect pragma after `$.")
    newfexpr.addSon(fexpr[pos])
    pos.inc
  else:
    newfexpr.addSon(fprefix(fexpr.span, istring("$")))
    newfexpr.addSon(farray(fexpr.span))

  # body
  if pos < fexpr.len and fexpr[pos].kind == fexprBlock:
    newfexpr.addSon(fexpr[pos])
    pos.inc
  else:
    newfexpr.addSon(fblock(fexpr.span))
  
  return newfexpr

#
# C pragmas
#

proc semImportc*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len == 1 or fexpr[1].kind != fexprStrLit:
    let name = istring($fexpr[1][1])
    fexpr[1].metadata.importc = some(name)
  else:
    let name = fexpr[1].strval
    fexpr[2].metadata.importc = some(name)

proc semHeader*(scope: Scope, fexpr: var FExpr) =
  if fexpr.kind in fexprContainer and fexpr.len == 3:
    if $fexpr[1] == "nodeclc":
      fexpr[2].metadata.header = none(IString)
    elif fexpr[1].kind == fexprStrLit:
      let name = fexpr[1].strval
      fexpr[2].metadata.header = some(name)
    else:
      fexpr[1].error("header argument should be FStrLit")
  else:
    fexpr.error("usage: `header \"headername.h\"`")

proc semExportc*(scope: Scope, fexpr: var FExpr) =
  if fexpr.kind in fexprContainer and fexpr.kind == fexprIdent:
    let name = istring($fexpr[2][1])
    fexpr[2].metadata.exportc = some(name)
  else:
    if fexpr[1].kind != fexprStrLit:
      fexpr[1].error("exportc argument should be FStrLit")
    let name = fexpr[1].strval
    fexpr[2].metadata.exportc = some(name)
    
proc semDeclc*(scope: Scope, fexpr: var FExpr) =
  if fexpr.kind in fexprContainer and fexpr.len == 3:
    if fexpr[1].kind == fexprStrLit:
      fexpr[2].metadata.declc = some(fexpr[1].strval)
    else:
      fexpr[1].error("unsupported in declc pragma")
  else:
    fexpr.error("usage: `declc \"#1($1)\"``")

proc semPatternc*(scope: Scope, fexpr: var FExpr) =
  if fexpr.kind in fexprContainer and fexpr.len == 3:
    if $fexpr[1] == "infixc":
      fexpr[2].metadata.infixc = true
    elif fexpr[1].kind == fexprStrLit:
      fexpr[2].metadata.patternc = some(fexpr[1].strval)
    else:
      fexpr[1].error("unsupported in patternc pragma")
  else:
    fexpr.error("usage: `patternc \"#1($1)\"` or `patternc infixc`")

#
# JS pragmas
#

proc semImportjs*(scope: Scope, fexpr: var FExpr) =
  if fexpr.kind in fexprContainer and fexpr.len == 1 or fexpr[1].kind != fexprStrLit and fexpr[1].kind in fexprContainer:
    let name = istring($fexpr[1].fnName)
    fexpr[1].metadata.importjs = some(name)
  else:
    let name = fexpr[1].strval
    fexpr[2].metadata.importjs = some(name)

proc semExportjs*(scope: Scope, fexpr: var FExpr) =
  if fexpr.kind in fexprContainer and fexpr.kind == fexprIdent:
    let name = istring($fexpr[2][1])
    fexpr[2].metadata.exportjs = some(name)
  else:
    if fexpr[1].kind != fexprStrLit:
      fexpr[1].error("exportjs argument should be FStrLit")
    let name = fexpr[1].strval
    fexpr[2].metadata.exportjs = some(name)

proc semPatternjs*(scope: Scope, fexpr: var FExpr) =
  if fexpr.kind in fexprContainer and fexpr.len == 3:
    if $fexpr[1] == "infixjs":
      fexpr[2].metadata.infixjs = true
    elif fexpr[1].kind == fexprStrLit:
      fexpr[2].metadata.patternjs = some(fexpr[1].strval)
    else:
      fexpr[1].error("unsupported in patternjs pragma")
  else:
    fexpr.error("usage: `patternjs \"#1($1)\"` or `patternjs infixc`")

#
# General pragmas
#
#
proc semCompiletime*(scope: Scope, fexpr: var FExpr) =
  fexpr[1].metadata.compiletime = true
    
proc semConverter*(scope: Scope, fexpr: var FExpr) =
  if fexpr[1].fnArguments.len != 1:
    fexpr.error("converter fn should be 1 argument.")
  let fromtype = fexpr[1].fnArguments[0][1].symbol
  fromtype.fexpr.obj.metadata.obj.converters.add(fexpr[1])

proc semPragma*(scope: Scope, fexpr: FExpr, pragma: FExpr) =
  if pragma.kind != fexprArray:
    pragma.error("$# isn't internal pragma." % $pragma)

  for key in pragma.mitems:
    let pragmaname = if key.kind in fexprContainer:
                       $key[0]
                     else:
                       $key
    var newkey = if key.kind in fexprContainer:
                   key.copy
                 else:
                   fseq(key.span, @[key])
    let internalopt = scope.getFunc(procname(pragmaname, @[]))
    newkey.addSon(fexpr)
    if internalopt.isSome:
      (internalopt.get.pd.internalproc.get)(scope, newkey)
    else:
      scope.rootPass(newkey)

#
# Evaluater
#

proc declGenerics*(scope: Scope, fexpr: FExpr, decl: bool): seq[Symbol] =
  result = @[]
  for g in fexpr.mitems:
    if g.kind == fexprSymbol:
      result.add(g.symbol)
    else:
      let sym = scope.symbol(istring($g), symbolGenerics, g)
      let status = scope.addDecl(istring($g), sym)
      if not status:
        g.error("redefinition $# generics." % $g)
      g = fsymbol(g.span, sym)
      result.add(sym)

proc declArgtypes*(scope: Scope, originscope: Scope, fexpr: FExpr): seq[Symbol] =
  result = @[]
  for i, arg in fexpr.mpairs:
    if arg.len < 2:
      arg.error("$# isn't argument declaration." % $arg)
    if arg[1].kind == fexprSymbol:
      result.add(arg[1].symbol)
    else:
      if arg.len != 2:
        let argtyp = fseq(arg.span, toSeq(arg.items)[1..^1])
        arg = fseq(arg.span, @[arg[0], argtyp])
      let typesym = scope.semType(arg[1])
      arg[0] = fsymbol(arg[0].span, scope.symbol(istring(name(arg[0])), symbolDef, arg[0]))
      arg[1] = fsymbol(arg[1].span, typesym)
      result.add(typesym)
        
proc semFunc*(scope: Scope, fexpr: var FExpr, defsym: SymbolKind, decl: bool): (Scope, seq[Symbol], seq[Symbol], Symbol, Symbol) =
  let fnscope = scope.extendScope()
  let generics = fnscope.declGenerics(fexpr.fnGenerics, true)
  let argtypes = fnscope.declArgtypes(scope, fexpr.fnArguments)
  let rettype = fnscope.semType(fexpr.fnReturn)
  fexpr.fnReturn = fsymbol(fexpr.fnReturn.span, rettype)
  
  let symkind = if defsym == symbolMacro:
                  defsym
                elif fexpr.fnName.kind == fexprQuote:
                  symbolInfix
                elif fexpr.fnName.kind == fexprSymbol:
                  fexpr.fnName.symbol.kind
                else:
                  defsym
  let sym = scope.symbol(istring(name(fexpr.fnName)), symkind, fexpr)

  if decl:
    let pd = ProcDecl(name: istring(name(fexpr.fnName)), argtypes: iarray(argtypes), generics: iarray(generics), returntype: rettype, sym: sym)
    if fexpr.metadata.isExpanded:
      scope.top.addSpecFunc(pd)
      fnscope.top.addSpecFunc(pd)
    else:
      discard scope.top.addFunc(pd)
      discard fnscope.top.addFunc(pd)

  let fsym = fsymbol(fexpr[0].span, sym)
  fexpr.fnName = fsym
  semPragma(scope, fexpr, fexpr.fnPragma)
  fexpr.metadata.scope = fnscope
  if fexpr.isGenerics:
    return (fnscope, generics, argtypes, rettype, sym)

  for i, arg in fexpr.fnArguments:
    let argtsym = scope.symbol(istring($arg[0]), symbolArg, arg[0])
    if not fnscope.addDecl(istring($arg[0]), argtsym):
      for d in fnscope.decls:
        echo d.name
      arg[0].error("redefinition $# argument." % $arg[0])
    arg[0] = fsymbol(arg[0].span, argtsym)
    arg[0].symbol.fexpr.metadata.typ = argtypes[i]
    arg[1] = fsymbol(arg[0].span, argtypes[i])
  fexpr.metadata.scope = fnscope
  # scope.resolveByVoid(fexpr)

  # semPragma(scope, fexpr, fexpr.fnPragma)
  fnscope.rootPass(fexpr.fnBody)
  if fexpr.fnBody.len != 0:
    if not fexpr.fnBody.metadata.typ.spec(rettype):
      echo fexpr.fnBody[^1], ":", fexpr.fnBody[^1].metadata.typ
      fexpr.fnBody.error("function expect $# return type, actually $#" % [$rettype, $fexpr.fnBody.metadata.typ])
  
  return (fnscope, generics, argtypes, rettype, sym)

proc semDefn*(scope: Scope, fexpr: var FExpr) =
  fexpr = expandDefn(fexpr)
  fexpr.metadata.internal = internalDefn
  let (fnscope, generics, argtypes, rettype, sym) = semFunc(scope, fexpr, symbolFunc, true)

  # let pd = ProcDecl(isInternal: false, name: istring(parsed.name), argtypes: argtypes, generics: generics, returntype: rettype, sym: sym, fexpr: fexpr)
  # if not scope.addFunc(pd):
  #   fexpr.error("redefinition $# function." % $parsed.name)

proc semSyntax*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len == 2:
    fexpr[1].metadata.isSyntax = true
    return
  fexpr = expandDefn(fexpr)
  fexpr.metadata.internal = internalMacro
  
  let (fnscope, generics, argtypes, rettype, sym) = semFunc(scope, fexpr, symbolSyntax, false)

  let mp = genMacroProc(MacroProcObj(importname: istring(codegenMangling(sym, @[], argtypes)))) # FIXME: support generics
  let pd = ProcDecl(isSyntax: true, macroproc: mp, name: istring($fexpr.fnName), argtypes: iarray(argtypes), generics: iarray(generics), returntype: rettype, sym: sym)
  if not scope.addFunc(pd):
    fexpr.error("redefinition $# macro." % $fexpr.fnName)

  gCtx.globaltoplevels.add(fexpr)
  gCtx.macroprocs.add(mp)
  gCtx.reloadMacroLibrary(scope.top)
  gCtx.globaltoplevels.del(high(gCtx.globaltoplevels))
  
  if not fexpr.metadata.isToplevel:
    gCtx.globaltoplevels.add(fexpr)

proc semMacro*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len == 2:
    fexpr[1].metadata.isSyntax = true
    return
  fexpr = expandDefn(fexpr)
  fexpr.metadata.internal = internalMacro
  let (fnscope, generics, argtypes, rettype, sym) = semFunc(scope, fexpr, symbolMacro, false)

  let mp = genMacroProc(MacroProcObj(importname: istring(codegenMangling(sym, @[], argtypes) & "_macro"))) # FIXME: support generics
  sym.macroproc = mp
  let pd = ProcDecl(isMacro: true, macroproc: mp, name: istring($fexpr.fnName), argtypes: iarray(argtypes), generics: iarray(generics), returntype: rettype, sym: sym)
  let status = scope.top.addFunc(pd)
  if not status:
    fexpr.error("redefinition $# macro." % $fexpr.fnName)

  let delpos = gCtx.globaltoplevels.len
  gCtx.globaltoplevels.add(fexpr)
  if fexpr.fnGenerics.isSpecTypes:
    gCtx.macroprocs.add(mp)
    gCtx.reloadMacroLibrary(scope.top)
  gCtx.globaltoplevels.del(delpos)
  
  if not fexpr.metadata.isToplevel:
    gCtx.globaltoplevels.add(fexpr)
    
proc semDeftype*(scope: Scope, fexpr: var FExpr) =
  fexpr = expandDeftype(fexpr)
  fexpr.metadata.internal = internalDeftype

  let typename = istring(name(fexpr.typeName))
  let sym = scope.symbol(typename, if fexpr.typeGenerics.len != 0: symbolTypeGenerics else: symbolType, fexpr)
  if fexpr.fnGenerics.len != 0 and fexpr.typeGenerics.isSpecTypes:
    sym.types = iarray(fexpr.fnGenerics.mapIt(it.symbol))
  if not fexpr.metadata.isExpanded and not scope.addDecl(typename, sym):
    fexpr.error("redefinition $# type." % $typename)
    
  let fsym = fsymbol(fexpr[0].span, sym)
  fexpr.fnName = fsym
  semPragma(scope, fexpr, fexpr.typePragma)

  if fexpr.isGenerics:
    return
  
  let typescope = scope.extendScope()
  for field in fexpr.typeBody.mitems:
    if field.len < 2:
      field.error("$# isn't field declaration." % $field)
    let ftyp = fseq(field.span, toSeq(field.items)[1..^1])
    field = fseq(field.span, @[field[0], ftyp])
    let s = typescope.semType(field[1])
    field[1].replaceByTypesym(s)
  fexpr.metadata.scope = typescope

  if fexpr.metadata.importc.isNone:
    sym.fexpr.metadata.isCStruct = true

proc semTypedef*(scope: Scope, fexpr: var FExpr) =
  let ftyp = fseq(fexpr[2].span, toSeq(fexpr.items)[2..^1])
  let newfexpr = fseq(fexpr.span, @[fexpr[0], fexpr[1], ftyp])
  newfexpr.metadata = fexpr.metadata
  fexpr = newfexpr
  let typesym = scope.semType(fexpr[2])
  if not scope.addDecl(istring($fexpr[1]), typesym):
    fexpr.error("redefinition $# type." % $fexpr[1])
  fexpr[2] = fsymbol(fexpr[2].span, typesym)
  fexpr.metadata.internal = internalTypedef

proc semIf*(scope: Scope, fexpr: var FExpr) =
  var branchtypes = newSeq[Symbol]()
  var isret = true
  fexpr.metadata.internal = internalIf

  let branches = getIfBranches(fexpr)

  for branch in branches:
    let bscope = scope.extendScope()
    if branch.cond.isSome:
      bscope.rootPass(fexpr[branch.cond.get])
      if not fexpr[branch.cond.get].metadata.typ.isBoolType:
        fexpr[branch.cond.get].error("if expression cond type should be Bool.")
    bscope.rootPass(fexpr[branch.body])
    if fexpr[branch.body].len != 0:
      branchtypes.add(fexpr[branch.body][^1].metadata.typ)
    else:
      isret = false

  if isret and branchtypes.isEqualTypes:
    fexpr.metadata.typ = branchtypes[0]
  else:
    scope.resolveByVoid(fexpr)

proc semWhile*(scope: Scope, fexpr: var FExpr) =
  scope.rootPass(fexpr[1])
  if not fexpr[1][0].metadata.typ.isBoolType:
    fexpr[1].error("while statement cond type chould be Bool.")
  let bodyscope = scope.extendScope()
  bodyscope.rootPass(fexpr[2])
  scope.resolveByVoid(fexpr)
  fexpr.metadata.scope = bodyscope
  fexpr.metadata.internal = internalWhile

proc semVar*(scope: Scope, fexpr: var FExpr) =
  let n = fexpr[1]
  if n.kind != fexprIdent:
    n.error("variable name should be FIdent.")
  
  let ftyp = fseq(fexpr[2].span, toSeq(fexpr.items)[2..^1])
  let newfexpr = fseq(fexpr.span, @[fexpr[0], fexpr[1], ftyp])
  newfexpr.metadata = fexpr.metadata
  fexpr = newfexpr

  let typsym = if fexpr[2].kind == fexprSymbol:
                 fexpr[2].symbol
               else:
                 scope.semType(fexpr[2])
  
  let varsym = scope.symbol(istring($n), symbolDef, n)
  n.metadata.typ = typsym.scope.varsym(typsym)
  let status = scope.addDecl(istring($n), varsym)
  if not status:
    fexpr.error("redefinition $# variable." % $n)
  
  let fsym = fsymbol(fexpr[1].span, varsym)
  let oldfexpr = fexpr
  fexpr = fexpr.span.quoteFExpr("var `embed `embed", [fsym, fsymbol(fexpr[2].span, typsym)])
  fexpr.metadata = oldfexpr.metadata
  fexpr.metadata.internal= internalVar
  scope.resolveByVoid(fexpr)

proc semConst*(scope: Scope, name: var FExpr, value: var FExpr) =
  if name.kind != fexprIdent:
    name.error("variable name should be FIdent.")
  
  value = value.span.quoteFExpr("const_eval(`embed)", [value])
  scope.rootPass(value)
  value = value[^1]

  let csym = scope.symbol(istring($name), symbolDef, name)
  name.metadata.internal = internalConst
  name.metadata.constvalue = value
  name.metadata.typ = value.metadata.typ
  name = fsymbol(name.span, csym)
  name.metadata.typ = value.metadata.typ
  let status = scope.addDecl(istring($name), csym)
  if not status:
    name.error("redefinition $# const." % $name)
  
proc semDef*(scope: Scope, fexpr: var FExpr) =
  if fexpr[1].kind == fexprSeq and fexpr[1].len != 3:
    let defmode = fexpr[1][0]
    if $defmode == "const":
      semConst(scope, fexpr[1][1], fexpr[2])
      fexpr.metadata.internal = internalConst
      return
    else:
      defmode.error("$# is unknwon def mode, please specify `const." % $defmode)
    
  if fexpr[1].kind notin {fexprIdent, fexprSeq}:
    fexpr[1].error("variable name should be FIdent.")
  let name = if fexpr[1].kind == fexprIdent:
               fexpr[1]
             else:
               fexpr[1][0]

  if fexpr[1].kind == fexprSeq and fexpr[1][1].isPragmaPrefix:
    semPragma(scope, fexpr, fexpr[1][2])

  scope.rootPass(fexpr[2])
  if fexpr[2].metadata.typ.isVoidType:
    fexpr[2].error("value is Void.")
  elif $fexpr[2].metadata.typ.name == "IntLit":
    fexpr[2] = fexpr[2].span.quoteFExpr("int(`embed)", [fexpr[2]])
    scope.rootPass(fexpr[2])
  elif $fexpr[2].metadata.typ.name == "FloatLit":
    fexpr[2] = fexpr[2].span.quoteFExpr("double(`embed)", [fexpr[2]])
    scope.rootPass(fexpr[2])
  elif $fexpr[2].metadata.typ.name == "StrLit":
    fexpr[2] = fexpr[2].span.quoteFExpr("cstring(`embed)", [fexpr[2]])
    scope.rootPass(fexpr[2])
    
  scope.resolveByVoid(fexpr)

  let varsym = scope.symbol(istring($name), symbolDef, fexpr[1])
  fexpr[1].metadata.typ = scope.varsym(fexpr[2].metadata.typ)
  fexpr[1].metadata.scope = scope
  let status = scope.addDecl(istring($name), varsym)
  if not status:
    fexpr.error("redefinition $# variable." % $name)

  let fsym = fsymbol(fexpr[1].span, varsym)
  if fexpr[1].kind == fexprIdent:
    fexpr[1] = fsym
  else:
    fexpr[1][0] = fsym
  fexpr.metadata.internal = internalDef
  
proc semSet*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len != 3:
    fexpr.error("expected set syntax: left = value")

  scope.resolveByVoid(fexpr)
  scope.rootPass(fexpr[1])
  scope.rootPass(fexpr[2])
  
  if fexpr[1].isInfixFuncCall and $fexpr[1][0] == "!" and scope.isSetter(fexpr[1][1].metadata.typ, fexpr[1][2].metadata.typ, fexpr[2].metadata.typ):
    fexpr = fexpr.span.quoteFExpr("`!!(`embed, `embed, `embed)", [fexpr[1][1], fexpr[1][2], fexpr[2]])
    scope.rootPass(fexpr)
    return

  if not fexpr[1].hasTyp:
    fexpr[1].error("value hasn't type.")
  if not fexpr[2].hasTyp:
    fexpr[2].error("value hasn't type.")
  if fexpr[1].metadata.typ.kind != symbolVar and fexpr[1].metadata.typ.kind != symbolRef:
    fexpr[1].error("ref value expected.")

  let matched = fexpr[1].metadata.typ.match(fexpr[2].metadata.typ)
  if not matched.isMatch:
    fexpr[2].error("cannot set $# value to $#." % [$fexpr[2].metadata.typ, $fexpr[1].metadata.typ])
  if matched.kind == matchConvert:
    fexpr[2] = fsymbol(fexpr[2].span, matched.convsym)
    scope.rootPass(fexpr[2])

  fexpr.metadata.internal = internalSet

proc semModulePrefix*(scope: Scope, fexpr: var FExpr) =
  let opt = scope.imports.find($fexpr[1])
  if opt.isNone:
    fexpr.error("undeclared $# identifier" % $fexpr[1])
  let module = opt.get
  
  if fexpr[2].isNormalFuncCall:
    scope.rootPass(fexpr[2][1])
    let argtypes = fexpr[2][1].mapIt(it.metadata.typ)
    let opt = module.getFunc(procname($fexpr[2][0], argtypes), importscope = false)
    if opt.isNone:
      fexpr.error("undeclared $#.$#($#) function." % [$fexpr[1], $fexpr[2][0], argtypes.mapIt($it).join(", ")])
    fexpr = fexpr.span.quoteFExpr("`embed `embed", [fsymbol(fexpr.span, opt.get.pd.sym), genConvertedCall(fexpr[2][1], opt.get.matches)])
    fexpr.metadata.typ = opt.get.pd.returntype
  elif fexpr[2].kind == fexprIdent:
    let opt = module.getDecl($fexpr[2], importscope = false)
    if opt.isNone:
      fexpr.error("undeclared $#.$# identifier." % [$fexpr[1], $fexpr[2]])
    fexpr = fsymbol(fexpr.span, opt.get)
    scope.rootPass(fexpr)
  else:
    fexpr.error("unknown `. syntax: $#" % $fexpr)
  
proc semFieldAccess*(scope: Scope, fexpr: var FExpr) =
  let fieldname = fexpr[2]
  if fieldname.kind != fexprIdent:
    fieldname.error("field name should be FIdent.")
  scope.rootPass(fexpr[1])
  if not fexpr[1].hasTyp:
    fexpr[1].error("value hasn't type.")
  let fieldopt = fexpr[1].metadata.typ.fexpr.getFieldType($fieldname)
  if fieldopt.isNone:
    fieldname.error("$# hasn't $# field." % [$fexpr[1].metadata.typ, $fieldname])
  if fexpr[1].metadata.typ.kind == symbolRef:
    fexpr.metadata.typ = fieldopt.get.scope.varsym(fieldopt.get)
  elif fexpr[1].metadata.typ.kind == symbolVar:
    fexpr.metadata.typ = fieldopt.get.scope.varsym(fieldopt.get)
  else:
    fexpr.metadata.typ = fieldopt.get

  fexpr.metadata.internal = internalFieldAccess

proc semDot*(scope: Scope, fexpr: var FExpr) =
  if fexpr[1].kind == fexprIdent:
    let opt = scope.getDecl($fexpr[1])
    if opt.isNone:
      scope.semModulePrefix(fexpr)
    else:
      scope.semFieldAccess(fexpr)
  else:
    scope.semFieldAccess(fexpr)
  
proc semInit*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len != 3:
    fexpr.error("init require 2 arguments.")
  if fexpr[1].kind != fexprList:
    fexpr.error("init type should be FList.")
  if fexpr[1].len != 1:
    fexpr.error("init type should be single argument.")
  if fexpr[2].kind != fexprBlock:
    fexpr.error("init body should be FBlock.")
    
  let typesym = scope.semType(fexpr[1][0])
  fexpr[1][0].replaceByTypesym(typesym)
  fexpr.metadata.typ = typesym
  scope.rootPass(fexpr[2])

  let argtypes = fexpr[2].mapIt(it.metadata.typ)
  let fields = typesym.fexpr.typeBody
  if fields.len != argtypes.len:
    fexpr.error("init $# expected $# arguments, but got $#" % [$typesym, $fields.len, $argtypes.len])
  for i, b in fexpr[2]:
    let fieldtyp = fields[i][1]
    if not match(fieldtyp.symbol, b.metadata.typ).isMatch:
      b.error("init field type mismatch: expected $#, but got $#" % [$fieldtyp, $b.metadata.typ])

  var newfexpr = fblock(fexpr.span)
  let tmpid = fident(fexpr.span, istring(gCtx.genTmpname()))
  newfexpr.addSon(fexpr.span.quoteFExpr("var `embed `embed", [tmpid, fsymbol(fexpr.span, typesym)]))
  for i, b in fexpr[2]:
    let fieldname = fields[i][0]
    newfexpr.addSon(b.span.quoteFExpr("`embed.`embed = `embed", [tmpid, fieldname, b]))
  newfexpr.addSon(tmpid)
  fexpr = newfexpr
  scope.rootPass(fexpr)

proc semImport*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len != 2:
    fexpr.error("import syntax require filepath.")
  if fexpr[1].kind != fexprStrLit:
    fexpr.error("import syntax filepath should be FStrLit.")
  let importname = fexpr[1].strval
  let filepath = ($importname).replace(".", "/")
  var modname = gCtx.semFile(filepath & ".flori")
  if modname.isNone:
    modname = gCtx.semFile(filepath / "root.flori")
    if modname.isNone:
      fexpr.error("cannot import $#" % $fexpr[1])
  let opt = gCtx.modules.find($modname.get)
  scope.top.importScope(importname, opt.get)
  fexpr.metadata.internal = internalImport

proc semExport*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len != 2:
    fexpr.error("export syntax require filepath.")
  if fexpr[1].kind != fexprStrLit:
    fexpr.error("export syntax filepath should be FStrLit.")
  let exportname = istring(($fexpr[1].strval).replace("/", "."))
  let opt = gCtx.modules.find($exportname)
  if opt.isNone:
    var importexpr = fexpr.span.quoteFExpr("import `embed", [fexpr[1]])
    scope.rootPass(importexpr)
  let moduleopt = gCtx.modules.find($exportname)
  scope.top.obj.exports.add(TupleTable[Scope](name: exportname, value: moduleopt.get))
  fexpr.metadata.internal = internalExport

proc collectQuotedItems*(fexpr: FExpr, collected: var seq[FExpr]) =
  for son in fexpr:
    if son.kind == fexprQuote and son.quoted.kind == fexprIdent:
      collected.add(son.quoted)
    elif son.kind in fexprContainer:
      collectQuotedItems(son, collected)

proc semQuote*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len != 2:
    fexpr.error("quote expected fblock.")
  if fexpr[1].kind != fexprBlock:
    fexpr[1].error("quote expected FBlock, actually $#" % $fexpr[1].kind)
  let fstr = fstrlit(fexpr[1].span, istring(($fexpr[1]).replace("\n", ";").replace("\"", "\\\"").replace("\\n", "\\\\n")))

  let ret = fblock(fexpr.span)
  let tmpid = fident(fexpr.span, istring(gCtx.genTmpname()))
  ret.addSon(fexpr.span.quoteFExpr("`embed := new_farray()", [tmpid]))
  var collected = newSeq[FExpr]()
  collectQuotedItems(fexpr[1], collected)
  for c in collected:
    ret.addSon(genCall(fident(fexpr.span, istring("push")), tmpid, c)) # push(tmpid, c)

  ret.addSon(fexpr.span.quoteFExpr("quote_expand(parse(\"$#\", $#, $#, `embed), `embed)" % [$fexpr.span.filename, $fexpr.span.line, $fexpr.span.linepos], [fstr, tmpid]))
  fexpr = ret
  scope.rootPass(fexpr)

proc semCodegenDecl*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len != 2:
    fexpr.error("usage: cemit \"...\"")
  if fexpr[1].kind != fexprStrLit:
    fexpr.error("usage: cemit \"...\"")
  fexpr.metadata.internal = internalCodegenDecl
proc semCodegenHead*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len != 2:
    fexpr.error("usage: cemit \"...\"")
  if fexpr[1].kind != fexprStrLit:
    fexpr.error("usage: cemit \"...\"")
  fexpr.metadata.internal = internalCodegenHead

proc semBlock*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len != 2:
    fexpr.error("usage: block {...}")
  if fexpr[1].kind != fexprBlock:
    fexpr.error("usage: block {...}")
  let blockscope = scope.extendScope()
  blockscope.rootPass(fexpr[1])
  fexpr.metadata.scope = blockscope
  fexpr.metadata.internal = internalBlock
    
#
# Internal
#
  
proc addInternalEval*(scope: Scope, n: IString, p: proc (scope: Scope, fexpr: var FExpr)) =
  let status = scope.addFunc(ProcDecl(
    internalproc: some(p),
    name: n,
    argtypes: iarray[Symbol](),
    generics: iarray[Symbol](),
    sym: scope.symbol(n, symbolInternal, fident(internalSpan, istring("internal")))
  ))
  if not status:
    fseq(internalSpan).error("redefinition $# function." % $n)
  
proc initInternalEval*(scope: Scope) =
  scope.addInternalEval(istring("fn"), semDefn)
  scope.addInternalEval(istring("syntax"), semSyntax)
  scope.addInternalEval(istring("macro"), semMacro)
  scope.addInternalEval(istring("type"), semDeftype)
  scope.addInternalEval(istring("typedef"), semTypedef)
  scope.addInternalEval(istring("if"), semIf)
  scope.addInternalEval(istring("while"), semWhile)
  scope.addInternalEval(istring("var"), semVar)
  scope.addInternalEval(istring(":="), semDef)
  scope.addInternalEval(istring("="), semSet)
  scope.addInternalEval(istring("."), semDot)
  scope.addInternalEval(istring("init"), semInit)
  scope.addInternalEval(istring("import"), semImport)
  scope.addInternalEval(istring("export"), semExport)
  scope.addInternalEval(istring("quote"), semQuote)
  scope.addInternalEval(istring("codegen_decl"), semCodegenDecl)
  scope.addInternalEval(istring("codegen_head"), semCodegenHead)
  scope.addInternalEval(istring("block"), semBlock)

  # c pragmas
  scope.addInternalEval(istring("importc"), semImportc)
  scope.addInternalEval(istring("header"), semHeader)
  scope.addInternalEval(istring("exportc"), semExportc)
  scope.addInternalEval(istring("patternc"), semPatternc)
  scope.addInternalEval(istring("declc"), semDeclc)
  # js pragmas
  scope.addInternalEval(istring("importjs"), semImportjs)
  scope.addInternalEval(istring("exportjs"), semExportjs)
  scope.addInternalEval(istring("patternjs"), semPatternjs)
  # general pragmas
  scope.addInternalEval(istring("compiletime"), semCompiletime)
  scope.addInternalEval(istring("converter"), semConverter)

proc initInternalScope*(ctx: var SemContext) =
  let scope = newScope(istring("internal"), "internal")
  scope.initInternalEval()
  ctx.modules.add(TupleTable[Scope](name: istring("internal"), value: scope))

proc internalScope*(ctx: SemContext): Scope =
  let opt = ctx.modules.find("internal")
  opt.get

proc newSemContext*(moptions = "", defines = newSeq[string]()): SemContext =
  result.modules = ilistNil[TupleTable[Scope]]()
  result.macrolib = nil
  result.macroprocs = @[]
  result.tmpcount = 0
  result.initInternalScope()
  result.importpaths = iarray(@[istring(getAppDir() / ".."), istring("."), istring(getHomeDir() / ".rabbit")])
  result.moptions = moptions
  result.defines = defines
  result.globaltoplevels = @[]
  result.expands = @[]
  rootPass = processSemPass
  gCtx = result

proc semModule*(ctx: var SemContext, name: IString, scope: Scope, fexprs: var seq[FExpr]) =
  let opt = ctx.modules.find($name)
  if opt.isSome:
    return
  scope.importScope(istring("internal"), ctx.internalScope)
  for f in fexprs.mitems:
    f.metadata.isToplevel = true
    scope.rootPass(f)
    ctx.globaltoplevels.add(f)
  ctx.modules.add(TupleTable[Scope](name: name, value: scope))

proc semFile*(ctx: var SemContext, filepath: string): Option[IString] =
  for importpath in ctx.importpaths:
    if existsFile($importpath / filepath):
      var fexprs = parseToplevel(filepath, readFile($importpath / filepath))
      let (dir, n, _) = filepath.splitFile()
      let modname = if dir != "":
                      istring(dir.replace("/", ".") & "." & n)
                    else:
                      istring(n)
      let scope = newScope(modname, $importpath / filepath)
      ctx.semModule(modname, scope, fexprs)
      return some(modname)
    elif existsFile($importpath / filepath / "root.flori"):
      var fexprs = parseToplevel(filepath, readFile($importpath / filepath / "root.flori"))
      let (dir, n, _) = filepath.splitFile()
      let modname = if dir != "":
                      istring(dir.replace("/", ".") & "." & n)
                    else:
                      istring(n)
      let scope = newScope(modname, $importpath / filepath / "root.flori")
      ctx.semModule(modname, scope, fexprs)
      return some(modname)
      
  return none(IString)
