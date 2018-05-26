
import fexpr_core
import passutils, typepass, sempass
import compileutils, macroffi
import passmacro

import options
import strutils, sequtils
import tables
import os

proc semFile*(ctx: SemanticContext, filepath: string): Option[Name]

#
# Parser
#

proc getFieldType*(fexpr: FExpr, fieldname: string): Option[Symbol] =
  if not fexpr.hasDeftype:
    fexpr.error("$# isn't structure type."  % $fexpr)
  for field in fexpr.deftype.body:
    if $field[0] == fieldname:
      return some(field[1].symbol)
  return none(Symbol)

proc parseDefn*(fexpr: var FExpr): Defn =
  let newfexpr = fseq(fexpr.span)
  newfexpr.metadata = fexpr.metadata
  newfexpr.addSon(fexpr[0])
  var pos = 1

  # name
  if pos >= fexpr.len or fexpr[pos].kind notin {fexprIdent, fexprSymbol, fexprQuote}:
    fexpr.error("fn syntax expect function name.")
  result.namepos = newfexpr.len
  newfexpr.addSon(fexpr[pos])
  pos.inc

  # generics
  if fexpr[pos].kind == fexprArray:
    result.genericspos = newfexpr.len
    newfexpr.addSon(fexpr[pos])
    pos.inc
  else:
    result.genericspos = newfexpr.len
    newfexpr.addSon(farray(fexpr.span))

  # args
  if pos >= fexpr.len or fexpr[pos].kind != fexprList:
    fexpr.error("fn syntax expect function arguments.")
  result.argspos = newfexpr.len
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
    ret.addSon(fident(fexpr.span, name("Void")))

  # ret generics
  if pos < fexpr.len and fexpr[pos].kind == fexprArray:
    result.retpos = newfexpr.len
    ret.addSon(fexpr[pos])
    newfexpr.addSon(ret)
    pos.inc
  else:
    result.retpos = newfexpr.len
    newfexpr.addSon(ret)

  # pragma
  if pos < fexpr.len and $fexpr[pos] == "$":
    newfexpr.addSon(fexpr[pos])
    pos.inc
    if pos >= fexpr.len or fexpr[pos].kind != fexprArray:
      fexpr.error("fn syntax expect function pragma after `$.")
    result.pragmapos = newfexpr.len
    newfexpr.addSon(fexpr[pos])
    pos.inc
  else:
    newfexpr.addSon(fprefix(fexpr.span, name("$")))
    result.pragmapos = newfexpr.len
    newfexpr.addSon(farray(fexpr.span))

  # body
  if pos < fexpr.len and fexpr[pos].kind == fexprBlock:
    result.bodypos = newfexpr.len
    newfexpr.addSon(fexpr[pos])
    pos.inc
  else:
    result.bodypos = newfexpr.len
    newfexpr.addSon(fblock(fexpr.span))

  result.fexpr = newfexpr
  fexpr = newfexpr

proc parseDeftype*(fexpr: var FExpr): Deftype =
  let newfexpr = fseq(fexpr.span)
  newfexpr.metadata = fexpr.metadata
  newfexpr.addSon(fident(fexpr.span, name("type")))
  var pos = 1

  # name
  if pos >= fexpr.len or fexpr[pos].kind notin {fexprIdent, fexprSymbol}:
    fexpr.error("type syntax expect name.")
  result.namepos = newfexpr.len
  newfexpr.addSon(fexpr[pos])
  pos.inc

  # generics
  if pos < fexpr.len and fexpr[pos].kind == fexprArray:
    result.genericspos = newfexpr.len
    newfexpr.addSon(fexpr[pos])
    pos.inc
  else:
    result.genericspos = newfexpr.len
    newfexpr.addSon(farray(fexpr.span))

  # pragma
  if pos < fexpr.len and $fexpr[pos] == "$":
    newfexpr.addSon(fexpr[pos])
    pos.inc
    if pos >= fexpr.len or fexpr[pos].kind != fexprArray:
      fexpr.error("type syntax expect pragma after `$.")
    result.pragmapos = newfexpr.len
    newfexpr.addSon(fexpr[pos])
    pos.inc
  else:
    newfexpr.addSon(fprefix(fexpr.span, name("$")))
    result.pragmapos = newfexpr.len
    newfexpr.addSon(farray(fexpr.span))

  # body
  if pos < fexpr.len and fexpr[pos].kind == fexprBlock:
    result.bodypos = newfexpr.len
    newfexpr.addSon(fexpr[pos])
    pos.inc
  else:
    result.bodypos = newfexpr.len
    newfexpr.addSon(fblock(fexpr.span))

  result.fexpr = newfexpr
  fexpr = newfexpr

proc parseIf*(fexpr: FExpr): IfExpr =
  if fexpr.len < 3:
    fexpr.error("if expression require greater than 3 arguments.")
  result.elifbranch = @[]
  result.elsebranch = fblock(fexpr.span)
  
  var pos = 1

  if fexpr[pos].kind != fexprList or fexpr[pos].len != 1:
    fexpr[pos].error("if cond should be single FList.")
  if fexpr[pos+1].kind != fexprBlock:
    fexpr[pos+1].error("if body should be FBlock.")
  result.elifbranch.add((fexpr[pos][0], fexpr[pos+1]))
  pos += 2

  while fexpr.len > pos:
    if $fexpr[pos] == "elif":
      pos.inc
      let cond = fexpr[pos]
      if cond.kind != fexprList or cond.len != 1:
        fexpr[pos].error("elif cond should be single FList.")
      pos.inc
      let body = fexpr[pos]
      if body.kind != fexprBlock:
        fexpr[pos].error("elif body should be FBlock.")
      pos.inc
      result.elifbranch.add((cond[0], body))
    else:
      pos.inc
      let body = fexpr[pos]
      if body.kind != fexprBlock:
        fexpr[pos].error("else body should be FBlock.")
      pos.inc
      result.elsebranch = body
      break

proc parseWhile*(fexpr: FExpr): WhileExpr =
  result.fexpr = fexpr
  if fexpr.len < 3:
    fexpr.error("while statement require arguments greater than 2")
  result.condpos = 1
  result.bodypos = 2
  if result.cond.kind != fexprList:
    result.cond.error("while cond should be FBlock.")
  if result.body.kind != fexprBlock:
    result.body.error("while body should be FBlock.")

proc parseDef*(fexpr: FExpr): DefExpr =
  result.fexpr = fexpr
  if fexpr.len != 3:
    fexpr.error("def expression require 2 arguments.")
  if fexpr[1].kind == fexprSeq and (fexpr[1].len == 2 or fexpr[1].len == 4):
    result.isPrefix = true
    result.namepos = 1
    result.valuepos = 2
  else:
    result.isPrefix = false
    result.namepos = 1
    result.valuepos = 2

#
# C pragmas
#

proc semImportc*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len == 1 or fexpr[1].kind != fexprStrLit:
    let name = $fexpr[1][1]
    fexpr[1].internalPragma.importc = some(name)
  else:
    let name = fexpr[1].strval
    fexpr[2].internalPragma.importc = some(name)

proc semHeader*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len == 3:
    if $fexpr[1] == "nodeclc":
      fexpr[2].internalPragma.header = none(string)
    elif fexpr[1].kind == fexprStrLit:
      let name = fexpr[1].strval
      fexpr[2].internalPragma.header = some(name)
    else:
      fexpr[1].error("header argument should be FStrLit")
  else:
    fexpr.error("usage: `header \"headername.h\"`")

proc semExportc*(scope: Scope, fexpr: var FExpr) =
  if fexpr.kind == fexprIdent:
    let name = $fexpr[2][1]
    fexpr[2].internalPragma.exportc = some(name)
  else:
    if fexpr[1].kind != fexprStrLit:
      fexpr[1].error("exportc argument should be FStrLit")
    let name = fexpr[1].strval
    fexpr[2].internalPragma.exportc = some(name)
    
proc semDeclc*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len == 3:
    if fexpr[1].kind == fexprStrLit:
      fexpr[2].internalPragma.declc = some(fexpr[1].strval)
    else:
      fexpr[1].error("unsupported in declc pragma")
  else:
    fexpr.error("usage: `declc \"#1($1)\"``")

proc semPatternc*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len == 3:
    if $fexpr[1] == "infixc":
      fexpr[2].internalPragma.infixc = true
    elif fexpr[1].kind == fexprStrLit:
      fexpr[2].internalPragma.patternc = some(fexpr[1].strval)
    else:
      fexpr[1].error("unsupported in patternc pragma")
  else:
    fexpr.error("usage: `patternc \"#1($1)\"` or `patternc infixc`")

#
# JS pragmas
#

proc semImportjs*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len == 1 or fexpr[1].kind != fexprStrLit:
    let name = $fexpr[1][1]
    fexpr[1].internalPragma.importjs = some(name)
  else:
    let name = fexpr[1].strval
    fexpr[2].internalPragma.importjs = some(name)

proc semExportjs*(scope: Scope, fexpr: var FExpr) =
  if fexpr.kind == fexprIdent:
    let name = $fexpr[2][1]
    fexpr[2].internalPragma.exportjs = some(name)
  else:
    if fexpr[1].kind != fexprStrLit:
      fexpr[1].error("exportjs argument should be FStrLit")
    let name = fexpr[1].strval
    fexpr[2].internalPragma.exportjs = some(name)

proc semPatternjs*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len == 3:
    if $fexpr[1] == "infixjs":
      fexpr[2].internalPragma.infixjs = true
    elif fexpr[1].kind == fexprStrLit:
      fexpr[2].internalPragma.patternjs = some(fexpr[1].strval)
    else:
      fexpr[1].error("unsupported in patternjs pragma")
  else:
    fexpr.error("usage: `patternjs \"#1($1)\"` or `patternjs infixc`")

#
# General pragmas
#
    
proc semInline*(scope: Scope, fexpr: var FExpr) =
  fexpr[1].internalPragma.inline = true
proc semCompiletime*(scope: Scope, fexpr: var FExpr) =
  fexpr[1].internalPragma.compiletime = true
proc semNoCompiletime*(scope: Scope, fexpr: var FExpr) =
  fexpr[1].internalPragma.nocompiletime = true
proc semConverter*(scope: Scope, fexpr: var FExpr) =
  if fexpr[1].defn.args.len != 1:
    fexpr.error("converter fn should be 1 argument.")
  let fromtype = fexpr[1].defn.args[0][1].symbol
  if not fromtype.fexpr.hasConverters:
    fromtype.fexpr.converters = Converters(converters: @[])
  fromtype.fexpr.converters.converters.add(fexpr[1])

proc semPragma*(scope: Scope, fexpr: FExpr, pragma: FExpr) =
  if pragma.kind != fexprArray:
    pragma.error("$# isn't internal pragma." % $pragma)
  fexpr.internalPragma = InternalPragma()

  for key in pragma.mitems:
    let pragmaname = if key.kind in fexprContainer:
                       name(key[0])
                     else:
                       name(key)
    var newkey = if key.kind in fexprContainer:
                   key.copy
                 else:
                   fseq(key.span, @[key])
    let internalopt = scope.getFunc(procname(pragmaname, @[]))
    newkey.addSon(fexpr)
    if internalopt.isSome:
      internalopt.get.pd.internalproc(scope, newkey)
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
      let sym = scope.symbol(name(g), symbolGenerics, g)
      let status = scope.addDecl(name(g), sym)
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
        arg = fseq(arg.span, @[arg[0], arg[1..^1]])
      let typesym = scope.semType(arg[1])
      arg[1] = fsymbol(arg[1].span, typesym)
      result.add(typesym)
        
proc semFunc*(scope: Scope, fexpr: var FExpr, parsed: Defn, defsym: SymbolKind, decl: bool): (Scope, seq[Symbol], seq[Symbol], Symbol, Symbol) =
  fexpr.defn = parsed
  let fnscope = scope.extendScope()
  let generics = fnscope.declGenerics(parsed.generics, not fexpr.isParsed)
  let argtypes = fnscope.declArgtypes(scope, parsed.args)
  let rettype = fnscope.semType(parsed.ret)
  parsed.ret = fsymbol(parsed.ret.span, rettype)
  
  let symkind = if defsym == symbolMacro:
                  defsym
                elif parsed.name.kind == fexprQuote:
                  symbolInfix
                else:
                  defsym
  let sym = scope.symbol(name(parsed.name), symkind, fexpr)

  if decl:
    let pd = ProcDecl(name: name(parsed.name), argtypes: argtypes, generics: generics, returntype: rettype, sym: sym, fexpr: fexpr)
    if fexpr.isParsed: # expanded
      scope.top.addSpecFunc(pd)
      fnscope.addSpecFunc(pd)
    else:
      discard scope.addFunc(pd)
      discard fnscope.addFunc(pd)

  if parsed.isGenerics:
    return (fnscope, generics, argtypes, rettype, sym)
  let fsym = fsymbol(fexpr[0].span, sym)
  parsed.name = fsym

  for i, arg in parsed.args:
    let argtsym = scope.symbol(name(arg[0]), symbolArg, arg[0])
    if not fnscope.addDecl(name(arg[0]), argtsym):
      arg[0].error("redefinition $# argument." % $arg[0])
    arg[0] = fsymbol(arg[0].span, argtsym)
    arg[0].symbol.fexpr.typ = argtypes[i]
    arg[1] = fsymbol(arg[0].span, argtypes[i])
  fexpr.internalScope = fnscope
  # scope.resolveByVoid(fexpr)

  semPragma(scope, fexpr, parsed.pragma)
  if parsed.generics.isSpecTypes and not fexpr.internalPragma.inline:
    fnscope.rootPass(parsed.body)
    if parsed.body.len != 0:
      if not parsed.body.typ.spec(rettype):
        parsed.body.error("function expect $# return type, actually $#" % [$rettype, $parsed.body.typ])
  
  return (fnscope, generics, argtypes, rettype, sym)

proc semDefn*(scope: Scope, fexpr: var FExpr) =
  var parsed = if fexpr.isParsed:
                 fexpr.defn
               else:
                 parseDefn(fexpr)
  let (fnscope, generics, argtypes, rettype, sym) = semFunc(scope, fexpr, parsed, symbolFunc, true)

  # let pd = ProcDecl(isInternal: false, name: name(parsed.name), argtypes: argtypes, generics: generics, returntype: rettype, sym: sym, fexpr: fexpr)
  # if not scope.addFunc(pd):
  #   fexpr.error("redefinition $# function." % $parsed.name)
  fexpr.internalMark = internalDefn

proc semSyntax*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len == 2:
    fexpr[1].internalPragma.isSyntax = true
    return
  
  var parsed = if fexpr.isParsed:
                 fexpr.defn
               else:
                 parseDefn(fexpr)
  let (fnscope, generics, argtypes, rettype, sym) = semFunc(scope, fexpr, parsed, symbolSyntax, false)

  let mp = MacroProc(importname: codegenMangling(sym, @[], argtypes)) # FIXME: support generics
  let pd = ProcDecl(isInternal: false, isSyntax: true, macroproc: mp, name: name(parsed.name), argtypes: argtypes, generics: generics, returntype: rettype, sym: sym, fexpr: fexpr)
  if not scope.addFunc(pd):
    fexpr.error("redefinition $# macro." % $parsed.name)
  fexpr.internalMark = internalMacro

  scope.ctx.globaltoplevels.add(fexpr)
  scope.ctx.macroprocs.add(mp)
  scope.ctx.reloadMacroLibrary(scope.top)
  scope.ctx.globaltoplevels.del(high(scope.ctx.globaltoplevels))
  
  if not fexpr.isToplevel:
    scope.ctx.globaltoplevels.add(fexpr)
    fexpr.isGenerated = true

proc semMacro*(scope: Scope, fexpr: var FExpr) =
  var parsed = if fexpr.isParsed:
                 fexpr.defn
               else:
                 parseDefn(fexpr)
  let (fnscope, generics, argtypes, rettype, sym) = semFunc(scope, fexpr, parsed, symbolMacro, false)

  let mp = MacroProc(importname: codegenMangling(sym, @[], argtypes) & "_macro") # FIXME: support generics
  sym.macroproc = mp
  let pd = ProcDecl(isInternal: false, isMacro: true, macroproc: mp, name: name(parsed.name), argtypes: argtypes, generics: generics, returntype: rettype, sym: sym, fexpr: fexpr)
  let status = scope.addFunc(pd)
  if not status:
    fexpr.error("redefinition $# macro." % $parsed.name)
  fexpr.internalMark = internalMacro

  let delpos = scope.ctx.globaltoplevels.len
  scope.ctx.globaltoplevels.add(fexpr)
  if parsed.generics.isSpecTypes:
    scope.ctx.macroprocs.add(mp)
    scope.ctx.reloadMacroLibrary(scope.top)
  scope.ctx.globaltoplevels.del(delpos)
  
  if not fexpr.isToplevel:
    scope.ctx.globaltoplevels.add(fexpr)
    fexpr.isGenerated = true
    
proc semDeftype*(scope: Scope, fexpr: var FExpr) =
  let parsed = if fexpr.isParsed:
                 fexpr.deftype
               else:
                 parseDeftype(fexpr)

  let typename = name(parsed.name)
  let sym = scope.symbol(typename, if parsed.generics.len != 0: symbolTypeGenerics else: symbolType, fexpr)
  if parsed.generics.len != 0 and parsed.generics.isSpecTypes:
    sym.types = parsed.generics.mapIt(it.symbol)
  if not fexpr.isParsed:
    if not scope.addDecl(typename, sym):
      fexpr.error("redefinition $# type." % $typename)
    
  fexpr.internalMark = internalDeftype
  fexpr.deftype = parsed
  let fsym = fsymbol(fexpr[0].span, sym)
  parsed.name = fsym

  if parsed.isGenerics:
    return
  
  semPragma(scope, fexpr, parsed.pragma)
  let typescope = scope.extendScope()
  for field in parsed.body.mitems:
    if field.len < 2:
      field.error("$# isn't field declaration." % $field)
    field = fseq(field.span, @[field[0], field[1..^1]])
    let s = typescope.semType(field[1])
    field[1].replaceByTypesym(s)
  fexpr.internalScope = typescope

  if fexpr.internalPragma.importc.isNone:    
    sym.fexpr.isCStruct = true

proc semTypedef*(scope: Scope, fexpr: var FExpr) =
  let newfexpr = fseq(fexpr.span, @[fexpr[0], fexpr[1], fexpr[2..^1]])
  newfexpr.metadata = fexpr.metadata
  fexpr = newfexpr
  let typesym = scope.semType(fexpr[2])
  if not scope.addDecl(name(fexpr[1]), typesym):
    fexpr.error("redefinition $# type." % $fexpr[1])
  fexpr[2] = fsymbol(fexpr[2].span, typesym)
  fexpr.internalMark = internalTypedef

proc semIf*(scope: Scope, fexpr: var FExpr) =
  var parsed = parseIf(fexpr)
  var branchtypes = newSeq[Symbol]()
  var isret = true

  for branch in parsed.elifbranch.mitems:
    let bscope = scope.extendScope()
    bscope.rootPass(branch.cond)
    if not branch.cond.typ.isBoolType:
      branch.cond.error("if expression cond type should be Bool.")
    bscope.rootPass(branch.body)
    if branch.body.len != 0:
      branchtypes.add(branch.body[^1].typ)
    else:
      isret = false
      
  let bscope = scope.extendScope()
  bscope.rootPass(parsed.elsebranch)
  if parsed.elsebranch.len != 0:
    branchtypes.add(parsed.elsebranch[^1].typ)
  else:
    isret = false

  if isret and branchtypes.isEqualTypes:
    fexpr.typ = branchtypes[0]
  else:
    scope.resolveByVoid(fexpr)

  fexpr.internalMark = internalIf
  fexpr.internalIfExpr = parsed

proc semWhile*(scope: Scope, fexpr: var FExpr) =
  var parsed = parseWhile(fexpr)
  scope.rootPass(parsed.cond)
  if not parsed.cond[0].typ.isBoolType:
    parsed.cond.error("while statement cond type chould be Bool.")
  let bodyscope = scope.extendScope()
  bodyscope.rootPass(parsed.body)
  scope.resolveByVoid(fexpr)
  fexpr.internalScope = bodyscope
  fexpr.internalMark = internalWhile
  fexpr.whileexpr = parsed

proc semVar*(scope: Scope, fexpr: var FExpr) =
  let n = fexpr[1]
  if n.kind != fexprIdent:
    n.error("variable name should be FIdent.")

  let newfexpr = fseq(fexpr.span, @[fexpr[0], fexpr[1], fexpr[2..^1]])
  newfexpr.metadata = fexpr.metadata
  fexpr = newfexpr

  let typsym = if fexpr[2].kind == fexprSymbol:
                 fexpr[2].symbol
               else:
                 scope.semType(fexpr[2])
  
  let varsym = scope.symbol(name(n), symbolDef, n)
  n.typ = typsym.scope.varsym(typsym)
  let status = scope.addDecl(name(n), varsym)
  if not status:
    fexpr.error("redefinition $# variable." % $n)
  scope.tracking(varsym.fexpr)
  
  let fsym = fsymbol(fexpr[1].span, varsym)
  let oldfexpr = fexpr
  fexpr = fexpr.span.quoteFExpr("var `embed `embed", [fsym, fsymbol(fexpr[2].span, typsym)])
  fexpr.metadata = oldfexpr.metadata
  fexpr.internalMark = internalVar
  scope.resolveByVoid(fexpr)

proc semConst*(scope: Scope, name: var FExpr, value: var FExpr) =
  if name.kind != fexprIdent:
    name.error("variable name should be FIdent.")
  
  value = value.span.quoteFExpr("const_eval(`embed)", [value])
  scope.rootPass(value)
  value = value[^1]

  let csym = scope.symbol(name(name), symbolDef, name)
  name.internalMark = internalConst
  name.constvalue = value
  name.typ = value.typ
  name = fsymbol(name.span, csym)
  name.typ = value.typ
  let status = scope.addDecl(name(name), csym)
  if not status:
    name.error("redefinition $# const." % $name)
  
proc semDef*(scope: Scope, fexpr: var FExpr) =
  var parsed = parseDef(fexpr)
  if parsed.isPrefix:
    let defmode = parsed.name[0]
    if $defmode == "const":
      semConst(scope, fexpr[1][1], fexpr[2])
      fexpr.internalMark = internalConst
      return
    else:
      defmode.error("$# is unknwon def mode, please specify `const." % $defmode)
    
  if parsed.name.kind notin {fexprIdent, fexprSeq}:
    parsed.name.error("variable name should be FIdent.")
  let name = if parsed.name.kind == fexprIdent:
               parsed.name
             else:
               parsed.name[0]

  if parsed.name.kind == fexprSeq and parsed.name[1].isPragmaPrefix:
    semPragma(scope, fexpr, parsed.name[2])

  scope.rootPass(parsed.value)
  if parsed.value.typ.isVoidType:
    parsed.value.error("value is Void.")
  elif $parsed.value.typ.name == "IntLit":
    parsed.value = parsed.value.span.quoteFExpr("int(`embed)", [parsed.value])
    scope.rootPass(parsed.value)
  elif $parsed.value.typ.name == "FloatLit":
    parsed.value = parsed.value.span.quoteFExpr("double(`embed)", [parsed.value])
    scope.rootPass(parsed.value)
  elif $parsed.value.typ.name == "StrLit":
    parsed.value = parsed.value.span.quoteFExpr("cstring(`embed)", [parsed.value])
    scope.rootPass(parsed.value)
    
  scope.resolveByVoid(fexpr)

  let varsym = scope.symbol(name(name), symbolDef, parsed.name)
  parsed.name.typ = parsed.value.typ.scope.varsym(parsed.value.typ)
  parsed.name.internalScope = scope
  let status = scope.addDecl(name(name), varsym)
  if not status:
    fexpr.error("redefinition $# variable." % $name)
  scope.tracking(varsym.fexpr)

  let fsym = fsymbol(fexpr[1].span, varsym)
  if parsed.name.kind == fexprIdent:
    parsed.name = fsym
  else:
    parsed.name[0] = fsym
  fexpr.internalMark = internalDef
  fexpr.defexpr = parsed
  
proc semSet*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len != 3:
    fexpr.error("expected set syntax: left = value")
  var parsed: SetExpr
  parsed.fexpr = fexpr
  parsed.dstpos = 1
  parsed.valuepos = 2

  scope.resolveByVoid(fexpr)
  scope.rootPass(parsed.dst)
  scope.rootPass(parsed.value)
  
  if parsed.dst.isInfixFuncCall and $parsed.dst[0] == "!" and scope.isSetter(parsed.dst[1].typ, parsed.dst[2].typ, parsed.value.typ):
    fexpr = fexpr.span.quoteFExpr("`!!(`embed, `embed, `embed)", [parsed.dst[1], parsed.dst[2], parsed.value])
    scope.rootPass(fexpr)
    return

  if not parsed.dst.hasTyp:
    parsed.dst.error("value hasn't type.")
  if not parsed.value.hasTyp:
    parsed.value.error("value hasn't type.")
  if parsed.dst.typ.kind != symbolVar and parsed.dst.typ.kind != symbolRef:
    parsed.dst.error("ref value expected.")

  let matched = parsed.dst.typ.match(parsed.value.typ)
  if not matched.isMatch:
    parsed.value.error("cannot set $# value to $#." % [$parsed.value.typ, $parsed.dst.typ])
  if matched.kind == matchConvert:
    fexpr[2] = fsymbol(fexpr[2].span, matched.convsym)
    scope.rootPass(fexpr[2])

  fexpr.internalMark = internalSet
  fexpr.setexpr = parsed

proc semModulePrefix*(scope: Scope, fexpr: var FExpr) =
  if not scope.importscopes.hasKey(name(fexpr[1])):
    fexpr.error("undeclared $# identifier" % $fexpr[1])
  let module = scope.importscopes[name(fexpr[1])]
  
  if fexpr[2].isNormalFuncCall:
    scope.rootPass(fexpr[2][1])
    let argtypes = fexpr[2][1].mapIt(it.typ)
    let opt = module.getFunc(procname(name(fexpr[2][0]), argtypes), importscope = false)
    if opt.isNone:
      fexpr.error("undeclared $#.$#($#) function." % [$fexpr[1], $fexpr[2][0], argtypes.mapIt($it).join(", ")])
    fexpr = fexpr.span.quoteFExpr("`embed `embed", [fsymbol(fexpr.span, opt.get.pd.sym), genConvertedCall(fexpr[2][1], opt.get.matches)])
    fexpr.typ = opt.get.pd.returntype
  elif fexpr[2].kind == fexprIdent:
    let opt = module.getDecl(name(fexpr[2]), importscope = false)
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
  let fieldopt = fexpr[1].typ.fexpr.getFieldType($fieldname)
  if fieldopt.isNone:
    fieldname.error("$# hasn't $# field." % [$fexpr[1].typ, $fieldname])
  if fexpr[1].typ.kind == symbolRef:
    fexpr.typ = fieldopt.get.scope.varsym(fieldopt.get)
  elif fexpr[1].typ.kind == symbolVar:
    fexpr.typ = fieldopt.get.scope.varsym(fieldopt.get)
  else:
    fexpr.typ = fieldopt.get

  fexpr.internalMark = internalFieldAccess
  fexpr.fieldaccessexpr = FieldAccessExpr(fexpr: fexpr)
  fexpr.fieldaccessexpr.valuepos = 1
  fexpr.fieldaccessexpr.fieldnamepos = 2

proc semDot*(scope: Scope, fexpr: var FExpr) =
  if fexpr[1].kind == fexprIdent:
    let opt = scope.getDecl(name(fexpr[1]))
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
  fexpr.typ = typesym
  scope.rootPass(fexpr[2])

  let argtypes = fexpr[2].mapIt(it.typ)

  for b in fexpr[2].mitems:
    if scope.isCopyable(b.typ):
      b = b.span.quoteFExpr("copy(`embed)", [b])
      scope.rootPass(b)

  fexpr.initexpr = InitExpr(fexpr: fexpr)
  fexpr.initexpr.typpos = 1
  fexpr.initexpr.bodypos = 2
  fexpr.initexpr.typ = fsymbol(fexpr[1][0].span, typesym)
  fexpr.internalMark = internalInit

proc semImport*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len != 2:
    fexpr.error("import syntax require filepath.")
  if fexpr[1].kind != fexprStrLit:
    fexpr.error("import syntax filepath should be FStrLit.")
  let importname = name(fexpr[1].strval)
  let filepath = ($importname).replace(".", "/")
  var modname = scope.ctx.semFile(filepath & ".flori")
  if modname.isNone:
    modname = scope.ctx.semFile(filepath / "root.flori")
    if modname.isNone:
      fexpr.error("cannot import $#" % $fexpr[1])
  scope.top.importScope(importname, scope.ctx.modules[modname.get])
  fexpr.internalMark = internalImport
  fexpr.importexpr = ImportExpr(fexpr: fexpr)
  fexpr.importexpr.importnamepos = 1

proc semExport*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len != 2:
    fexpr.error("export syntax require filepath.")
  if fexpr[1].kind != fexprStrLit:
    fexpr.error("export syntax filepath should be FStrLit.")
  let exportname = name(fexpr[1].strval.replace("/", "."))
  if not scope.ctx.modules.hasKey(exportname):
    var importexpr = fexpr.span.quoteFExpr("import `embed", [fexpr[1]])
    scope.rootPass(importexpr)
  let module = scope.ctx.modules[exportname]
  scope.top.exportscopes[exportname] = module
  fexpr.internalMark = internalExport

proc semReload*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len != 2:
    fexpr.error("export syntax require filepath.")
  if fexpr[1].kind != fexprStrLit:
    fexpr.error("export syntax filepath should be FStrLit.")
  let reloadname = name(fexpr[1].strval.replace("/", "."))
  let filepath = ($reloadname).replace(".", "/")
  if not scope.ctx.modules.hasKey(reloadname):
    fexpr.error("couldn't find $# reloadable module" % $fexpr[1])
  scope.ctx.modules.del(reloadname)
  var modname = scope.ctx.semFile(filepath & ".flori")
  if modname.isNone:
    modname = scope.ctx.semFile(filepath / "root.flori")
    if modname.isNone:
      fexpr.error("cannot import $#" % $fexpr[1])
  scope.top.importScope(reloadname, scope.ctx.modules[modname.get])
  fexpr.internalMark = internalReload

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
  let fstr = fstrlit(fexpr[1].span, ($fexpr[1]).replace("\n", ";").replace("\"", "\\\"").replace("\\n", "\\\\n"))

  let ret = fblock(fexpr.span)
  let tmpid = fident(fexpr.span, scope.ctx.genTmpName())
  ret.addSon(fexpr.span.quoteFExpr("`embed := new_farray()", [tmpid]))
  var collected = newSeq[FExpr]()
  collectQuotedItems(fexpr[1], collected)
  for c in collected:
    ret.addSon(genCall(fident(fexpr.span, name("push")), tmpid, c)) # push(tmpid, c)

  ret.addSon(fexpr.span.quoteFExpr("quote_expand(parse(\"$#\", $#, $#, `embed), `embed)" % [fexpr.span.filename, $fexpr.span.line, $fexpr.span.linepos], [fstr, tmpid]))
  fexpr = ret
  scope.rootPass(fexpr)

proc semCodegenDecl*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len != 2:
    fexpr.error("usage: cemit \"...\"")
  if fexpr[1].kind != fexprStrLit:
    fexpr.error("usage: cemit \"...\"")
  fexpr.internalMark = internalCodegenDecl
proc semCodegenHead*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len != 2:
    fexpr.error("usage: cemit \"...\"")
  if fexpr[1].kind != fexprStrLit:
    fexpr.error("usage: cemit \"...\"")
  fexpr.internalMark = internalCodegenHead

proc semBlock*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len != 2:
    fexpr.error("usage: block {...}")
  if fexpr[1].kind != fexprBlock:
    fexpr.error("usage: block {...}")
  let blockscope = scope.extendScope()
  blockscope.rootPass(fexpr[1])
  fexpr.internalScope = blockscope
  fexpr.internalMark = internalBlock

proc semWhenMacro*(scope: Scope, fexpr: var FExpr) =
  if fexpr.len == 2:
    var tbody = fexpr[1]
    var fbody = fblock(fexpr.span)
    let tscope = scope.extendScope()
    let fscope = scope.extendScope()
    tscope.rootPass(tbody)
    fscope.rootPass(fbody)
    tbody.runtime = fbody
    fexpr = tbody
  elif fexpr.len == 4:
    if $fexpr[2] != "else":
      fexpr[2].error("when_macro expect `else branch.")
    var tbody = fexpr[1]
    var fbody = fexpr[3]
    let tscope = scope.extendScope()
    let fscope = scope.extendScope()
    tscope.rootPass(tbody)
    fscope.rootPass(fbody)
    tbody.runtime = fbody
    fexpr = tbody
  else:
    fexpr.error("usage: when_macro {...} else {...}")
    
#
# Internal
#
  
proc addInternalEval*(scope: Scope, n: Name, p: proc (scope: Scope, fexpr: var FExpr)) =
  let status = scope.addFunc(ProcDecl(
    isInternal: true,
    internalproc: p,
    name: n,
    argtypes: @[],
    generics: @[],
    sym: scope.symbol(n, symbolInternal, fident(internalSpan, name("internal")))
  ))
  if not status:
    fseq(internalSpan).error("redefinition $# function." % $n)
  
proc initInternalEval*(scope: Scope) =
  scope.addInternalEval(name("fn"), semDefn)
  scope.addInternalEval(name("syntax"), semSyntax)
  scope.addInternalEval(name("macro"), semMacro)
  scope.addInternalEval(name("type"), semDeftype)
  scope.addInternalEval(name("typedef"), semTypedef)
  scope.addInternalEval(name("if"), semIf)
  scope.addInternalEval(name("while"), semWhile)
  scope.addInternalEval(name("var"), semVar)
  scope.addInternalEval(name(":="), semDef)
  scope.addInternalEval(name("="), semSet)
  scope.addInternalEval(name("."), semDot)
  scope.addInternalEval(name("init"), semInit)
  scope.addInternalEval(name("import"), semImport)
  scope.addInternalEval(name("export"), semExport)
  scope.addInternalEval(name("reload"), semReload)
  scope.addInternalEval(name("quote"), semQuote)
  scope.addInternalEval(name("codegen_decl"), semCodegenDecl)
  scope.addInternalEval(name("codegen_head"), semCodegenHead)
  scope.addInternalEval(name("block"), semBlock)
  scope.addInternalEval(name("when_macro"), semWhenMacro)

  # c pragmas
  scope.addInternalEval(name("importc"), semImportc)
  scope.addInternalEval(name("header"), semHeader)
  scope.addInternalEval(name("exportc"), semExportc)
  scope.addInternalEval(name("patternc"), semPatternc)
  scope.addInternalEval(name("declc"), semDeclc)
  # js pragmas
  scope.addInternalEval(name("importjs"), semImportjs)
  scope.addInternalEval(name("exportjs"), semExportjs)
  scope.addInternalEval(name("patternjs"), semPatternjs)
  # general pragmas
  scope.addInternalEval(name("inline"), semInline)
  scope.addInternalEval(name("compiletime"), semCompiletime)
  scope.addInternalEval(name("nocompiletime"), semNoCompiletime)
  scope.addInternalEval(name("converter"), semConverter)

proc initInternalScope*(ctx: SemanticContext) =
  let scope = ctx.newScope(name("internal"), "internal")
  scope.initInternalEval()
  ctx.modules[name("internal")] = scope

proc internalScope*(ctx: SemanticContext): Scope =
  ctx.modules[name("internal")]

proc newSemanticContext*(moptions = "", defines = newSeq[string]()): SemanticContext =
  new result
  result.modules = initOrderedTable[Name, Scope]()
  result.macrolib = nil
  result.macroprocs = @[]
  result.tmpcount = 0
  result.initInternalScope()
  result.importpaths = @[getAppDir() / "..", ".", getHomeDir() / ".rabbit"]
  result.moptions = moptions
  result.defines = defines
  result.globaltoplevels = @[]
  result.expands = @[]
  rootPass = processSemPass
  gCtx = result

proc semModule*(ctx: SemanticContext, name: Name, scope: Scope, fexprs: var seq[FExpr]) =
  if ctx.modules.hasKey(name):
    return
  scope.importScope(name("internal"), ctx.internalScope)
  for f in fexprs.mitems:
    f.isToplevel = true
    scope.rootPass(f)
    scope.ctx.globaltoplevels.add(f)
  ctx.modules[name] = scope

proc semFile*(ctx: SemanticContext, filepath: string): Option[Name] =
  for importpath in ctx.importpaths:
    if existsFile(importpath / filepath):
      var fexprs = parseToplevel(filepath, readFile(importpath / filepath))
      let (dir, n, _) = filepath.splitFile()
      let modname = if dir != "":
                      name(dir.replace("/", ".") & "." & n)
                    else:
                      name(n)
      let scope = ctx.newScope(modname, importpath / filepath)
      ctx.semModule(modname, scope, fexprs)
      return some(modname)
    elif existsFile(importpath / filepath / "root.flori"):
      var fexprs = parseToplevel(filepath, readFile(importpath / filepath / "root.flori"))
      let (dir, n, _) = filepath.splitFile()
      let modname = if dir != "":
                      name(dir.replace("/", ".") & "." & n)
                    else:
                      name(n)
      let scope = ctx.newScope(modname, importpath / filepath / "root.flori")
      ctx.semModule(modname, scope, fexprs)
      return some(modname)
      
  return none(Name)
