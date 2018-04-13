
import fexpr_core, scopeout, marking
import passutils, typepass
import compileutils, macroffi

import options
import strutils, sequtils
import tables
import os

proc semFile*(ctx: SemanticContext, rootPass: PassProcType, filepath: string): Option[Name]

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

  # ret ref
  if pos < fexpr.len and ($fexpr[pos] == "ref" or $fexpr[pos] == "dynamic"):
    result.retprefixpos = newfexpr.len
    newfexpr.addSon(fexpr[pos])
    pos.inc
  else:
    result.retprefixpos = -1

  # ret
  if pos < fexpr.len and fexpr[pos].kind in {fexprIdent, fexprSymbol}:
    result.retpos = newfexpr.len
    newfexpr.addSon(fexpr[pos])
    pos.inc
  else:
    result.retpos = newfexpr.len
    newfexpr.addSon(fident(fexpr.span, name("Void")))

  # ret generics
  if pos < fexpr.len and fexpr[pos].kind == fexprArray:
    result.retgenericspos = newfexpr.len
    newfexpr.addSon(fexpr[pos])
    pos.inc
  else:
    result.retgenericspos = newfexpr.len
    newfexpr.addSon(farray(fexpr.span))

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

proc semImportc*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  if fexpr.kind == fexprIdent:
    let name = $fexpr.parent[1]
    fexpr.parent.internalPragma.importc = some(name)
  else:
    if fexpr[1].kind != fexprStrLit:
      fexpr[1].error("importc argument should be FStrLit")
    let name = fexpr[1].strval
    fexpr.parent.internalPragma.importc = some(name)

proc semHeader*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  if fexpr.len == 2:
    if $fexpr[1] == "nodeclc":
      fexpr.parent.internalPragma.header = none(string)
    elif fexpr[1].kind == fexprStrLit:
      let name = fexpr[1].strval
      fexpr.parent.internalPragma.header = some(name)
    else:
      fexpr[1].error("header argument should be FStrLit")
  else:
    fexpr.error("usage: `header \"headername.h\"`")

proc semExportc*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  if fexpr.kind == fexprIdent:
    let name = $fexpr.parent[1]
    fexpr.parent.internalPragma.exportc = some(name)
  else:
    if fexpr[1].kind != fexprStrLit:
      fexpr[1].error("exportc argument should be FStrLit")
    let name = fexpr[1].strval
    fexpr.parent.internalPragma.exportc = some(name)
    
proc semDeclc*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  if fexpr.len == 2:
    if fexpr[1].kind == fexprStrLit:
      fexpr.parent.internalPragma.declc = some(fexpr[1].strval)
    else:
      fexpr[1].error("unsupported in declc pragma")
  else:
    fexpr.error("usage: `declc \"#1($1)\"``")

proc semPatternc*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  if fexpr.len == 2:
    if $fexpr[1] == "infixc":
      fexpr.parent.internalPragma.infixc = true
    elif fexpr[1].kind == fexprStrLit:
      fexpr.parent.internalPragma.patternc = some(fexpr[1].strval)
    else:
      fexpr[1].error("unsupported in patternc pragma")
  else:
    fexpr.error("usage: `patternc \"#1($1)\"` or `patternc infixc`")

#
# JS pragmas
#

proc semImportjs*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  if fexpr.kind == fexprIdent:
    let name = $fexpr.parent[1]
    fexpr.parent.internalPragma.importjs = some(name)
  else:
    if fexpr[1].kind != fexprStrLit:
      fexpr[1].error("importjs argument should be FStrLit")
    let name = fexpr[1].strval
    fexpr.parent.internalPragma.importjs = some(name)

proc semExportjs*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  if fexpr.kind == fexprIdent:
    let name = $fexpr.parent[1]
    fexpr.parent.internalPragma.exportjs = some(name)
  else:
    if fexpr[1].kind != fexprStrLit:
      fexpr[1].error("exportjs argument should be FStrLit")
    let name = fexpr[1].strval
    fexpr.parent.internalPragma.exportjs = some(name)

proc semPatternjs*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  if fexpr.len == 2:
    if $fexpr[1] == "infixjs":
      fexpr.parent.internalPragma.infixjs = true
    elif fexpr[1].kind == fexprStrLit:
      fexpr.parent.internalPragma.patternjs = some(fexpr[1].strval)
    else:
      fexpr[1].error("unsupported in patternjs pragma")
  else:
    fexpr.error("usage: `patternjs \"#1($1)\"` or `patternjs infixc`")

#
# General pragmas
#
    
proc semInline*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  fexpr.parent.internalPragma.inline = true
proc semNoDestruct*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  fexpr.parent.internalPragma.nodestruct = true
proc semCompiletime*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  fexpr.parent.internalPragma.compiletime = true

proc semPragma*(rootPass: PassProcType, scope: Scope, fexpr: FExpr, pragma: FExpr) =
  if pragma.kind != fexprArray:
    pragma.error("$# isn't internal pragma." % $pragma)
  fexpr.internalPragma = InternalPragma()

  for key in pragma.mitems:
    key.parent = fexpr
    let pragmaname = if key.kind in fexprContainer:
                       name(key[0])
                     else:
                       name(key)
    let internalopt = scope.getFunc(procname(pragmaname, @[]))
    if internalopt.isSome:
      internalopt.get.internalproc(rootPass, scope, key)
    else:
      key[0].error("undeclared $# pragma. (internal only support in currently)" % $pragmaname)

#
# Evaluater
#

proc declGenerics*(scope: Scope, fexpr: FExpr): seq[Symbol] =
  result = @[]
  for g in fexpr.mitems:
    let sym = scope.symbol(name(g), symbolGenerics, g)
    let status = scope.addDecl(name(g), sym)
    if not status:
      g.error("redefinition $# generics." % $g)
    g = fsymbol(g.span, sym)
    result.add(sym)

proc declArgtypes*(scope: Scope, fexpr: FExpr, isGenerics: bool): seq[Symbol] =
  result = @[]
  for i, arg in fexpr:
    var pos = 1
    let argtyp = arg.parseTypeExpr(pos)
    let typesym = scope.semType(argtyp)

    let argsym = scope.symbol(name(arg[0]), symbolArg, arg[0])
    argsym.argpos = i
    arg[0].typ = typesym
    arg[0].replaceByTypesym(argsym)
    arg[1].replaceByTypesym(typesym)
    result.add(typesym)

  if not isGenerics and not fexpr.mapIt(it[1]).isIncludeRef:
    for i, arg in fexpr:
      if not isGenerics:
        let status = scope.addDecl(name(arg[0]), arg[0].symbol)
        if not status:
          arg[0].error("redefinition $# variable." % $arg[0])
        
proc semFunc*(rootPass: PassProcType, scope: Scope, fexpr: FExpr, parsed: Defn, defsym: SymbolKind): (Scope, seq[Symbol], seq[Symbol], Symbol, Symbol) =
  let fnscope = scope.extendScope()
  let generics = if parsed.isGenerics:
                   fnscope.declGenerics(parsed.generics)
                 else:
                   @[]
  let argtypes = fnscope.declArgtypes(parsed.args, parsed.isGenerics)
  let rettype = fnscope.semType(ParsedType(typ: parsed.ret, generics: parsed.retgenerics, prefix: if parsed.hasRetprefix: some(parsed.retprefix) else: none(FExpr)))
  parsed.ret.replaceByTypesym(rettype)
  
  let symkind = if parsed.name.kind == fexprQuote: symbolInfix else: defsym
  let sym = scope.symbol(name(parsed.name), symkind, fexpr)
  let pd = ProcDecl(isInternal: false, name: name(parsed.name), argtypes: argtypes, generics: generics, returntype: rettype, sym: sym, fexpr: fexpr)
  discard fnscope.addFunc(pd)

  fexpr.internalScope = fnscope
  fexpr.defn = parsed
  scope.resolveByVoid(fexpr)

  semPragma(rootPass, scope, fexpr, parsed.pragma)
  if parsed.generics.isSpecTypes and not fexpr.internalPragma.inline and not argtypes.isIncludeRef:
    fnscope.rootPass(parsed.body)
    if parsed.body.len != 0:
      if not parsed.body[^1].typ.match(rettype):
        parsed.body[^1].error("function expect $# return type, actually $#" % [$rettype, $parsed.body[^1].typ])
    if not fexpr.internalPragma.nodestruct:
      expandDestructor(rootPass, fnscope, fexpr.defn.body)

  # symbol resolve
  let fsym = fsymbol(fexpr[0].span, sym)
  parsed.name = fsym
  
  return (fnscope, generics, argtypes, rettype, sym)

proc semDefn*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  var parsed = parseDefn(fexpr)
  let (fnscope, generics, argtypes, rettype, sym) = semFunc(rootPass, scope, fexpr, parsed, symbolFunc)

  let pd = ProcDecl(isInternal: false, name: name(parsed.name), argtypes: argtypes, generics: generics, returntype: rettype, sym: sym, fexpr: fexpr)
  let status = scope.addFunc(pd)
  if not status:
    fexpr.error("redefinition $# function." % $parsed.name)
  fexpr.internalMark = internalDefn

proc semSyntax*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  if fexpr.kind == fexprIdent:
    if not fexpr.hasParent:
      fexpr.error("usage: macro name() FExpr $[syntax] {...}")
    fexpr.parent.internalPragma.isSyntax = true
    return
  
  var parsed = parseDefn(fexpr)
  let (fnscope, generics, argtypes, rettype, sym) = semFunc(rootPass, scope, fexpr, parsed, symbolSyntax)

  let mp = MacroProc(importname: codegenMangling(sym, @[], argtypes)) # FIXME: support generics
  let pd = ProcDecl(isInternal: false, isSyntax: true, macroproc: mp, name: name(parsed.name), argtypes: argtypes, generics: generics, returntype: rettype, sym: sym, fexpr: fexpr)
  let status = scope.addFunc(pd)
  if not status:
    fexpr.error("redefinition $# macro." % $parsed.name)
  fexpr.internalMark = internalMacro

  scope.ctx.globaltoplevels.add(fexpr)
  scope.ctx.macroprocs.add(mp)
  scope.ctx.reloadMacroLibrary(scope.top)
  scope.ctx.globaltoplevels.del(high(scope.ctx.globaltoplevels))
  
  if not fexpr.isToplevel:
    scope.ctx.globaltoplevels.add(fexpr)
    fexpr.isGenerated = true

proc semMacro*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  var parsed = parseDefn(fexpr)
  let (fnscope, generics, argtypes, rettype, sym) = semFunc(rootPass, scope, fexpr, parsed, symbolMacro)

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
    
proc semDeftype*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  var parsed = parseDeftype(fexpr)

  let typescope = scope.extendScope()

  let typename = name(parsed.name)
  let sym = scope.symbol(typename, if parsed.isGenerics: symbolTypeGenerics else: symbolType, fexpr)
  let status = scope.addDecl(typename, sym)
  if not status:
    fexpr.error("redefinition $# type." % $typename)

  if parsed.isGenerics:
    discard typescope.declGenerics(parsed.generics)

  for field in parsed.body:
    var pos = 1
    let fieldtyp = field.parseTypeExpr(pos)
    let s = typescope.semType(fieldtyp)
    field[1].replaceByTypesym(s)
  
  # symbol resolve
  let fsym = fsymbol(fexpr[0].span, sym)
  fexpr[1] = fsym
  parsed.name = fsym
  # internal metadata for postprocess phase
  semPragma(rootPass, scope, fexpr, parsed.pragma)
  fexpr.internalScope = typescope
  fexpr.internalMark = internalDeftype
  fexpr.deftype = parsed

  if fexpr.internalPragma.importc.isNone:
    sym.fexpr.isCStruct = true

proc semIf*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
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

proc semWhile*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  var parsed = parseWhile(fexpr)
  scope.rootPass(parsed.cond)
  if not parsed.cond[0].typ.isBoolType:
    parsed.cond.error("while statement cond type chould be Bool.")
  let bodyscope = scope.extendScope()
  bodyscope.rootPass(parsed.body)
  expandDestructor(rootPass, bodyscope, parsed.body)
  scope.resolveByVoid(fexpr)
  fexpr.internalMark = internalWhile
  fexpr.whileexpr = parsed

proc semVar*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  let n = fexpr[1]
  if n.kind != fexprIdent:
    n.error("variable name should be FIdent.")

  var pos = 2
  let parsedtype = parseTypeExpr(fexpr, pos)
  let typsym = if fexpr[2].kind == fexprSymbol:
                 fexpr[2].symbol
               else:
                 scope.semType(parsedtype)
  
  let varsym = scope.symbol(name(n), symbolDef, n)
  n.typ = typsym.scope.varsym(typsym)
  let status = scope.addDecl(name(n), varsym)
  if not status:
    fexpr.error("redefinition $# variable." % $n)
  varsym.fexpr.marking = newMarking(scope, typsym)
  scope.tracking(varsym.fexpr)
  
  let fsym = fsymbol(fexpr[1].span, varsym)
  let oldfexpr = fexpr
  fexpr = fexpr.span.quoteFExpr("var `embed `embed", [fsym, fsymbol(fexpr[2].span, typsym)])
  fexpr.metadata = oldfexpr.metadata
  fexpr.internalMark = internalVar
  scope.resolveByVoid(fexpr)

proc semConst*(rootPass: PassProcType, scope: Scope, name: var FExpr, value: var FExpr) =
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
  
proc semDef*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  var parsed = parseDef(fexpr)
  if parsed.isPrefix:
    let defmode = parsed.name[0]
    if $defmode == "const":
      semConst(rootPass, scope, fexpr[1][1], fexpr[2])
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
    semPragma(rootPass, scope, fexpr, parsed.name[2])

  scope.rootPass(parsed.value)
  if parsed.value.typ.isVoidType:
    parsed.value.error("value is Void.")
  scope.resolveByVoid(fexpr)

  let varsym = scope.symbol(name(name), symbolDef, name)
  name.typ = parsed.value.typ.scope.varsym(parsed.value.typ)
  let status = scope.addDecl(name(name), varsym)
  if not status:
    fexpr.error("redefinition $# variable." % $name)
  if parsed.value.hasMarking:
    varsym.fexpr.marking = parsed.value.marking
  else:
    varsym.fexpr.marking = newMarking(scope, parsed.value.typ)
  scope.tracking(varsym.fexpr)

  let fsym = fsymbol(fexpr[1].span, varsym)
  if parsed.name.kind == fexprIdent:
    parsed.name = fsym
  else:
    parsed.name[0] = fsym
  fexpr.internalMark = internalDef
  fexpr.defexpr = parsed

proc semSet*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  if fexpr.len != 3:
    fexpr.error("expected set syntax: left = value")
  var parsed: SetExpr
  parsed.fexpr = fexpr
  parsed.dstpos = 1
  parsed.valuepos = 2

  scope.resolveByVoid(fexpr)
  scope.rootPass(parsed.dst)
  scope.rootPass(parsed.value)

  if not parsed.dst.typ.match(parsed.value.typ):
    parsed.value.error("cannot set $# value to $#." % [$parsed.value.typ, $parsed.dst.typ])

  var body = fblock(fexpr.span)
  if not parsed.value.hasMarking:
    parsed.value.marking = newMarking(scope, parsed.value.typ)
  if parsed.dst.hasMarking:
    if parsed.dst.marking.owned and scope.isDestructable(parsed.dst.typ):
      var destcall = fexpr.span.quoteFExpr("destruct(`embed)", [parsed.dst])
      scope.rootPass(destcall)
      body.addSon(destcall)
      body.addSon(fexpr)
      returnFrom(parsed.dst.marking)
    parsed.dst.marking.getFrom(parsed.value.marking)

  fexpr.internalMark = internalSet
  fexpr.setexpr = parsed
  
  if body.len != 0:
    fexpr = body
    scope.rootPass(fexpr)

proc semFieldAccess*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  let fieldname = fexpr[2]
  if fieldname.kind != fexprIdent:
    fieldname.error("field name should be FIdent.")
  scope.rootPass(fexpr[1])
  let fieldopt = fexpr[1].typ.fexpr.getFieldType($fieldname)
  if fieldopt.isNone:
    fieldname.error("$# hasn't $# field." % [$fexpr[1].typ, $fieldname])
  if fexpr[1].typ.kind == symbolRef:
    fexpr.typ = fieldopt.get.scope.varsym(fieldopt.get)
  elif fexpr[1].typ.kind == symbolVar:
    fexpr.typ = fieldopt.get.scope.varsym(fieldopt.get)
  else:
    fexpr.typ = fieldopt.get

  if fexpr[1].hasMarking:
    fexpr.marking = fexpr[1].marking.fieldbody[name(fexpr[2])]

  fexpr.internalMark = internalFieldAccess
  fexpr.fieldaccessexpr = FieldAccessExpr(fexpr: fexpr)
  fexpr.fieldaccessexpr.valuepos = 1
  fexpr.fieldaccessexpr.fieldnamepos = 2
  
proc semInit*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  if fexpr.len != 3:
    fexpr.error("init require 2 arguments.")
  if fexpr[1].kind != fexprList:
    fexpr.error("init type should be FList.")
  if fexpr[1].len != 1:
    fexpr.error("init type should be single argument.")
  if fexpr[2].kind != fexprBlock:
    fexpr.error("init body should be FBlock.")
    
  var pos = 0
  let inittype = fexpr[1][0].parseTypeExpr(pos)
  let typesym = scope.semType(inittype)
  fexpr[1][0].replaceByTypesym(typesym)
  fexpr.typ = typesym
  scope.rootPass(fexpr[2])

  let argtypes = fexpr[2].mapIt(it.typ)

  fexpr.marking = newMarking(scope, typesym)
  for i, b in fexpr[2]:
    let typefield = typesym.fexpr.deftype.body[i]
    if b.hasMarking:
      fexpr.marking.fieldbody[name(typefield[0])].getFrom(b.marking)
      # returnFrom(b.marking)
      # fexpr.marking.fieldbody[name(typefield[0])] = b.marking

  fexpr.initexpr = InitExpr(fexpr: fexpr)
  fexpr.initexpr.typpos = 1
  fexpr.initexpr.bodypos = 2
  fexpr.initexpr.typ = fsymbol(inittype.typ.span, typesym)
  fexpr.internalMark = internalInit

proc semImport*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  if fexpr.len != 2:
    fexpr.error("import syntax require filepath.")
  if fexpr[1].kind != fexprStrLit:
    fexpr.error("import syntax filepath should be FStrLit.")
  let importname = name(fexpr[1].strval)
  let filepath = ($importname).replace(".", "/")
  var modname = scope.ctx.semFile(rootPass, filepath & ".flori")
  if modname.isNone:
    modname = scope.ctx.semFile(rootPass, filepath / "root.flori")
    if modname.isNone:
      fexpr.error("cannot import $#" % $fexpr[1])
  scope.top.importScope(importname, scope.ctx.modules[modname.get])
  fexpr.internalMark = internalImport
  fexpr.importexpr = ImportExpr(fexpr: fexpr)
  fexpr.importexpr.importnamepos = 1

proc semExport*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
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

proc collectQuotedItems*(fexpr: FExpr, collected: var seq[FExpr]) =
  for son in fexpr:
    if son.kind == fexprQuote and son.quoted.kind == fexprIdent:
      collected.add(son.quoted)
    elif son.kind in fexprContainer:
      collectQuotedItems(son, collected)

proc semQuote*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
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

proc semIsDestructable*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  if fexpr.len != 2 or fexpr[1].kind != fexprList and fexpr[1].len != 0:
    fexpr.error("usage: is_destructable(type)")
  
  let typesym = scope.semTypeExpr(fexpr[1][0])
  if fexpr.internalScope.getFunc(procname(name("destruct"), @[typesym])).isSome:
    fexpr = fexpr.span.quoteFExpr("true", [])
  else:
    fexpr = fexpr.span.quoteFExpr("false", [])
  scope.rootPass(fexpr)

proc semCEmit*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  if fexpr.len != 2:
    fexpr.error("usage: cemit \"...\"")
  if fexpr[1].kind != fexprStrLit:
    fexpr.error("usage: cemit \"...\"")
  fexpr.internalMark = internalCEmit

proc semBlock*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  if fexpr.len != 2:
    fexpr.error("usage: block {...}")
  if fexpr[1].kind != fexprBlock:
    fexpr.error("usage: block {...}")
  let blockscope = scope.extendScope()
  blockscope.rootPass(fexpr[1])
  expandDestructor(rootPass, blockscope, fexpr[1])
  fexpr.internalMark = internalBlock
    
#
# Internal
#
  
proc addInternalEval*(scope: Scope, n: Name, p: proc (rootPass: PassProcType, scope: Scope, fexpr: var FExpr)) =
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
  scope.addInternalEval(name("if"), semIf)
  scope.addInternalEval(name("while"), semWhile)
  scope.addInternalEval(name("var"), semVar)
  scope.addInternalEval(name(":="), semDef)
  scope.addInternalEval(name("="), semSet)
  scope.addInternalEval(name("."), semFieldAccess)
  scope.addInternalEval(name("init"), semInit)
  scope.addInternalEval(name("import"), semImport)
  scope.addInternalEval(name("export"), semExport)
  scope.addInternalEval(name("quote"), semQuote)
  scope.addInternalEval(name("cemit"), semCEmit)
  scope.addInternalEval(name("block"), semBlock)

  scope.addInternalEval(name("is_destructable"), semIsDestructable)

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
  scope.addInternalEval(name("nodestruct"), semNoDestruct)
  scope.addInternalEval(name("compiletime"), semCompiletime)

proc initInternalScope*(ctx: SemanticContext) =
  let scope = ctx.newScope(name("internal"), "internal")
  scope.initInternalEval()
  ctx.modules[name("internal")] = scope

proc internalScope*(ctx: SemanticContext): Scope =
  ctx.modules[name("internal")]

proc newSemanticContext*(ccoptions = ""): SemanticContext =
  new result
  result.modules = initOrderedTable[Name, Scope]()
  result.macrolib = nil
  result.macroprocs = @[]
  result.tmpcount = 0
  result.initInternalScope()
  result.importpaths = @[getAppDir() / "..", ".", getHomeDir() / ".rabbit"]
  result.ccoptions = ccoptions
  result.globaltoplevels = @[]
  result.expands = @[]
  gCtx = result

proc semModule*(ctx: SemanticContext, rootPass: PassProcType, name: Name, scope: Scope, fexprs: var seq[FExpr]) =
  if ctx.modules.hasKey(name):
    return
  scope.importScope(name("internal"), ctx.internalScope)
  for f in fexprs.mitems:
    f.isToplevel = true
    scope.rootPass(f)
    scope.ctx.globaltoplevels.add(f)
  ctx.modules[name] = scope

proc semFile*(ctx: SemanticContext, rootPass: PassProcType, filepath: string): Option[Name] =
  for importpath in ctx.importpaths:
    if existsFile(importpath / filepath):
      var fexprs = parseToplevel(filepath, readFile(importpath / filepath))
      let (dir, n, _) = filepath.splitFile()
      let modname = if dir != "":
                      name(dir.replace("/", ".") & "." & n)
                    else:
                      name(n)
      let scope = ctx.newScope(modname, importpath / filepath)
      ctx.semModule(rootPass, modname, scope, fexprs)
      return some(modname)
    elif existsFile(importpath / filepath / "root.flori"):
      var fexprs = parseToplevel(filepath, readFile(importpath / filepath / "root.flori"))
      let (dir, n, _) = filepath.splitFile()
      let modname = if dir != "":
                      name(dir.replace("/", ".") & "." & n)
                    else:
                      name(n)
      let scope = ctx.newScope(modname, importpath / filepath / "root.flori")
      ctx.semModule(rootPass, modname, scope, fexprs)
      return some(modname)
      
  return none(Name)
