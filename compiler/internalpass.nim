
import parser, types, fexpr, scope, metadata, ctrc, effect
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
  if pos < fexpr.len and $fexpr[pos] == "ref":
    newfexpr.addSon(fexpr[pos])
    result.isretref = true
    pos.inc
  else:
    result.isretref = false

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
  if fexpr[1].kind == fexprSeq and fexpr[1].len == 2:
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

    arg[0].ctrc = initCTRC(cnt = 0)
    let argsym = scope.symbol(name(arg[0]), symbolArg, arg[0])
    argsym.argpos = i
    arg[0].typ = typesym
    arg[0].replaceByTypesym(argsym)
    arg[1].replaceByTypesym(typesym)
    result.add(typesym)

    if not isGenerics:
      let status = scope.addDecl(name(arg[0]), argsym)
      if not status:
        arg[0].error("redefinition $# variable." % $arg[0])

proc isIncludeRef*(argtypes: seq[Symbol]): bool =
  for argt in argtypes:
    if argt.kind == symbolRef:
      return true
  return false
        
proc semFunc*(rootPass: PassProcType, scope: Scope, fexpr: FExpr, parsed: Defn, defsym: SymbolKind): (Scope, seq[Symbol], seq[Symbol], Symbol, Symbol) =
  let fnscope = scope.extendScope()
  let generics = if parsed.isGenerics:
                   fnscope.declGenerics(parsed.generics)
                 else:
                   @[]
  let argtypes = fnscope.declArgtypes(parsed.args, parsed.isGenerics)
  let rettype = fnscope.semType(ParsedType(typ: parsed.ret, generics: parsed.retgenerics, isref: parsed.isretref))
  parsed.ret.replaceByTypesym(rettype)
  
  let symkind = if parsed.name.kind == fexprQuote: symbolInfix else: defsym
  let sym = scope.symbol(name(parsed.name), symkind, fexpr)
  let pd = ProcDecl(isInternal: false, name: name(parsed.name), argtypes: argtypes, generics: generics, returntype: rettype, sym: sym, fexpr: fexpr)
  discard fnscope.addFunc(pd)

  fexpr.internalScope = fnscope
  fexpr.defn = parsed
  scope.resolveByVoid(fexpr)

  semPragma(rootPass, scope, fexpr, parsed.pragma)
  # if argtypes.isIncludeRef and fexpr.internalPragma.importc.isNone and generics.len == 0:
  #   fexpr.internalPragma.inline = true
  if parsed.generics.isSpecTypes and not fexpr.internalPragma.inline:
    fnscope.rootPass(parsed.body)
    if parsed.body.len != 0:
      if not parsed.body[^1].typ.spec(rettype):
        parsed.body[^1].error("function expect $# return type, actually $#" % [$rettype, $parsed.body[^1].typ])
    if parsed.body.len != 0 and not parsed.body[^1].typ.isVoidType:
      if parsed.body[^1].hasCTRC:
        if not parsed.body[^1].ctrc.inc:
          parsed.body[^1].error("value is already destroyed.")
    fnScopeout(rootPass, fnscope, fexpr) # FIXME:

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

  if not fexpr.isToplevel:
    scope.ctx.globaltoplevels.add(fexpr)
    fexpr.isGenerated = true

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
  
  scope.ctx.globaltoplevels.add(fexpr)
  if parsed.generics.isSpecTypes:
    scope.ctx.macroprocs.add(mp)
    scope.ctx.reloadMacroLibrary(scope.top)
  scope.ctx.globaltoplevels.del(high(scope.ctx.globaltoplevels))
  
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
  bodyScopeout(rootPass, bodyscope, parsed.body)
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
  varsym.fexpr.ctrc = initCTRC()
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
  csym.fexpr.ctrc = initCTRC()
  
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
    
  if parsed.name.kind != fexprIdent:
    parsed.name.error("variable name should be FIdent.")

  scope.rootPass(parsed.value)
  if parsed.value.typ.isVoidType:
    parsed.value.error("value is Void.")
  scope.resolveByVoid(fexpr)

  let varsym = scope.symbol(name(parsed.name), symbolDef, parsed.name)
  parsed.name.typ = parsed.value.typ.scope.varsym(parsed.value.typ)
  let status = scope.addDecl(name(parsed.name), varsym)
  if not status:
    fexpr.error("redefinition $# variable." % $parsed.name)

  let body = fblock(fexpr.span)
  if parsed.value.kind == fexprSymbol: # alias
    varsym.fexpr.ctrc = parsed.value.ctrc
  elif parsed.value.kind == fexprBlock and parsed.value.len != 0 and parsed.value[^1].kind == fexprSymbol: # alias
    varsym.fexpr.ctrc = parsed.value[^1].ctrc
  elif parsed.value.hasCTRC:
    varsym.fexpr.ctrc = initCTRC(cnt = 1)
    # varsym.fexpr.ctrc = initCTRCWithFields(parsed.value.typ.fexpr.deftype.body)
    # varsym.fexpr.ctrc.cnt = 1
    # body.addSon(fexpr)
    # for key, value in parsed.value.ctrc.fieldbody:
    #   if value.ctrc.cnt != 0:
    #     if scope.getFunc(procname(name("destruct"), @[value.typ])).isNone:
    #       continue
          
    #     varsym.fexpr.ctrc[key] = value
    #     var field = fexpr.span.quoteFExpr("`embed . `embed", [fexpr[1], fident(fexpr.span, key)])
    #     scope.rootPass(field)
    #     scope.tracking(field)
    #     # var track = fexpr.span.quoteFExpr("track(`embed -> `embed)", [field, value])
    #     # scope.rootPass(track)
    #     # body.addSon(track)
    scope.tracking(varsym.fexpr)
  else:
    varsym.fexpr.ctrc = initCTRC(cnt = 1)
    scope.tracking(varsym.fexpr)

  let fsym = fsymbol(fexpr[1].span, varsym)
  fexpr[1] = fsym
  parsed.name = fsym
  fexpr.internalMark = internalDef
  fexpr.defexpr = parsed
  if body.len != 0:
    fexpr = body
    scope.rootPass(fexpr)
    fexpr.typ = fexpr[^1].typ
  
proc semTrack*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr) =
  scope.resolveByVoid(fexpr)
  let infix = fexpr[1][0]
  if infix.kind != fexprSeq and $infix[0] == "->":
    fexpr.error("track syntax expect: track(`variable -> `depend)")
  let twoway = if $infix[0] == "->":
                 false
               elif $infix[0] == "<->":
                 true
               else:
                 infix[0].error("track syntax expect: track(`variable` -> `depend)")
                 false
  var variable = infix[1]
  var depend = infix[2]
  if variable.kind notin {fexprIdent, fexprSymbol} and not variable.isInfixFuncCall:
    variable.error("track arguments should be FIdent, actually $#" % $variable)
  if depend.kind notin {fexprIdent, fexprSymbol} and not depend.isInfixFuncCall:
    depend.error("track arguments should be FIdent, actually $#" % $depend)
  scope.rootPass(variable)
  scope.rootPass(depend)

  if variable.isInfixFuncCall:
    if not variable[1].ctrc.depend(variable.ctrc):
      depend.error("$# variable has been destroyed." % $depend)
    if not variable.ctrc.depend(depend.ctrc):
      depend.error("$# variable has been destroyed." % $depend)
  else:
    if not variable.ctrc.depend(depend.ctrc):
      depend.error("$# variable has been destroyed." % $depend)
        
  # if not variable.ctrc.depend(depend.ctrc):
  #   depend.error("$# variable has been destroyed.")
  # if twoway:
  #   if not depend.ctrc.depend(variable.ctrc):
  #     variable.error("$# variable has been destroyed.")
      
  # scope.scopedepends.add(initDepend(variable, depend))
  # if twoway:
  #   scope.scopedepends.add(initDepend(depend, variable))
    
  fexpr.internalMark = internalTrack

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

  fexpr.internalMark = internalSet
  fexpr.setexpr = parsed

  if parsed.dst.hasCTRC:
    parsed.dst.ctrc.dec
    if parsed.dst.ctrc.destroyed:
      if scope.getFunc(procname(name("destruct"), @[parsed.dst.typ])).isSome:
        let dcall = parsed.dst.span.quoteFExpr("destruct(`embed)", [parsed.dst])
        fexpr = fblock(parsed.dst.span, @[dcall, fexpr])
        scope.rootPass(fexpr)
        parsed.dst.ctrc.exdestroyed = true
        if parsed.dst.isInfixFuncCall:
          parsed.dst[1].ctrc[name(parsed.dst[2])].ctrc = parsed.value.ctrc
          if parsed.dst[1].symbol.scope.level != scope.level:
            parsed.dst[1].symbol.fexpr.ctrc.fuzzy = true
        else:
          if parsed.value.hasCTRC:
            parsed.dst.ctrc = parsed.value.ctrc
            if parsed.dst.symbol.scope.level != scope.level:
              parsed.dst.symbol.fexpr.ctrc.fuzzy = true
          else:
            parsed.dst.ctrc = initCTRC(cnt = 1)
  # if parsed.value.hasCTRC:
  #   if parsed.dst.hasCTRC:
  #     if parsed.dst.isInfixFuncCall:
  #       if not parsed.dst[1].ctrc.depend(parsed.value.ctrc):
  #         parsed.value.error("$# has been destroyed." % $parsed.value)
      # if not parsed.value.ctrc.inc:
      #   parsed.value.error("$# has been destroyed." % $parsed.value)

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

  if fexpr[1].hasCTRC:
    if fexpr[1].ctrc.hasKey(name(fieldname)):
      fexpr.ctrc = fexpr[1].ctrc[name(fieldname)].ctrc

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

  fexpr.ctrc = initCTRC()
  # fexpr.ctrc = initCTRCWithFields(fexpr.typ.fexpr.deftype.body)
  # for i, b in fexpr[2]:
  #   if b.hasCTRC:
  #     # if not b.ctrc.inc:
  #     #   b.error("$# has been destroyed!" % $b)
  #     fexpr.ctrc[name(fexpr.typ.fexpr.deftype.body[i][0])] = b
  #     # fexpr.ctrc[name(fexpr.typ.fexpr.deftype.body[i][0])].ctrc = b.ctrc

  let argtypes = fexpr[2].mapIt(it.typ)

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
  scope.addInternalEval(name("track"), semTrack)
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
