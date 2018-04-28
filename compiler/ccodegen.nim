
import fexpr_core

import tables
import options
import strutils, sequtils
import os
import algorithm

type
  SrcExpr* = object
    prev*: string
    exp*: string
  CCodegenContext* = ref object
    headers*: OrderedTable[string, bool]
    typesrc*: string
    fnsrc*: string
    fndeclsrc*: string
    headersrc*: string
    cdeclsrc*: string
    cheadsrc*: string
    tmpcount*: int
    macrogen*: bool
    
proc codegenFExpr*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr)
proc codegenCallArg*(ctx: CCodegenContext, src: var SrcExpr, arg: FExpr, fnargtype: Symbol)

proc initSrcExpr*(): SrcExpr =
  result.prev = ""
  result.exp = ""
proc `&=`*(src: var SrcExpr, s: string) = src.exp &= s
proc `&=`*(src: var SrcExpr, s: SrcExpr) =
  src.exp &= s.prev
  src.exp &= s.exp
proc addPrev*(src: var SrcExpr, s: string) = src.prev &= s
proc addPrev*(src: var SrcExpr, s: SrcExpr) =
  src.prev &= s.prev
  src.prev &= s.exp

proc newCCodegenContext*(macrogen = false): CCodegenContext =
  CCodegenContext(headers: initOrderedTable[string, bool](), typesrc: "", fnsrc: "", fndeclsrc: "", headersrc: "", cdeclsrc: "", cheadsrc: "", tmpcount: 0, macrogen: macrogen)
proc gentmpsym*(ctx: CCodegenContext): string =
  result = "__floritmp" & $ctx.tmpcount
  ctx.tmpcount.inc

proc replaceSpecialSymbols*(s: string): string =
  s.replace(".", "_").replace("+", "plus").replace("-", "minus").replace("*", "asterisk").replace("/", "slash").replace("!", "excl").replace("=", "eq").replace("%", "per").replace("&", "and")

proc codegenSymbol*(sym: Symbol): string

proc codegenVarfnSymbol*(sym: Symbol): string =
  result = replaceSpecialSymbols($sym.scope.name & "_" & $sym.name)

proc codegenSymbol*(sym: Symbol): string =
  result = ""
  if sym.kind == symbolTypeGenerics and sym.types.len != 0:
    result &= $sym.scope.name & "_" & $sym.name & "_" & sym.types.mapIt(codegenSymbol(it)).join("_")
  elif sym.kind == symbolVar:
    result &= codegenSymbol(sym.wrapped)
  elif sym.kind == symbolRef:
    result &= codegenSymbol(sym.wrapped)
  elif sym.kind == symbolMove:
    result &= "move_" & codegenSymbol(sym.wrapped)
  elif sym.kind == symbolIntLit:
    result &= $sym.intval
  elif sym.kind == symbolFuncType:
    result &= $sym.scope.name & "_fn_$#_$#" % [sym.argtypes.mapIt(codegenSymbol(it)).join("_"), codegenSymbol(sym.rettype)]
  else:
    result &= $sym.scope.name & "_" & $sym.name
  result = result.replaceSpecialSymbols()
proc codegenSymbol*(fexpr: FExpr): string =
  if fexpr.kind != fexprSymbol:
    fexpr.error("$# isn't symbol." % $fexpr)
  return codegenSymbol(fexpr.symbol)

proc codegenType*(ctx: CCodegenContext, sym: Symbol, share = false): string
  
proc codegenTypePattern*(ctx: CCodegenContext, pattern: string, types: seq[Symbol]): string =
  result = pattern
  for i, typ in types:
    result = result.replace("##" & $(i+1), codegenSymbol(typ))
    result = result.replace("#" & $(i+1), ctx.codegenType(typ))
proc codegenTypeImportc*(ctx: CCodegenContext, sym: Symbol): string =
  if sym.fexpr.hasInternalPragma and sym.fexpr.internalPragma.header.isSome and not ctx.headers.hasKey(sym.fexpr.internalPragma.header.get):
    ctx.headers[sym.fexpr.internalPragma.header.get] = true
    ctx.headersrc &= "#include \"$#\"\n" % sym.fexpr.internalPragma.header.get
  if sym.fexpr.internalPragma.patternc.isSome:
    ctx.codegenTypePattern(sym.fexpr.internalPragma.patternc.get, sym.types)
  else:
    sym.fexpr.internalPragma.importc.get

proc codegenType*(ctx: CCodegenContext, sym: Symbol, share = false): string =
  if sym.kind == symbolVar:
    return ctx.codegenType(sym.wrapped, share)
  elif sym.kind == symbolRef:
    return ctx.codegenType(sym.wrapped, share) & "*"
  elif sym.kind == symbolMove:
    return ctx.codegenType(sym.wrapped, share)
  elif sym.kind == symbolFuncType:
    return "$# (*)($#)" % [ctx.codegenType(sym.rettype, share), sym.argtypes.mapIt(ctx.codegenType(it, share)).join(", ")]
  
  if sym.fexpr.hasInternalPragma and sym.fexpr.internalPragma.importc.isSome:
    return ctx.codegenTypeImportc(sym)
  result = ""
  if sym.fexpr.isCStruct:
    result &= "struct "
  result &= codegenSymbol(sym)
proc codegenType*(ctx: CCodegenContext, fexpr: FExpr): string =
  if fexpr.kind != fexprSymbol:
    fexpr.error("$# isn't symbol." % $fexpr)
  return ctx.codegenType(fexpr.symbol)

proc codegenMangling*(sym: Symbol, generics: seq[Symbol], types: seq[Symbol], internal = false): string =
  if internal:
    result = $sym.name & "G" & generics.mapIt(codegenSymbol(it)).join("_") & "G" & "_" & types.mapIt(codegenSymbol(it)).join("_")
  else:
    result = codegenSymbol(sym) & "G" & generics.mapIt(codegenSymbol(it)).join("_") & "G" & "_" & types.mapIt(codegenSymbol(it)).join("_")

iterator codegenArgs*(ctx: CCodegenContext, src: var SrcExpr, args: FExpr): FExpr =
  if args.len >= 1:
    yield(args[0])
  if args.len >= 2:
    for i in 1..<args.len:
      src &= ", "
      yield(args[i])
iterator codegenArgsWithIndex*(ctx: CCodegenContext, src: var SrcExpr, args: FExpr): (int, FExpr) =
  if args.len >= 1:
    yield(0, args[0])
  if args.len >= 2:
    for i in 1..<args.len:
      src &= ", "
      yield(i, args[i])

proc codegenBody*(ctx: CCodegenContext, src: var SrcExpr, body: seq[FExpr], ret: string = nil, rettype: Symbol = nil) =
  if body.len == 0:
    return
  if body.len >= 2:
    for b in body[0..^2]:
      var newsrc = initSrcExpr()
      ctx.codegenFExpr(newsrc, b)
      src &= newsrc.prev
      if newsrc.exp.len != 0:
        src &= newsrc.exp & ";\n"
  var newsrc = initSrcExpr()
  if not rettype.isNil:
    ctx.codegenCallArg(newsrc, body[^1], rettype)
  else:
    ctx.codegenFExpr(newsrc, body[^1])
  src &= newsrc.prev
  if ret != nil:
    src &= ret
  if newsrc.exp != "":
    src &= newsrc.exp & ";\n"
proc codegenBody*(ctx: CCodegenContext, src: var SrcExpr, body: FExpr, ret: string = nil, rettype: Symbol = nil) =
  ctx.codegenBody(src, toSeq(body.items), ret, rettype)

proc codegenDefnInstance*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.internalPragma.inline:
    return
  
  var decl = initSrcExpr()
  decl &= ctx.codegenType(fexpr.defn.ret)
  decl &= " "
  if fexpr.internalPragma.exportc.isSome:
    decl &= fexpr.internalPragma.exportc.get
  else:
    decl &= codegenMangling(fexpr.defn.name.symbol, fexpr.defn.generics.mapIt(it.symbol), fexpr.defn.args.mapIt(it[1].symbol))
  decl &= "("
  for arg in ctx.codegenArgs(decl, fexpr.defn.args):
    if arg[1].symbol.kind == symbolFuncType:
      decl &= "$# (*$#)($#)" % [
        ctx.codegenType(arg[1].symbol.rettype),
        codegenSymbol(arg[0]),
        arg[1].symbol.argtypes.mapIt(ctx.codegenType(it)).join(", ")
      ]
    else:
      decl &= ctx.codegenType(arg[1])
      decl &= " "
      decl &= codegenSymbol(arg[0])
  decl &= ")"

  ctx.fndeclsrc &= decl.exp & ";\n"

  src &= decl
  src &= " {\n"
  if fexpr.defn.body.len != 0:
    if fexpr.defn.body[^1].typ.isVoidType:
      ctx.codegenBody(src, fexpr.defn.body)
    else:
      ctx.codegenBody(src, fexpr.defn.body, ret = "return ", fexpr.defn.ret.symbol)
  src &= "}\n"

proc codegenMacroWrapper*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let mangling = codegenMangling(fexpr.defn.name.symbol, fexpr.defn.generics.mapIt(it.symbol), fexpr.defn.args.mapIt(it[1].symbol))
  src &= "flori_fexpr $#_macro(flori_fexpr fexpr) {\n" % mangling
  src &= "return " & mangling & "("
  for i, arg in ctx.codegenArgsWithIndex(src, fexpr.defn.args):
    src &= "flori_access(fexpr, $#)" % $i
  src &= ");\n"
  src &= "}\n"

proc codegenDefn*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.internalPragma.importc.isNone:
    if fexpr.defn.generics.isSpecTypes:
      ctx.codegenDefnInstance(src, fexpr)
      if $fexpr[0] == "macro":
        ctx.codegenMacroWrapper(src, fexpr)
        
proc codegenDeftypeStruct*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  ctx.typesrc &= "struct $# {\n" % codegenSymbol(fexpr.deftype.name.symbol)
  for field in fexpr.deftype.body:
    ctx.typesrc &= ctx.codegenType(field[1].symbol)
    ctx.typesrc &= " "
    ctx.typesrc &= $field[0]
    ctx.typesrc &= ";\n"
  ctx.typesrc &= "};\n"

proc codegenDeftypePattern*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.internalPragma.declc.isSome:
    ctx.typesrc &= ctx.codegenTypePattern(fexpr.internalPragma.declc.get, fexpr.deftype.name.symbol.types)
    ctx.typesrc &= "\n"
  
proc codegenDeftype*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.internalPragma.importc.isSome:
    if not fexpr.deftype.isGenerics:
      ctx.codegenDeftypePattern(src, fexpr)
  else:
    if not fexpr.deftype.isGenerics:
      ctx.codegenDeftypeStruct(src, fexpr)

proc codegenIf*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let tmpret = ctx.gentmpsym()

  if not fexpr.typ.isVoidType: # temporary return variable declaration.
    src.prev &= ctx.codegenType(fexpr.typ) & " " & tmpret & ";\n"
  let ret = if not fexpr.typ.isVoidType:
              tmpret & " = "
            else:
              nil

  let elifbranch = fexpr.internalIfExpr.elifbranch

  var ifcondsrc = initSrcExpr()
  var ifbodysrc = initSrcExpr()
  ctx.codegenCallArg(ifcondsrc, elifbranch[0].cond, if elifbranch[0].cond.typ.kind == symbolRef: elifbranch[0].cond.typ.wrapped else: elifbranch[0].cond.typ)
  ctx.codegenBody(ifbodysrc, elifbranch[0].body, ret)
  var elsecnt = 1
  src.prev &= ifcondsrc.prev
  src.prev &= "if (" & ifcondsrc.exp & ") {\n"
  src.addPrev(ifbodysrc)
  src.prev &= "} else {"

  for branch in elifbranch[1..^1]:
    var elifcondsrc = initSrcExpr()
    var elifbodysrc = initSrcExpr()
    ctx.codegenCallArg(elifcondsrc, branch.cond, if branch.cond.typ.kind == symbolRef: branch.cond.typ.wrapped else: branch.cond.typ)
    ctx.codegenBody(elifbodysrc, branch.body, ret)
    src.prev &= elifcondsrc.prev
    src.prev &= "if (" & elifcondsrc.exp & ") {\n"
    src.addPrev(elifbodysrc)
    src.prev &= "} else {"
    elsecnt += 1

  var elsebodysrc = initSrcExpr()
  ctx.codegenBody(elsebodysrc, fexpr.internalIfExpr.elsebranch, ret)
  src.addPrev(elsebodysrc)
  src.prev &= "}".repeat(elsecnt)

  # return temporary variable.
  if not fexpr.typ.isVoidType:
    src &= tmpret

proc codegenWhile*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= "while ("
  ctx.codegenFExpr(src, fexpr.whileexpr.cond[0])
  src &= ") {\n"
  ctx.codegenBody(src, fexpr.whileexpr.body)
  src &= "}"
  
proc codegenVar*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= ctx.codegenType(fexpr[2])
  src &= " "
  src &= codegenSymbol(fexpr[1])

proc codegenConst*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= "#define "
  src &= codegenSymbol(fexpr[1][1])
  src &= " "
  ctx.codegenFExpr(src, fexpr[2])
  src &= "\n"
  
proc codegenDef*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= ctx.codegenType(fexpr.defexpr.value.typ)
  src &= " "
  src &= codegenSymbol(if fexpr.defexpr.name.kind == fexprSymbol: fexpr.defexpr.name else: fexpr.defexpr.name[0])
  src &= " = "
  ctx.codegenFExpr(src, fexpr.defexpr.value)

proc codegenDefDecl*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let t = ctx.codegenType(fexpr.defexpr.value.typ)
  let n = codegenSymbol(if fexpr.defexpr.name.kind == fexprSymbol: fexpr.defexpr.name else: fexpr.defexpr.name[0])
  src &= "$# $#;\n" % [t, n]
proc codegenDefValue*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= codegenSymbol(if fexpr.defexpr.name.kind == fexprSymbol: fexpr.defexpr.name else: fexpr.defexpr.name[0])
  src &= " = "
  ctx.codegenFExpr(src, fexpr.defexpr.value)

proc codegenSet*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let dsttyp = fexpr.setexpr.dst.typ
  if dsttyp.kind == symbolRef:
    src &= "*"
  ctx.codegenFExpr(src, fexpr.setexpr.dst)
  src &= " = "
  ctx.codegenCallArg(src, fexpr.setexpr.value, dsttyp)

proc codegenFieldAccess*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  ctx.codegenFExpr(src, fexpr.fieldaccessexpr.value)
  if fexpr.fieldaccessexpr.value.typ.kind == symbolRef:
    src &= "->"
  else:
    src &= "."
  ctx.codegenFExpr(src, fexpr.fieldaccessexpr.fieldname)

proc codegenInit*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= "(" & ctx.codegenType(fexpr.initexpr.typ) & "){"
  for i, arg in ctx.codegenArgsWithIndex(src, fexpr.initexpr.body):
    ctx.codegenCallArg(src, arg, fexpr.typ.fexpr.deftype.body[i][1].symbol)
  src &= "}"

proc codegenCodegenDecl*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  ctx.cdeclsrc &= fexpr[1].strval
proc codegenCodegenHead*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  ctx.cheadsrc &= fexpr[1].strval

proc codegenBlock*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= "{\n"
  ctx.codegenFExpr(src, fexpr[1])
  src &= "\n}\n"

proc codegenInternal*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr, topcodegen: bool) =
  if fexpr.hasInternalPragma and not ctx.macrogen and fexpr.internalPragma.compiletime:
    return
  if fexpr.hasInternalPragma and ctx.macrogen and fexpr.internalPragma.nocompiletime:
    return
  if not (fexpr.hasInternalPragma and fexpr.internalPragma.exportc.isSome) and fexpr.isEliminated:
    return
  
  case fexpr.internalMark
  of internalDefn:
    if topcodegen:
      ctx.codegenDefn(src, fexpr)
  of internalMacro:
    if topcodegen and ctx.macrogen:
      ctx.codegenDefn(src, fexpr)
  of internalDeftype:
    if topcodegen:
      ctx.codegenDeftype(src, fexpr)
  of internalTypedef:
    discard
  of internalIf:
    if not topcodegen:
      ctx.codegenIf(src, fexpr)
  of internalWhile:
    if not topcodegen:
      ctx.codegenWhile(src, fexpr)
  of internalVar:
    if topcodegen and fexpr.isToplevel:
      ctx.codegenVar(src, fexpr)
      src &= ";\n"
    elif not topcodegen and not fexpr.isToplevel:
      ctx.codegenVar(src, fexpr)
  of internalConst:
    if topcodegen:
      ctx.codegenConst(src, fexpr)
  of internalDef:
    if topcodegen and fexpr.isToplevel:
      ctx.codegenDefDecl(src, fexpr)
    elif not topcodegen and fexpr.isToplevel:
      ctx.codegenDefValue(src, fexpr)
    else:
      ctx.codegenDef(src, fexpr)
  of internalSet:
    if not topcodegen:
      ctx.codegenSet(src, fexpr)
  of internalFieldAccess:
    if not topcodegen:
      ctx.codegenFieldAccess(src, fexpr)
  of internalInit:
    if not topcodegen:
      ctx.codegenInit(src, fexpr)
  of internalImport:
    discard
  of internalExport:
    discard
  of internalCodegenDecl:
    if topcodegen:
      ctx.codegenCodegenDecl(src, fexpr)
  of internalCodegenHead:
    if topcodegen:
      ctx.codegenCodegenHead(src, fexpr)
  of internalBlock:
    if not topcodegen:
      ctx.codegenBlock(src, fexpr)

proc codegenCallArg*(ctx: CCodegenContext, src: var SrcExpr, arg: FExpr, fnargtype: Symbol) =
  if arg.typ.kind == symbolVar and arg.typ.wrapped.kind == symbolRef and fnargtype.kind == symbolRef:
    ctx.codegenFExpr(src, arg)
  elif arg.typ.kind == symbolRef and fnargtype.kind == symbolRef:
    ctx.codegenFExpr(src, arg)
  elif arg.typ.kind == symbolVar and fnargtype.kind == symbolRef:
    src &= "&"
    ctx.codegenFExpr(src, arg)
  elif arg.typ.kind == symbolRef and fnargtype.kind != symbolRef:
    src &= "*"
    ctx.codegenFExpr(src, arg)
  else:
    ctx.codegenFExpr(src, arg)

proc codegenPatArg*(ctx: CCodegenContext, src: var SrcExpr, arg: FExpr, fnargtype: Symbol) =
  if arg.typ.kind == symbolVar and fnargtype.kind == symbolRef:
    src &= "&"
    ctx.codegenFExpr(src, arg)
  elif arg.typ.kind == symbolRef and fnargtype.kind != symbolRef:
    src &= "*"
    ctx.codegenFExpr(src, arg)
  else:
    ctx.codegenFExpr(src, arg)

proc codegenPatternArgs*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr, fnargtypes: seq[Symbol], ret: Symbol, pattern: var string) =
  pattern = pattern.replace("$#0", ctx.codegenType(ret))
  for i, arg in fexpr.sons.reversed():
    var comp = initSrcExpr()
    ctx.codegenPatArg(comp, arg, fnargtypes[fexpr.len-i-1])
    src.prev &= comp.prev
    pattern = pattern.replace("$#" & $(fexpr.len-i), ctx.codegenType(arg.typ))
    pattern = pattern.replace("$" & $(fexpr.len-i), comp.exp)

proc codegenPatternCCall*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr, pattern: string, fn: FExpr) =
  var pattern = pattern
  var argtypes = fexpr[0].symbol.fexpr.defn.args.mapIt(it[1].symbol)
  if fexpr.isGenericsFuncCall:
    for i, g in fexpr[1]:
      pattern = pattern.replace("#" & $(i+1), ctx.codegenType(g))
    ctx.codegenPatternArgs(src, fexpr[2], argtypes, fexpr.typ, pattern)
  else:
    if fexpr[0].symbol.kind == symbolInfix:
      ctx.codegenPatternArgs(src, fexpr[1..^1], argtypes, fexpr.typ, pattern)
    else:
      ctx.codegenPatternArgs(src, fexpr[1], argtypes, fexpr.typ, pattern)
  src &= pattern

proc codegenCCall*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =  
  let fn = fexpr[0].symbol.fexpr
  let fname = fn.internalPragma.importc.get
  
  if fn.internalPragma.header.isSome and not ctx.headers.hasKey(fn.internalPragma.header.get):
    ctx.headers[fn.internalPragma.header.get] = true
    ctx.headersrc &= "#include \"$#\"\n" % fn.internalPragma.header.get

  if fn.internalPragma.declc.isSome:
    var psrc = initSrcExpr()
    ctx.codegenPatternCCall(psrc, fexpr, fn.internalPragma.declc.get, fn)
    src &= psrc.exp
  
  if fn.internalPragma.infixc:
    if fexpr.len == 3:
      src &= "("
      ctx.codegenCallArg(src, fexpr[1], fexpr[0].symbol.fexpr.defn.args[0][1].symbol)
      src &= " "
      src &= fname
      src &= " "
      ctx.codegenCallArg(src, fexpr[2], fexpr[0].symbol.fexpr.defn.args[1][1].symbol)
      src &= ")"
    elif fexpr[1].len == 2:
      src &= "("
      ctx.codegenCallArg(src, fexpr[1][0], fexpr[0].symbol.fexpr.defn.args[0][1].symbol)
      src &= " "
      src &= fname
      src &= " "
      ctx.codegenCallArg(src, fexpr[1][1], fexpr[0].symbol.fexpr.defn.args[1][1].symbol)
      src &= ")"
    else:
      fexpr.error("$# is not infix expression." % $fexpr)
  elif fn.internalPragma.patternc.isSome:
    ctx.codegenPatternCCall(src, fexpr, fn.internalPragma.patternc.get, fn)
  else:
    src &= fname
    src &= "("
    for i, arg in ctx.codegenArgsWithIndex(src, fexpr[1]):
      ctx.codegenCallArg(src, arg, fexpr[0].symbol.fexpr.defn.args[i][1].symbol)
    src &= ")"
    
proc getCallGenerics(fexpr: FExpr): seq[Symbol] =
  fexpr[0].symbol.fexpr.defn.generics.mapIt(it.symbol)
proc getCallTypes(fexpr: FExpr): seq[Symbol] =
  fexpr[0].symbol.fexpr.defn.args.mapIt(it[1].symbol)
  
proc codegenCall*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.len == 0:
    return
  
  if fexpr[0].kind != fexprSymbol:
    fexpr[0].error("$# isn't symbol." % $fexpr[0])
  let fn = fexpr[0].symbol.fexpr
  if fn.hasinternalPragma and not ctx.macrogen and fn.internalPragma.compiletime:
    return
  elif fn.hasinternalPragma and fn.internalPragma.importc.isSome:
    ctx.codegenCCall(src, fexpr)
  else: # normal call
    if fexpr.isGenericsFuncCall:
      src &= codegenMangling(fexpr[0].symbol, fexpr.getCallGenerics(), fexpr.getCallTypes())
      src &= "("
      for i, arg in ctx.codegenArgsWithIndex(src, fexpr[2]):
        let fnargtype = fexpr[0].symbol.fexpr.defn.args[i][1].symbol
        ctx.codegenCallArg(src, arg, fnargtype)
      src &= ")"
    elif fexpr.len == 3 and fexpr[0].symbol.kind == symbolInfix: # infix call
      src &= codegenMangling(fexpr[0].symbol, fexpr.getCallGenerics(), fexpr.getCallTypes()) # FIXME: support generics
      src &= "("
      ctx.codegenFExpr(src, fexpr[1])
      src &= ", "
      ctx.codegenFExpr(src, fexpr[2])
      src &= ")"
    elif fexpr[0].hasTyp and fexpr[0].typ.kind == symbolFuncType:
      src &= "(("
      src &= codegenVarfnSymbol(fexpr[0].symbol)
      src &= ")("
      for i, arg in ctx.codegenArgsWithIndex(src, fexpr[1]):
        let fnargtype = fexpr[0].typ.argtypes[i]
        ctx.codegenCallArg(src, arg, fnargtype)
      src &= "))"
    else:
      src &= codegenMangling(fexpr[0].symbol, fexpr.getCallGenerics(), fexpr.getCallTypes())
      src &= "("
      for i, arg in ctx.codegenArgsWithIndex(src, fexpr[1]):
        let fnargtype = fexpr[0].symbol.fexpr.defn.args[i][1].symbol
        ctx.codegenCallArg(src, arg, fnargtype)
      src &= ")"

proc codegenFExpr*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  case fexpr.kind
  of fexprIdent:
    src &= $fexpr
  of fexprSymbol:
    if fexpr.symbol.kind == symbolFunc:
      src &= codegenMangling(fexpr.symbol, @[], fexpr.typ.argtypes) # FIXME:
    elif fexpr.typ.kind == symbolFuncType:
      src &= codegenVarfnSymbol(fexpr.symbol)
    elif fexpr.typ.kind == symbolIntLit:
      src &= $fexpr.typ.intval
    else:
      src &= codegenSymbol(fexpr)
  of fexprIntLit:
    src &= $fexpr
  of fexprFloatLit:
    src &= $fexpr
  of fexprStrLit:
    src &= $fexpr
  of fexprArray:
    fexpr.error("unsupported $# in C Codegen." % $fexpr.kind)
  of fexprList: # FIXME: multi element
    if fexpr.len != 1:
      fexpr.error("unsupported mutli element list. in c codegen.")
    src &= "("
    ctx.codegenFExpr(src, fexpr[0])
    src &= ")"
  of fexprBlock:
    if fexpr.len == 0:
      src &= "{}"
      return
    
    if fexpr[^1].typ.isVoidType:
      ctx.codegenBody(src, toSeq(fexpr.items))
    else:
      let tmp = ctx.gentmpsym()
      var blocksrc = initSrcExpr()
      ctx.codegenBody(blocksrc, toSeq(fexpr.items), ret = ctx.codegenType(fexpr[^1].typ) & " " & tmp & " = ")
      src.addPrev(blocksrc)
      src &= tmp
  of fexprSeq:
    if fexpr.hasinternalMark:
      ctx.codegenInternal(src, fexpr, topcodegen = false)
    else:
      ctx.codegenCall(src, fexpr)
  else:
    fexpr.error("$# is unsupported expression in eval." % $fexpr.kind)

proc codegenToplevel*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.hasinternalMark:
    ctx.codegenInternal(src, fexpr, topcodegen = true)
  elif fexpr.kind == fexprBlock:
    for son in fexpr:
      if son.isGenerated:
        continue
      son.isToplevel = true
      ctx.codegenToplevel(src, son)
  
proc codegenSingle*(ctx: CCodegenContext, sem: SemanticContext): string =
  result = ""
  var src = initSrcExpr()
  if ctx.macrogen:
    ctx.headers["floriffi.h"] = true
    result &= "#include \"floriffi.h\"\n"
    result &= "#define FLORI_COMPILETIME\n"
  for d in sem.defines:
    result &= "#define $#\n" % d
  for f in sem.globaltoplevels:
    ctx.codegenToplevel(src, f)
  src &= "\n"
  src &= "void flori_main() {\n"
  ctx.codegenBody(src, sem.globaltoplevels)
  src &= "}\n"
  src &= "int main(int argc, char** argv) { flori_main(); }\n"
  result &= ctx.cheadsrc & "\n" & ctx.headersrc & "\n" & ctx.cdeclsrc & "\n" & ctx.typesrc & "\n" & ctx.fndeclsrc & "\n" & ctx.fnsrc & "\n" & src.exp
