
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
  s.replace(".", "_").replace("+", "plus").replace("-", "minus").replace("*", "asterisk").replace("/", "slash").replace("\\", "slash").replace("!", "excl").replace("=", "eq").replace("%", "per").replace("&", "and")

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
  if sym.fexpr.metadata.header.isSome and not ctx.headers.hasKey($sym.fexpr.metadata.header.get):
    ctx.headers[$sym.fexpr.metadata.header.get] = true
    ctx.headersrc &= "#include \"$#\"\n" % $sym.fexpr.metadata.header.get
  if sym.fexpr.metadata.patternc.isSome:
    ctx.codegenTypePattern($sym.fexpr.metadata.patternc.get, toSeq(sym.types.items))
  else:
    $sym.fexpr.metadata.importc.get

proc codegenType*(ctx: CCodegenContext, sym: Symbol, share = false): string =
  if sym.kind == symbolVar:
    return ctx.codegenType(sym.wrapped, share)
  elif sym.kind == symbolRef:
    return ctx.codegenType(sym.wrapped, share) & "*"
  elif sym.kind == symbolFuncType:
    return "$# (*)($#)" % [ctx.codegenType(sym.rettype, share), sym.argtypes.mapIt(ctx.codegenType(it, share)).join(", ")]

  if sym.fexpr.metadata.importc.isSome:
    return ctx.codegenTypeImportc(sym)
  result = ""
  if sym.fexpr.metadata.isCStruct:
    result &= "struct "
  result &= codegenSymbol(sym)
proc codegenType*(ctx: CCodegenContext, fexpr: FExpr): string =
  if fexpr.kind != fexprSymbol:
    fexpr.error("$# isn't symbol." % $fexpr)
  return ctx.codegenType(fexpr.symbol)

proc codegenMangling*(sym: Symbol, generics: seq[Symbol], types: seq[Symbol], internal = false): string =
  if internal:
    result = $sym.name & "G" & generics.mapIt(codegenSymbol(it)).join("_") & "G" & "_" & types.mapIt(codegenSymbol(it)).join("_")
  elif sym.fexpr.metadata.exportc.isSome:
    result = $sym.fexpr.metadata.exportc.get
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

proc codegenBody*(ctx: CCodegenContext, src: var SrcExpr, body: seq[FExpr], ret: string = nil, rettype = symbolNil()) =
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
proc codegenBody*(ctx: CCodegenContext, src: var SrcExpr, body: FExpr, ret: string = nil, rettype = symbolNil()) =
  ctx.codegenBody(src, toSeq(body.items), ret, rettype)

proc codegenDefnDecl*(ctx: CCodegeNContext, src: var SrcExpr, fexpr: FExpr, namepre = "", namepost = "") =
  src &= ctx.codegenType(fexpr.getReturn().get)
  src &= " "
  src &= namepre
  if fexpr.metadata.exportc.isSome:
    src &= $fexpr.metadata.exportc.get
  else:
    src &= codegenMangling(fexpr.getName().get.symbol, fexpr.getGenerics().get.mapIt(it.symbol), fexpr.getArguments().get.mapIt(it[1].symbol))
  src &= namepost
  src &= "("
  for arg in ctx.codegenArgs(src, fexpr.getArguments().get):
    if arg[1].symbol.kind == symbolFuncType:
      src &= "$# (*$#)($#)" % [
        ctx.codegenType(arg[1].symbol.rettype),
        codegenSymbol(arg[0]),
        arg[1].symbol.argtypes.mapIt(ctx.codegenType(it)).join(", ")
      ]
    else:
      src &= ctx.codegenType(arg[1])
      src &= " "
      src &= codegenSymbol(arg[0])
  src &= ")"
  
proc codegenDefnInstance*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  var decl = initSrcExpr()
  ctx.codegenDefnDecl(decl, fexpr)
  ctx.fndeclsrc &= decl.exp & ";\n"

  src &= decl
  src &= " {\n"
  let body = fexpr.getFnBody().get
  if body.len != 0:
    if body[^1].metadata.typ.isVoidType:
      ctx.codegenBody(src, body)
    else:
      ctx.codegenBody(src, body, ret = "return ", fexpr.getReturn().get.symbol)
  src &= "}\n"

proc codegenMacroWrapper*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let mangling = codegenMangling(fexpr.getName().get.symbol, fexpr.getGenerics().get.mapIt(it.symbol), fexpr.getArguments().get.mapIt(it[1].symbol))
  src &= "flori_fexpr $#_macro(flori_fexpr fexpr) {\n" % mangling
  src &= "return " & mangling & "("
  for i, arg in ctx.codegenArgsWithIndex(src, fexpr.getArguments().get):
    src &= "flori_access(fexpr, $#)" % $i
  src &= ");\n"
  src &= "}\n"

proc codegenDefn*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.metadata.importc.isNone:
    if fexpr.getGenerics().get.isSpecTypes:
      ctx.codegenDefnInstance(src, fexpr)
      if $fexpr[0] == "macro":
        ctx.codegenMacroWrapper(src, fexpr)
        
proc codegenDeftypeStruct*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  ctx.typesrc &= "struct $# {\n" % codegenSymbol(fexpr.getName().get.symbol)
  for field in fexpr.getTypeBody().get:
    if field[1].symbol.kind == symbolFuncType:
      ctx.typesrc &= "$# (*$#)($#);\n" % [
        ctx.codegenType(field[1].symbol.rettype),
        $field[0],
        field[1].symbol.argtypes.mapIt(ctx.codegenType(it)).join(", ")
      ]
    else:
      ctx.typesrc &= ctx.codegenType(field[1].symbol)
      ctx.typesrc &= " "
      ctx.typesrc &= $field[0]
      ctx.typesrc &= ";\n"
  ctx.typesrc &= "};\n"

proc codegenDeftypePattern*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.metadata.declc.isSome:
    ctx.typesrc &= ctx.codegenTypePattern($fexpr.metadata.declc.get, toSeq(fexpr.getName().get.symbol.types.items))
    ctx.typesrc &= "\n"
  
proc codegenDeftype*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.isGenerics:
    return
  
  if fexpr.metadata.importc.isSome:
    ctx.codegenDeftypePattern(src, fexpr)
  else:
    ctx.codegenDeftypeStruct(src, fexpr)

proc codegenIf*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let tmpret = ctx.gentmpsym()

  if not fexpr.metadata.typ.isVoidType: # temporary return variable declaration.
    src.prev &= ctx.codegenType(fexpr.metadata.typ) & " " & tmpret & ";\n"
  let ret = if not fexpr.metadata.typ.isVoidType:
              tmpret & " = "
            else:
              nil

  let branches = fexpr.getIfBranches()

  var ifcondsrc = initSrcExpr()
  var ifbodysrc = initSrcExpr()
  ctx.codegenCallArg(ifcondsrc, branches[0].cond.get, if branches[0].cond.get.metadata.typ.kind == symbolRef: branches[0].cond.get.metadata.typ.wrapped else: branches[0].cond.get.metadata.typ)
  ctx.codegenBody(ifbodysrc, branches[0].body, ret)
  var elsecnt = 1
  src.prev &= ifcondsrc.prev
  src.prev &= "if (" & ifcondsrc.exp & ") {\n"
  src.addPrev(ifbodysrc)
  src.prev &= "} else {"

  for branch in branches[1..^1]:
    if branch.cond.isSome:
      var elifcondsrc = initSrcExpr()
      var elifbodysrc = initSrcExpr()
      ctx.codegenCallArg(elifcondsrc, branch.cond.get, if branch.cond.get.metadata.typ.kind == symbolRef: branch.cond.get.metadata.typ.wrapped else: branch.cond.get.metadata.typ)
      ctx.codegenBody(elifbodysrc, branch.body, ret)
      src.prev &= elifcondsrc.prev
      src.prev &= "if (" & elifcondsrc.exp & ") {\n"
      src.addPrev(elifbodysrc)
      src.prev &= "} else {"
      elsecnt += 1
    else:
      var elsebodysrc = initSrcExpr()
      ctx.codegenBody(elsebodysrc, branch.body, ret)
      src.addPrev(elsebodysrc)
      src.prev &= "}".repeat(elsecnt)

  # return temporary variable.
  if not fexpr.metadata.typ.isVoidType:
    src &= tmpret

proc codegenWhile*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= "while ("
  ctx.codegenFExpr(src, fexpr[1])
  src &= ") {\n"
  ctx.codegenBody(src, fexpr[2])
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
  src &= ctx.codegenType(fexpr[2].metadata.typ)
  src &= " "
  src &= codegenSymbol(if fexpr[1].kind == fexprSymbol: fexpr[1] else: fexpr[1][0])
  src &= " = "
  ctx.codegenFExpr(src, fexpr[2])

proc codegenDefDecl*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let t = ctx.codegenType(fexpr[2].metadata.typ)
  let n = codegenSymbol(if fexpr[1].kind == fexprSymbol: fexpr[1] else: fexpr[1][0])
  src &= "$# $#;\n" % [t, n]
proc codegenDefValue*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= codegenSymbol(if fexpr[1].kind == fexprSymbol: fexpr[1] else: fexpr[1][0])
  src &= " = "
  ctx.codegenFExpr(src, fexpr[2])

proc codegenSet*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let dsttyp = fexpr[1].metadata.typ
  if dsttyp.isRef and not fexpr[2].metadata.typ.isRef:
    src &= "*"
  ctx.codegenFExpr(src, fexpr[1])
  src &= " = "
  ctx.codegenCallArg(src, fexpr[2], dsttyp)

proc codegenFieldAccess*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  ctx.codegenFExpr(src, fexpr[1])
  if fexpr[1].metadata.typ.kind == symbolRef:
    src &= "->"
  elif fexpr[1].metadata.typ.kind == symbolVar and fexpr[1].metadata.typ.wrapped.kind == symbolRef:
    src &= "->"
  else:
    src &= "."
  ctx.codegenFExpr(src, fexpr[2])

proc codegenCodegenDecl*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  ctx.cdeclsrc &= fexpr[1].strval
proc codegenCodegenHead*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  ctx.cheadsrc &= fexpr[1].strval

proc codegenBlock*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= "{\n"
  ctx.codegenFExpr(src, fexpr[1])
  src &= "\n}\n"

proc codegenInternal*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr, topcodegen: bool) =
  if (not fexpr.metadata.exportc.isSome) and fexpr.metadata.isEliminated:
    return
  
  case fexpr.metadata.internal
  of internalNone:
    discard
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
    if topcodegen and fexpr.metadata.isToplevel:
      ctx.codegenVar(src, fexpr)
      src &= ";\n"
    elif not topcodegen and not fexpr.metadata.isToplevel:
      ctx.codegenVar(src, fexpr)
  of internalConst:
    if topcodegen:
      ctx.codegenConst(src, fexpr)
  of internalDef:
    if topcodegen and fexpr.metadata.isToplevel:
      ctx.codegenDefDecl(src, fexpr)
    elif not topcodegen and fexpr.metadata.isToplevel:
      ctx.codegenDefValue(src, fexpr)
    else:
      ctx.codegenDef(src, fexpr)
  of internalSet:
    if not topcodegen:
      ctx.codegenSet(src, fexpr)
  of internalFieldAccess:
    if not topcodegen:
      ctx.codegenFieldAccess(src, fexpr)
  of internalImport, internalExport, internalReload:
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
  if arg.metadata.typ.isRef and fnargtype.isRef:
    ctx.codegenFExpr(src, arg)
  elif arg.metadata.typ.kind == symbolVar and fnargtype.isRef:
    src &= "&"
    ctx.codegenFExpr(src, arg)
  elif arg.metadata.typ.isRef and not fnargtype.isRef:
    src &= "*"
    ctx.codegenFExpr(src, arg)
  else:
    ctx.codegenFExpr(src, arg)

proc codegenPatArg*(ctx: CCodegenContext, src: var SrcExpr, arg: FExpr, fnargtype: Symbol) =
  if arg.metadata.typ.kind == symbolVar and fnargtype.kind == symbolRef:
    src &= "&"
    ctx.codegenFExpr(src, arg)
  elif arg.metadata.typ.kind == symbolRef and fnargtype.kind != symbolRef:
    src &= "*"
    ctx.codegenFExpr(src, arg)
  else:
    ctx.codegenFExpr(src, arg)

proc codegenPatternArgs*(ctx: CCodegenContext, src: var SrcExpr, sons: seq[FExpr], fnargtypes: seq[Symbol], ret: Symbol, pattern: var string) =
  pattern = pattern.replace("$#0", ctx.codegenType(ret))
  for i, arg in sons.reversed():
    var comp = initSrcExpr()
    ctx.codegenPatArg(comp, arg, fnargtypes[sons.len-i-1])
    src.prev &= comp.prev
    pattern = pattern.replace("$#" & $(sons.len-i), ctx.codegenType(arg.metadata.typ))
    pattern = pattern.replace("$" & $(sons.len-i), comp.exp)

proc codegenPatternArgs*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr, fnargtypes: seq[Symbol], ret: Symbol, pattern: var string) =
  codegenPatternArgs(ctx, src, toSeq(fexpr.sons.items), fnargtypes, ret, pattern)

proc codegenPatternCCall*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr, pattern: string, fn: FExpr) =
  var pattern = pattern
  var argtypes = fexpr[0].symbol.fexpr.getArguments().get.mapIt(it[1].symbol)
  if fexpr.isGenericsFuncCall:
    for i, g in fexpr[1]:
      pattern = pattern.replace("#" & $(i+1), ctx.codegenType(g))
    ctx.codegenPatternArgs(src, fexpr[2], argtypes, fexpr.metadata.typ, pattern)
  else:
    if fexpr.isNormalFuncCall:
      ctx.codegenPatternArgs(src, fexpr[1], argtypes, fexpr.metadata.typ, pattern)
    elif fexpr.isInfixFuncCall:
      ctx.codegenPatternArgs(src, toSeq(fexpr.items)[1..^1], argtypes, fexpr.metadata.typ, pattern)
    else:
      fexpr.error("unsupported c function call syntax.")
  src &= pattern

proc codegenCCall*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let fn = fexpr[0].symbol.fexpr
  let fname = $fn.metadata.importc.get
  
  if fn.metadata.header.isSome and not ctx.headers.hasKey($fn.metadata.header.get):
    ctx.headers[$fn.metadata.header.get] = true
    ctx.headersrc &= "#include \"$#\"\n" % $fn.metadata.header.get

  if fn.metadata.declc.isSome:
    var psrc = initSrcExpr()
    ctx.codegenPatternCCall(psrc, fexpr, $fn.metadata.declc.get, fn)
    src &= psrc.exp
  
  let args = fexpr[0].symbol.fexpr.getArguments().get
  if fn.metadata.infixc:
    if fexpr.len == 3:
      src &= "("
      ctx.codegenCallArg(src, fexpr[1], args[0][1].symbol)
      src &= " "
      src &= fname
      src &= " "
      ctx.codegenCallArg(src, fexpr[2], args[1][1].symbol)
      src &= ")"
    elif fexpr[1].len == 2:
      src &= "("
      ctx.codegenCallArg(src, fexpr[1][0], args[0][1].symbol)
      src &= " "
      src &= fname
      src &= " "
      ctx.codegenCallArg(src, fexpr[1][1], args[1][1].symbol)
      src &= ")"
    else:
      fexpr.error("$# is not infix expression." % $fexpr)
  elif fn.metadata.patternc.isSome:
    ctx.codegenPatternCCall(src, fexpr, $fn.metadata.patternc.get, fn)
  elif fexpr.isInfixFuncCall:
    src &= fname
    src &= "("
    ctx.codegenCallArg(src, fexpr[1], args[0][1].symbol)
    src &= ", "
    ctx.codegenCallArg(src, fexpr[2], args[1][1].symbol)
    src &= ")"
  else:
    src &= fname
    src &= "("
    for i, arg in ctx.codegenArgsWithIndex(src, fexpr[1]):
      ctx.codegenCallArg(src, arg, args[i][1].symbol)
    src &= ")"
    
proc getCallGenerics(fexpr: FExpr): seq[Symbol] =
  if fexpr[0].symbol.kind == symbolFuncType:
    @[]
  else:
    fexpr[0].symbol.fexpr.getGenerics().get.mapIt(it.symbol)
proc getCallTypes(fexpr: FExpr): seq[Symbol] =
  if fexpr[0].symbol.kind == symbolFuncType:
    toSeq(fexpr[0].symbol.argtypes.items)
  else:
    fexpr[0].symbol.fexpr.getArguments().get.mapIt(it[1].symbol)
  
proc codegenCall*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.len == 0:
    return

  if fexpr[0].kind notin fexprAllNames:
    src &= "(("
    ctx.codegenFExpr(src, fexpr[0])
    src &= ")("
    for i, arg in ctx.codegenArgsWithIndex(src, fexpr[1]):
      let fnargtype = fexpr[0].metadata.typ.argtypes[i]
      ctx.codegenCallArg(src, arg, fnargtype)
    src &= "))"
    return
  
  let fn = fexpr[0].symbol.fexpr
  if fn.metadata.importc.isSome:
    ctx.codegenCCall(src, fexpr)
  else: # normal call
    if fexpr.isNormalFuncCall:
      src &= codegenMangling(fexpr[0].symbol, fexpr.getCallGenerics(), fexpr.getCallTypes())
      src &= "("
      for i, arg in ctx.codegenArgsWithIndex(src, fexpr[1]):
        let fnargtype = fexpr[0].symbol.fexpr.getArguments().get[i][1].symbol
        ctx.codegenCallArg(src, arg, fnargtype)
      src &= ")"
    elif fexpr.isGenericsFuncCall:
      src &= codegenMangling(fexpr[0].symbol, fexpr.getCallGenerics(), fexpr.getCallTypes())
      src &= "("
      for i, arg in ctx.codegenArgsWithIndex(src, fexpr[2]):
        let fnargtype = fexpr[0].symbol.fexpr.getArguments().get[i][1].symbol
        ctx.codegenCallArg(src, arg, fnargtype)
      src &= ")"
    elif fexpr.isInfixFuncCall: # infix call
      src &= codegenMangling(fexpr[0].symbol, fexpr.getCallGenerics(), fexpr.getCallTypes()) # FIXME: support generics
      src &= "("
      ctx.codegenCallArg(src, fexpr[1], fexpr[0].symbol.fexpr.getArguments().get[0][1].symbol)
      src &= ", "
      ctx.codegenCallArg(src, fexpr[2], fexpr[0].symbol.fexpr.getArguments().get[1][1].symbol)
      src &= ")"
    elif fexpr[0].hasTyp and fexpr[0].metadata.typ.kind == symbolFuncType:
      src &= "(("
      src &= codegenVarfnSymbol(fexpr[0].symbol)
      src &= ")("
      for i, arg in ctx.codegenArgsWithIndex(src, fexpr[1]):
        let fnargtype = fexpr[0].metadata.typ.argtypes[i]
        ctx.codegenCallArg(src, arg, fnargtype)
      src &= "))"
    else:
      fexpr.error("unsupported function call syntax.")

proc codegenFExpr*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  case fexpr.kind
  of fexprIdent:
    src &= $fexpr
  of fexprSymbol:
    if fexpr.symbol.kind == symbolFunc:
      src &= codegenMangling(fexpr.symbol, @[], toSeq(fexpr.metadata.typ.argtypes.items)) # FIXME:
    elif fexpr.metadata.typ.kind == symbolFuncType:
      src &= codegenVarfnSymbol(fexpr.symbol)
    elif fexpr.metadata.typ.kind == symbolIntLit:
      src &= $fexpr.metadata.typ.intval
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
    
    if fexpr[^1].metadata.typ.isVoidType:
      ctx.codegenBody(src, toSeq(fexpr.items))
    else:
      let tmp = ctx.gentmpsym()
      var blocksrc = initSrcExpr()
      ctx.codegenBody(blocksrc, toSeq(fexpr.items), ret = ctx.codegenType(fexpr[^1].metadata.typ) & " " & tmp & " = ")
      src.addPrev(blocksrc)
      src &= tmp
  of fexprSeq:
    if fexpr.metadata.internal != internalNone:
      ctx.codegenInternal(src, fexpr, topcodegen = false)
    else:
      ctx.codegenCall(src, fexpr)
  else:
    fexpr.error("$# is unsupported expression in eval." % $fexpr.kind)

proc codegenToplevel*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.metadata.internal != internalNone:
    ctx.codegenInternal(src, fexpr, topcodegen = true)
  elif fexpr.kind == fexprBlock:
    for son in fexpr:
      ctx.codegenToplevel(src, son)

proc collectDependFn*(s: var seq[Symbol], fexpr: FExpr) =
  if fexpr.isFuncCall and fexpr[0].kind == fexprSymbol:
    s.add(fexpr[0].symbol)
  if fexpr.kind in fexprContainer:
    for f in fexpr:
      collectDependFn(s, f)
      
proc codegenDynFn*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  var depends = newSeq[Symbol]()
  collectDependFn(depends, fexpr.getFnBody().get)
  for d in depends:
    ctx.codegenDefnDecl(src, d.fexpr, "(*", ")")
    src &= ";\n"
  ctx.codegenDefn(src, fexpr)
proc codegenDynFn*(ctx: CCodegenContext, fexpr: FExpr): string =
  var src = initSrcExpr()
  ctx.codegenDynFn(src, fexpr)
  ctx.cheadsrc & "\n" & ctx.headersrc & "\n" & ctx.cdeclsrc & "\n" & ctx.typesrc & "\n" & src.exp
  
proc codegenSingle*(ctx: CCodegenContext, sem: SemContext): string =
  result = ""
  var src = initSrcExpr()
  if ctx.macrogen:
    ctx.headers["floriffi.h"] = true
    result &= "#include \"floriffi.h\"\n"
    result &= "#define FLORI_COMPILETIME\n"
  for d in sem.defines:
    result &= "#define $#\n" % d
  result &= "int gArgc;\n"
  result &= "char** gArgv;\n"
  for f in sem.globaltoplevels:
    ctx.codegenToplevel(src, f)
  src &= "\n"
  src &= "void flori_main() {\n"
  ctx.codegenBody(src, sem.globaltoplevels)
  src &= "}\n"
  src &= "int main(int argc, char** argv) { gArgc = argc, gArgv = argv; flori_main(); }\n"
  result &= ctx.cheadsrc & "\n" & ctx.headersrc & "\n" & ctx.cdeclsrc & "\n" & ctx.typesrc & "\n" & ctx.fndeclsrc & "\n" & ctx.fnsrc & "\n" & src.exp
