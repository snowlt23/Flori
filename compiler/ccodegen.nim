
import types, fexpr, scope, metadata

import tables
import options
import strutils, sequtils
import os

type
  SrcExpr* = object
    headers*: Table[string, bool]
    typedecls*: string
    fndecls*: string
    vardecls*: string
    prev*: string
    exp*: string
  CCodegenContext* = ref object
    modulesrcs*: OrderedTable[Name, SrcExpr]
    tmpcount*: int
    macrogen*: bool
    
proc codegenFExpr*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr)

proc initSrcExpr*(): SrcExpr =
  result.headers = initTable[string, bool]()
  result.typedecls = ""
  result.fndecls = ""
  result.vardecls = ""
  result.prev = ""
  result.exp = ""
proc `&=`*(src: var SrcExpr, s: string) = src.exp &= s
proc extendDecls*(src: var SrcExpr, s: SrcExpr) =
  src.typedecls &= s.typedecls
  src.fndecls &= s.fndecls
  src.vardecls &= s.vardecls
proc `&=`*(src: var SrcExpr, s: SrcExpr) =
  src.extendDecls(s)
  src.exp &= s.prev
  src.exp &= s.exp
proc addPrev*(src: var SrcExpr, s: string) = src.prev &= s
proc addPrev*(src: var SrcExpr, s: SrcExpr) =
  src.extendDecls(s)
  src.prev &= s.prev
  src.prev &= s.exp
proc getSrc*(src: SrcExpr): string =
  "#include \"flori_decls.h\"\n\n" & src.exp
proc addHeader*(src: var SrcExpr, s: string) =
  src.headers[s] = true

proc newCCodegenContext*(macrogen = false): CCodegenContext =
  CCodegenContext(modulesrcs: initOrderedTable[Name, SrcExpr](), tmpcount: 0, macrogen: macrogen)
proc gentmpsym*(ctx: CCodegenContext): string =
  result = "__floritmp" & $ctx.tmpcount
  ctx.tmpcount.inc

proc generateInitDecls*(ctx: CCodegenContext): string =
  result = ""
  for si, module in ctx.modulesrcs:
    result &= "void __flori_" & replace($si, ".", "_") & "_init();\n"
proc generateInits*(ctx: CCodegenContext): string =
  result = ""
  for si, module in ctx.modulesrcs:
    result &= "__flori_" & replace($si, ".", "_") & "_init();\n"
proc generateFloriMain*(ctx: CCodegenContext): string =
  result = ""
  result &= ctx.generateInitDecls()
  result &= "int flori_main() {\n"
  result &= ctx.generateInits()
  result &= "}\n"
proc generateCMain*(ctx: CCodegenContext): string =
  result = ""
  result &= "int main() {\n"
  result &= "flori_main();\n"
  result &= "}\n"

proc generateFloriDecls*(ctx: CCodegenContext): string =
  result = ""
  if ctx.macrogen:
    result &= "#define FLORI_COMPILETIME\n"
  for src in ctx.modulesrcs.values:
    for header in src.headers.keys:
      result.add("#include \"$#\"\n" % header)
  result &= "\n"
  for src in ctx.modulesrcs.values:
    result.add(src.typedecls)
  for src in ctx.modulesrcs.values:
    result.add(src.fndecls)
  for src in ctx.modulesrcs.values:
    result.add(src.vardecls)
  result &= "\n"

proc replaceSpecialSymbols*(s: string): string =
  s.replace(".", "_").replace("+", "plus").replace("-", "minus").replace("*", "asterisk").replace("/", "slash").replace("!", "excl").replace("=", "eq")

proc codegenSymbol*(sym: Symbol): string

proc codegenSymbol*(sym: Symbol): string =
  result = ""
  if sym.kind == symbolTypeGenerics and sym.types.len != 0:
    result &= $sym.scope.name & "_" & $sym.name & "_" & sym.types.mapIt(codegenSymbol(it)).join("_")
  elif sym.kind == symbolVar and sym.types.len != 0:
    result &= codegenSymbol(sym.types[0])
  elif sym.kind == symbolIntLit:
    result &= $sym.intval
  else:
    result &= $sym.scope.name & "_" & $sym.name
  result = result.replaceSpecialSymbols()
proc codegenSymbol*(fexpr: FExpr): string =
  if fexpr.kind != fexprSymbol:
    fexpr.error("$# isn't symbol." % $fexpr)
  return codegenSymbol(fexpr.symbol)

proc codegenType*(sym: Symbol): string
  
proc codegenTypePattern*(pattern: string, types: seq[Symbol]): string =
  result = pattern
  for i, typ in types:
    result = result.replace("##" & $(i+1), codegenSymbol(typ))
    result = result.replace("#" & $(i+1), codegenType(typ))
proc codegenTypeImportc*(sym: Symbol): string =
  if sym.fexpr.internalPragma.pattern.isSome:
    codegenTypePattern(sym.fexpr.internalPragma.pattern.get, sym.types)
  else:
    sym.fexpr.internalPragma.importc.get

proc codegenType*(sym: Symbol): string =
  if sym.kind == symbolVar and sym.types.len != 0:
    return codegenType(sym.types[0])
  
  if sym.fexpr.hasInternalPragma and sym.fexpr.internalPragma.importc.isSome:
    return codegenTypeImportc(sym)
  result = ""
  if sym.fexpr.hasCStruct:
    result &= "struct "
  result &= codegenSymbol(sym)
proc codegenType*(fexpr: FExpr): string =
  if fexpr.kind != fexprSymbol:
    fexpr.error("$# isn't symbol." % $fexpr)
  return codegenType(fexpr.symbol)

proc codegenMangling*(sym: Symbol, generics: seq[Symbol], types: seq[Symbol]): string =
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

proc codegenBody*(ctx: CCodegenContext, src: var SrcExpr, body: seq[FExpr], ret: string = nil) =
  if body.len == 0:
    return
  if body.len >= 2:
    for b in body[0..^2]:
      var newsrc = initSrcExpr()
      ctx.codegenFExpr(newsrc, b)
      src.extendDecls(newsrc)
      src &= newsrc.prev
      if newsrc.exp != "":
        src &= newsrc.exp & ";\n"
  var newsrc = initSrcExpr()
  ctx.codegenFExpr(newsrc, body[^1])
  src &= newsrc.prev
  if ret != nil:
    src &= ret
  src &= newsrc.exp & ";\n"
proc codegenBody*(ctx: CCodegenContext, src: var SrcExpr, body: FExpr, ret: string = nil) =
  ctx.codegenBody(src, toSeq(body.items), ret)

proc codegenDefnInstance*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  var decl = initSrcExpr()
  decl &= codegenType(fexpr.defn.ret)
  decl &= " "
  decl &= codegenMangling(fexpr.defn.name.symbol, fexpr.defn.generics.mapIt(it.symbol), fexpr.defn.args.mapIt(it[1].symbol))
  decl &= "("
  for arg in ctx.codegenArgs(decl, fexpr.defn.args):
    if arg[1].symbol.kind == symbolRef:
      decl &= codegenType(arg[1].symbol.types[0])
      decl &= "*"
    else:
      decl &= codegenType(arg[1])
    decl &= " "
    decl &= codegenSymbol(arg[0])
  decl &= ")"
  src.fndecls &= decl.exp
  src.fndecls &= ";\n"

  src &= decl
  src &= " {\n"
  if fexpr.defn.body.len != 0:
    if fexpr.defn.body[^1].typ.isVoidType:
      ctx.codegenBody(src, fexpr.defn.body)
    else:
      ctx.codegenBody(src, fexpr.defn.body, ret = "return ")
  src &= "}\n"

proc codegenDefn*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.internalPragma.header.isSome:
    src.addHeader(fexpr.internalPragma.header.get)
  if fexpr.internalPragma.importc.isNone:
    if fexpr.defn.generics.isSpecTypes:
      ctx.codegenDefnInstance(src, fexpr)
      
proc codegenDeftypeStruct*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src.typedecls &= "struct $# {\n" % codegenSymbol(fexpr.deftype.name.symbol)
  for field in fexpr.deftype.body:
    src.typedecls &= codegenType(field[1].symbol)
    src.typedecls &= " "
    src.typedecls &= $field[0]
    src.typedecls &= ";\n"
  src.typedecls &= "};\n"

proc codegenDeftypePattern*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.internalPragma.declc.isSome:
    src.typedecls &= codegenTypePattern(fexpr.internalPragma.declc.get, fexpr.deftype.name.symbol.types)
    src.typedecls &= "\n"
  
proc codegenDeftype*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.internalPragma.header.isSome:
    src.addHeader(fexpr.internalPragma.header.get)
  if fexpr.internalPragma.importc.isSome:
    if not fexpr.deftype.isGenerics:
      ctx.codegenDeftypePattern(src, fexpr)
  else:
    if not fexpr.deftype.isGenerics:
      ctx.codegenDeftypeStruct(src, fexpr)

proc codegenIf*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let tmpret = ctx.gentmpsym()

  if not fexpr.typ.isVoidType: # temporary return variable declaration.
    src.prev &= codegenType(fexpr.typ) & " " & tmpret & ";\n"
  let ret = if not fexpr.typ.isVoidType:
              tmpret & " = "
            else:
              nil

  let elifbranch = fexpr.internalIfExpr.elifbranch

  var ifcondsrc = initSrcExpr()
  var ifbodysrc = initSrcExpr()
  ctx.codegenFExpr(ifcondsrc, elifbranch[0].cond)
  ctx.codegenBody(ifbodysrc, elifbranch[0].body, ret)
  src.extendDecls(ifcondsrc)
  src.prev &= ifcondsrc.prev
  src.prev &= "if (" & ifcondsrc.exp & ") {\n"
  src.addPrev(ifbodysrc)
  src.prev &= "}"

  for branch in elifbranch[1..^1]:
    var elifcondsrc = initSrcExpr()
    var elifbodysrc = initSrcExpr()
    ctx.codegenFExpr(elifcondsrc, branch.cond)
    ctx.codegenBody(elifbodysrc, branch.body, ret)
    src.extendDecls(elifcondsrc)
    src.prev &= elifcondsrc.prev
    src.prev &= " else if (" & elifcondsrc.exp & ") {\n"
    src.addPrev(elifbodysrc)
    src.prev &= "}"

  var elsebodysrc = initSrcExpr()
  ctx.codegenBody(elsebodysrc, fexpr.internalIfExpr.elsebranch, ret)
  src.prev &= " else {\n"
  src.addPrev(elsebodysrc)
  src.prev &= "}"

  # return temporary variable.
  if not fexpr.typ.isVoidType:
    src &= tmpret

proc codegenWhile*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= "while ("
  ctx.codegenFExpr(src, fexpr.internalWhileExpr.cond[0])
  src &= ") {\n"
  ctx.codegenBody(src, fexpr.internalWhileExpr.body)
  src &= "}"
  
proc codegenVar*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= codegenType(fexpr[2])
  src &= " "
  src &= codegenSymbol(fexpr[1])
  
proc codegenDef*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= codegenType(fexpr.internalDefExpr.value.typ)
  src &= " "
  src &= codegenSymbol(fexpr.internalDefExpr.name)
  src &= " = "
  ctx.codegenFExpr(src, fexpr.internalDefExpr.value)

proc codegenDefDecl*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let t = codegenType(fexpr.internalDefExpr.value.typ)
  let n = codegenSymbol(fexpr.internalDefExpr.name)
  src.vardecls &= "extern $# $#;" % [t, n]
  src &= "$# $#;" % [t, n]
proc codegenDefValue*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= codegenSymbol(fexpr.internalDefExpr.name)
  src &= " = "
  ctx.codegenFExpr(src, fexpr.internalDefExpr.value)

proc codegenSet*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let dsttyp = fexpr.internalSetExpr.dst.typ
  if dsttyp.kind == symbolRef:
    src &= "*"
  ctx.codegenFExpr(src, fexpr.internalSetExpr.dst)
  src &= " = "
  ctx.codegenFExpr(src, fexpr.internalSetExpr.value)

proc codegenFieldAccess*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  ctx.codegenFExpr(src, fexpr.internalFieldAccessExpr.value)
  if fexpr.internalFieldAccessExpr.value.typ.kind == symbolRef:
    src &= "->"
  else:
    src &= "."
  ctx.codegenFExpr(src, fexpr.internalFieldAccessExpr.fieldname)

proc codegenInit*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= "(" & codegenType(fexpr.initexpr.typ) & "){"
  for arg in ctx.codegenArgs(src, fexpr.initexpr.body):
    ctx.codegenFExpr(src, arg)
  src &= "}"

proc codegenImport*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  discard

proc codegenInternal*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr, topcodegen: bool) =
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
  of internalIf:
    if not topcodegen:
      ctx.codegenIf(src, fexpr)
  of internalWhile:
    if not topcodegen:
      ctx.codegenWhile(src, fexpr)
  of internalVar:
    if not topcodegen:
      ctx.codegenVar(src, fexpr)
  of internalDef:
    if topcodegen and fexpr.isToplevel:
      ctx.codegenDefDecl(src, fexpr)
    elif fexpr.isToplevel:
      ctx.codegenDefValue(src, fexpr)
    else:
      ctx.codegenDef(src, fexpr)
  of internalTrack:
    discard
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
    if topcodegen:
      ctx.codegenImport(src, fexpr)

proc codegenPatternArgs*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr, pattern: var string) =
  for i, arg in fexpr:
    var comp = initSrcExpr()
    ctx.codegenFExpr(comp, arg)
    src.extendDecls(comp)
    src.prev &= comp.prev
    pattern = pattern.replace("$" & $(i+1), comp.exp)

proc codegenPatternCCall*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr, pattern: string, fn: FExpr) =
  var pattern = pattern
  if fexpr.isGenericsFuncCall:
    for i, g in fexpr[1]:
      pattern = pattern.replace("#" & $(i+1), codegenType(g))
    ctx.codegenPatternArgs(src, fexpr[2], pattern)
  else:
    if fexpr[0].symbol.kind == symbolInfix:
      ctx.codegenPatternArgs(src, fexpr[1..^1], pattern)
    else:
      ctx.codegenPatternArgs(src, fexpr[1], pattern)
  src &= pattern

proc codegenCCall*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let fn = fexpr[0].symbol.fexpr
  let fname = fn.internalPragma.importc.get

  if fn.internalPragma.declc.isSome:
    var psrc = initSrcExpr()
    ctx.codegenPatternCCall(psrc, fexpr, fn.internalPragma.declc.get, fn)
    src.fndecls &= psrc.exp
  
  if fn.internalPragma.infixc:
    if fexpr.len == 3:
      src &= "("
      ctx.codegenFExpr(src, fexpr[1])
      src &= " "
      src &= fname
      src &= " "
      ctx.codegenFExpr(src, fexpr[2])
      src &= ")"
    elif fexpr[1].len == 2:
      src &= "("
      ctx.codegenFExpr(src, fexpr[1][0])
      src &= " "
      src &= fname
      src &= " "
      ctx.codegenFExpr(src, fexpr[1][1])
      src &= ")"
    else:
      fexpr.error("$# is not infix expression." % $fexpr)
  elif fn.internalPragma.pattern.isSome:
    ctx.codegenPatternCCall(src, fexpr, fn.internalPragma.pattern.get, fn)
  else:
    src &= fname
    src &= "("
    for arg in ctx.codegenArgs(src, fexpr[1]):
      ctx.codegenFExpr(src, arg)
    src &= ")"
    
proc getCallGenerics(fexpr: FExpr): seq[Symbol] =
  fexpr[0].symbol.fexpr.defn.generics.mapIt(it.symbol)
proc getCallTypes(fexpr: FExpr): seq[Symbol] =
  fexpr[0].symbol.fexpr.defn.args.mapIt(it[1].symbol)

proc codegenCallArgs*(ctx: CCodegenContext, src: var SrcExpr, arg: FExpr, fnargtype: Symbol) =
  if arg.typ.kind == symbolVar and fnargtype.kind == symbolRef:
    src &= "&"
    ctx.codegenFExpr(src, arg)
  elif arg.typ.kind == symbolRef and fnargtype.kind != symbolRef:
    src &= "*"
    ctx.codegenFExpr(src, arg)
  else:
    ctx.codegenFExpr(src, arg)
  
proc codegenCall*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.len == 0:
    return
  
  if fexpr[0].kind != fexprSymbol:
    fexpr[0].error("$# isn't symbol." % $fexpr[0])
  let fn = fexpr[0].symbol.fexpr
  if fn.hasinternalPragma and fn.internalPragma.importc.isSome:
    ctx.codegenCCall(src, fexpr)
  else: # normal call
    if fexpr.isGenericsFuncCall:
      src &= codegenMangling(fexpr[0].symbol, fexpr.getCallGenerics(), fexpr.getCallTypes())
      src &= "("
      for i, arg in ctx.codegenArgsWithIndex(src, fexpr[2]):
        let fnargtype = fexpr[0].symbol.fexpr.defn.args[i][1].symbol
        ctx.codegenCallArgs(src, arg, fnargtype)
      src &= ")"
    elif fexpr.len == 3 and fexpr[0].symbol.kind == symbolInfix: # infix call
      src &= codegenMangling(fexpr[0].symbol, fexpr.getCallGenerics(), fexpr.getCallTypes()) # FIXME: support generics
      src &= "("
      ctx.codegenFExpr(src, fexpr[1])
      src &= ", "
      ctx.codegenFExpr(src, fexpr[2])
      src &= ")"
    else:
      src &= codegenMangling(fexpr[0].symbol, fexpr.getCallGenerics(), fexpr.getCallTypes())
      src &= "("
      for i, arg in ctx.codegenArgsWithIndex(src, fexpr[1]):
        let fnargtype = fexpr[0].symbol.fexpr.defn.args[i][1].symbol
        ctx.codegenCallArgs(src, arg, fnargtype)
      src &= ")"

proc codegenFExpr*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  case fexpr.kind
  of fexprIdent:
    if fexpr.resolve.isNil:
      src &= $fexpr
    elif fexpr.resolve.symbol.kind == symbolVar:
      src &= codegenSymbol(fexpr.resolve)
    else:
      src &= $fexpr
  of fexprSymbol:
    if not fexpr.symbol.fexpr.hasinternalMark:
      src &= codegenSymbol(fexpr)
  of fexprIntLit:
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
      ctx.codegenBody(blocksrc, toSeq(fexpr.items), ret = codegenType(fexpr[^1].typ) & " " & tmp & " = ")
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
      if son.isToplevel:
        continue
      son.isToplevel = true
      ctx.codegenToplevel(src, son)

proc codegenModule*(ctx: CCodegenContext, name: Name, scope: Scope) =
  var modsrc = initSrcExpr()
  for f in scope.toplevels:
    ctx.codegenToplevel(modsrc, f)

  modsrc &= "\n"
  modsrc &= "void __flori_$#_init() {\n" % replace($name, ".", "_")
  ctx.codegenBody(modsrc, scope.toplevels)
  modsrc &= "}\n"

  ctx.modulesrcs[name] = modsrc

proc codegen*(ctx: CCodegenContext, sem: SemanticContext) =
  for name, module in sem.modules:
    ctx.codegenModule(name, module)

proc cfilenames*(ctx: CCodegenContext, dir: string): seq[string] =
  result = @[]
  for si, modsrc in ctx.modulesrcs:
    result.add(dir / $si & ".c")
  result.add(dir / "main.c")

proc writeModules*(ctx: CCodegenContext, dir: string) =
  if not existsDir(dir):
    createDir(dir)
  for si, modsrc in ctx.modulesrcs:
    writeFile(dir / $si & ".c", modsrc.getSrc)
  writeFile(dir / "main.c", ctx.generateFloriMain() & ctx.generateCMain())
  writeFile(dir / "flori_decls.h", ctx.generateFloriDecls())
