
import fexpr_core

import tables
import options
import strutils, sequtils
import os
import algorithm
import base64

type
  SrcExpr* = object
    prev*: string
    exp*: string
    line*: int
    linepos*: int
  FExprmap* = object
    fexpr*: FExpr
    sourceid*: int
    jsline*: int
    jslinepos*: int
  Sourcemap* = object
    sources*: seq[tuple[name: string, src: string]]
    fexprs*: seq[FExprmap]
  JSCodegenContext* = ref object
    tmpcount*: int
    sourcemap*: Sourcemap
    
proc codegenFExpr*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr)
proc codegenCallArg*(ctx: JSCodegenContext, src: var SrcExpr, arg: FExpr, fnargtype: Symbol)

#
# Base64 VLQ
#

const basechars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="

proc itoChar*(x: int): char =
  for i, c in basechars:
    if i == x:
      return c
  assert(false)

proc encodeInteger*(n: int): string =
  result = ""
  var num = n

  if (num < 0):
    num = (-num shl 1) or 1
  else:
    num = num shl 1
    
  var clamped = num and 31
  num = num shr 5
  if (num > 0):
    clamped = clamped or 32
  result &= itoChar(clamped)

  while (num > 0):
    var clamped = num and 31
    num = num shr 5
    if (num > 0):
      clamped = clamped or 32
    result &= itoChar(clamped)

proc encodeBase64VLQ*(xs: openArray[int]): string =
  result = ""
  for x in xs:
    result &= encodeInteger(x)
    
#
# codegen
#

proc getSourcename*(modscope: Scope): string =
  return $modscope.name & ".flori"

proc getSourceid*(ctx: JSCodegenContext, modscope: Scope): int =
  for i, source in ctx.sourcemap.sources:
    if source.name == modscope.getSourcename:
      return i
  assert(false)

proc addFExprmap*(ctx: JSCodegenContext, src: SrcExpr, fexpr: FExpr) =
  ctx.sourcemap.fexprs.add(FExprmap(fexpr: fexpr, sourceid: ctx.getSourceid(fexpr.internalScope), jsline: src.line, jslinepos: src.linepos))

proc initSourcemap*(semctx: SemanticContext): Sourcemap =
  result.sources = @[]
  result.fexprs = @[]
  for modscope in semctx.modules.values:
    if existsFile(modscope.path):
      result.sources.add((modscope.getSourcename, readFile(modscope.path)))
    else:
      result.sources.add((modscope.getSourcename, ""))

proc initSrcExpr*(): SrcExpr =
  result.prev = ""
  result.exp = ""
  result.line = 1
  result.linepos = 1
proc `&=`*(src: var SrcExpr, s: string) =
  src.exp &= s
  let lf = char(10)
  for c in s:
    if c == lf:
      src.line += 1
      src.linepos = 1
    else:
      src.linepos += 1
proc `&=`*(src: var SrcExpr, s: SrcExpr) =
  src.exp &= s.prev
  src.exp &= s.exp
  let lf = char(10)
  for c in s.prev:
    if c == lf:
      src.line += 1
      src.linepos = 1
    else:
      src.linepos += 1
  for c in s.exp:
    if c == lf:
      src.line += 1
      src.linepos = 1
    else:
      src.linepos += 1
    
proc addPrev*(src: var SrcExpr, s: string) = src.prev &= s
proc addPrev*(src: var SrcExpr, s: SrcExpr) =
  src.prev &= s.prev
  src.prev &= s.exp

proc newJSCodegenContext*(semctx: SemanticContext): JSCodegenContext =
  return JSCodegenContext(tmpcount: 0, sourcemap: initSourcemap(semctx))
proc gentmpsym*(ctx: JSCodegenContext): string =
  result = "__floritmp" & $ctx.tmpcount
  ctx.tmpcount.inc

proc replaceSpecialSymbols*(s: string): string =
  s.replace(".", "_").replace("+", "plus").replace("-", "minus").replace("*", "asterisk").replace("/", "slash").replace("!", "excl").replace("=", "eq").replace("%", "per").replace("&", "and")

proc codegenSymbol*(sym: Symbol): string

proc codegenSymbol*(sym: Symbol): string =
  result = ""
  if sym.kind == symbolTypeGenerics and sym.types.len != 0:
    result &= $sym.scope.name & "_" & $sym.name & "_" & sym.types.mapIt(codegenSymbol(it)).join("_")
  elif sym.kind == symbolVar:
    result &= codegenSymbol(sym.wrapped)
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
proc codegenTypeImportjs*(sym: Symbol): string =
  if sym.fexpr.internalPragma.patternjs.isSome:
    codegenTypePattern(sym.fexpr.internalPragma.patternjs.get, sym.types)
  else:
    sym.fexpr.internalPragma.importjs.get

proc codegenType*(sym: Symbol): string =
  if sym.kind == symbolVar:
    return codegenType(sym.wrapped)
  elif sym.kind == symbolRef:
    return codegenType(sym.wrapped) & "*"
  
  if sym.fexpr.hasInternalPragma and sym.fexpr.internalPragma.importjs.isSome:
    return codegenTypeImportjs(sym)
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

iterator codegenArgs*(ctx: JSCodegenContext, src: var SrcExpr, args: FExpr): FExpr =
  if args.len >= 1:
    yield(args[0])
  if args.len >= 2:
    for i in 1..<args.len:
      src &= ", "
      yield(args[i])
iterator codegenArgsWithIndex*(ctx: JSCodegenContext, src: var SrcExpr, args: FExpr): (int, FExpr) =
  if args.len >= 1:
    yield(0, args[0])
  if args.len >= 2:
    for i in 1..<args.len:
      src &= ", "
      yield(i, args[i])

proc codegenBody*(ctx: JSCodegenContext, src: var SrcExpr, body: seq[FExpr], ret: string = nil) =
  if body.len == 0:
    return
  if body.len >= 2:
    for b in body[0..^2]:
      var newsrc = initSrcExpr()
      ctx.codegenFExpr(newsrc, b)
      src &= newsrc.prev
      if newsrc.exp != "":
        src &= newsrc.exp & ";\n"
  var newsrc = initSrcExpr()
  ctx.codegenFExpr(newsrc, body[^1])
  src &= newsrc.prev
  if ret != nil:
    src &= ret
  if newsrc.exp != "":
    src &= newsrc.exp & ";\n"
proc codegenBody*(ctx: JSCodegenContext, src: var SrcExpr, body: FExpr, ret: string = nil) =
  ctx.codegenBody(src, toSeq(body.items), ret)

proc codegenDefnInstance*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.internalPragma.inline:
    return
  
  var decl = initSrcExpr()
  src &= "function "
  if fexpr.internalPragma.exportjs.isSome:
    src &= fexpr.internalPragma.exportjs.get
  else:
    src &= codegenMangling(fexpr.defn.name.symbol, fexpr.defn.generics.mapIt(it.symbol), fexpr.defn.args.mapIt(it[1].symbol))
  src &= "("
  for arg in ctx.codegenArgs(decl, fexpr.defn.args):
    src &= codegenSymbol(arg[0])
  src &= ")"
  src &= " {\n"
  if fexpr.defn.body.len != 0:
    if fexpr.defn.body[^1].typ.isVoidType:
      ctx.codegenBody(src, fexpr.defn.body)
    else:
      ctx.codegenBody(src, fexpr.defn.body, ret = "return ")
  src &= "}\n"

proc codegenDefn*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.internalPragma.importjs.isNone:
    if fexpr.defn.generics.isSpecTypes:
      ctx.codegenDefnInstance(src, fexpr)

proc codegenDeftype*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  discard

proc codegenIf*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let tmpret = ctx.gentmpsym()

  if not fexpr.typ.isVoidType: # temporary return variable declaration.
    src.prev &= "var " & tmpret & ";\n"
  let ret = if not fexpr.typ.isVoidType:
              tmpret & " = "
            else:
              nil

  let elifbranch = fexpr.internalIfExpr.elifbranch

  var ifcondsrc = initSrcExpr()
  var ifbodysrc = initSrcExpr()
  ctx.codegenFExpr(ifcondsrc, elifbranch[0].cond)
  ctx.codegenBody(ifbodysrc, elifbranch[0].body, ret)
  src.prev &= ifcondsrc.prev
  src.prev &= "if (" & ifcondsrc.exp & ") {\n"
  src.addPrev(ifbodysrc)
  src.prev &= "}"

  for branch in elifbranch[1..^1]:
    var elifcondsrc = initSrcExpr()
    var elifbodysrc = initSrcExpr()
    ctx.codegenFExpr(elifcondsrc, branch.cond)
    ctx.codegenBody(elifbodysrc, branch.body, ret)
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

proc codegenWhile*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= "while ("
  ctx.codegenFExpr(src, fexpr.internalWhileExpr.cond[0])
  src &= ") {\n"
  ctx.codegenBody(src, fexpr.internalWhileExpr.body)
  src &= "}"
  
proc codegenVar*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= "var " & codegenSymbol(fexpr[1])

proc codegenConst*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= "var "
  src &= codegenSymbol(fexpr[1][1])
  src &= " "
  ctx.codegenFExpr(src, fexpr[2])
  src &= "\n"
  
proc codegenDef*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= "var "
  src &= codegenSymbol(fexpr.internalDefExpr.name)
  src &= " = "
  ctx.codegenFExpr(src, fexpr.internalDefExpr.value)

proc codegenDefDecl*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= "var "
  src &= codegenSymbol(fexpr.internalDefExpr.name)
  src &= ";\n"
proc codegenDefValue*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= codegenSymbol(fexpr.internalDefExpr.name)
  src &= " = "
  ctx.codegenFExpr(src, fexpr.internalDefExpr.value)

proc codegenSet*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let dsttyp = fexpr.internalSetExpr.dst.typ
  ctx.codegenFExpr(src, fexpr.internalSetExpr.dst)
  src &= " = "
  ctx.codegenFExpr(src, fexpr.internalSetExpr.value)

proc codegenFieldAccess*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  ctx.codegenFExpr(src, fexpr.internalFieldAccessExpr.value)
  src &= "."
  ctx.codegenFExpr(src, fexpr.internalFieldAccessExpr.fieldname)

proc codegenInit*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= "{"
  for i, b in ctx.codegenArgsWithIndex(src, fexpr.initexpr.body):
    src &= "\"$#\": " % $fexpr.initexpr.typ.deftype.body[i][0].symbol.name
    ctx.codegenFExpr(src, b)

proc codegenCEmit*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  discard

proc codegenBlock*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= "{\n"
  ctx.codegenFExpr(src, fexpr[1])
  src &= "\n}\n"

proc codegenInternal*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr, topcodegen: bool) =
  case fexpr.internalMark
  of internalDefn:
    if topcodegen:
      ctx.codegenDefn(src, fexpr)
  of internalMacro:
    discard
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
    discard
  of internalExport:
    discard
  of internalCEmit:
    if topcodegen and fexpr.isToplevel:
      ctx.codegenCEmit(src, fexpr)
    elif not topcodegen and not fexpr.isToplevel:
      ctx.codegenCEmit(src, fexpr)
  of internalBlock:
    if not topcodegen:
      ctx.codegenBlock(src, fexpr)

proc codegenCallArg*(ctx: JSCodegenContext, src: var SrcExpr, arg: FExpr, fnargtype: Symbol) =
  ctx.codegenFExpr(src, arg)

proc codegenPatArg*(ctx: JSCodegenContext, src: var SrcExpr, arg: FExpr, fnargtype: Symbol) =
  ctx.codegenFExpr(src, arg)

proc codegenPatternArgs*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr, fnargtypes: seq[Symbol], ret: Symbol, pattern: var string) =
  pattern = pattern.replace("$#0", codegenType(ret))
  for i, arg in fexpr.sons.reversed():
    var comp = initSrcExpr()
    ctx.codegenPatArg(comp, arg, fnargtypes[fexpr.len-i-1])
    src.prev &= comp.prev
    pattern = pattern.replace("$#" & $(fexpr.len-i), codegenType(arg.typ))
    pattern = pattern.replace("$" & $(fexpr.len-i), comp.exp)

proc codegenPatternJSCall*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr, pattern: string, fn: FExpr) =
  var pattern = pattern
  var argtypes = fexpr[0].symbol.fexpr.defn.args.mapIt(it[1].symbol)
  if fexpr.isGenericsFuncCall:
    for i, g in fexpr[1]:
      pattern = pattern.replace("#" & $(i+1), codegenType(g))
    ctx.codegenPatternArgs(src, fexpr[2], argtypes, fexpr.typ, pattern)
  else:
    if fexpr[0].symbol.kind == symbolInfix:
      ctx.codegenPatternArgs(src, fexpr[1..^1], argtypes, fexpr.typ, pattern)
    else:
      ctx.codegenPatternArgs(src, fexpr[1], argtypes, fexpr.typ, pattern)
  src &= pattern

proc codegenJSCall*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let fn = fexpr[0].symbol.fexpr
  let fname = fn.internalPragma.importjs.get
  
  if fn.internalPragma.infixjs:
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
  elif fn.internalPragma.patternjs.isSome:
    ctx.codegenPatternJSCall(src, fexpr, fn.internalPragma.patternjs.get, fn)
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
  
proc codegenCall*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.len == 0:
    return
  
  if fexpr[0].kind != fexprSymbol:
    fexpr[0].error("$# isn't symbol." % $fexpr[0])
  let fn = fexpr[0].symbol.fexpr
  if fn.hasinternalPragma and fn.internalPragma.importjs.isSome:
    ctx.codegenJSCall(src, fexpr)
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
    else:
      src &= codegenMangling(fexpr[0].symbol, fexpr.getCallGenerics(), fexpr.getCallTypes())
      src &= "("
      for i, arg in ctx.codegenArgsWithIndex(src, fexpr[1]):
        let fnargtype = fexpr[0].symbol.fexpr.defn.args[i][1].symbol
        ctx.codegenCallArg(src, arg, fnargtype)
      src &= ")"

proc codegenFExpr*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  case fexpr.kind
  of fexprIdent:
    src &= $fexpr
  of fexprSymbol:
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
      ctx.codegenBody(blocksrc, toSeq(fexpr.items), ret = "var " & tmp & " = ")
      src.addPrev(blocksrc)
      src &= tmp
  of fexprSeq:
    if fexpr.hasinternalMark:
      ctx.codegenInternal(src, fexpr, topcodegen = false)
    else:
      ctx.codegenCall(src, fexpr)
  else:
    fexpr.error("$# is unsupported expression in eval." % $fexpr.kind)

proc codegenToplevel*(ctx: JSCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.hasinternalMark:
    if fexpr.internalMark in {internalDefn, internalDef}:
      ctx.addFExprmap(src, fexpr)
    ctx.codegenInternal(src, fexpr, topcodegen = true)
  elif fexpr.kind == fexprBlock:
    for son in fexpr:
      if son.isGenerated:
        continue
      son.isToplevel = true
      ctx.codegenToplevel(src, son)

proc generateSourcemap*(sourcemap: var Sourcemap, filename: string): string =
  result = ""
  result &= "{"
  result &= "\"version\": 3, "
  result &= "\"file\": \"$#\", " % filename
  result &= "\"sourceRoot\": \"\", "
  result &= "\"names\": [], "
  result &= "\"sources\": "
  result &= "[" & sourcemap.sources.mapIt("\"" & it.name & "\"").join(", ") & "], "
  result &= "\"mappings\": \""
  sourcemap.fexprs.sort(
    proc (x, y: FExprmap): int =
      if x.jsline == y.jsline:
        if x.sourceid == y.sourceid:
          cmp(x.fexpr.span.line, y.fexpr.span.line)
        else:
          cmp(x.sourceid, y.sourceid)
      else:
        cmp(x.jsline, y.jsline)
  )
  var curjsline = 1
  var prevmap = sourcemap.fexprs[0]
  var newline = true
  var isfirst = true
  for fmap in sourcemap.fexprs:
    while curjsline != fmap.jsline:
      result &= ";"
      newline = true
      curjsline += 1
    if not newline:
      result &= ","
    let jslinepos = if isfirst:
                      fmap.jslinepos - 1
                    else:
                      fmap.jslinepos - prevmap.jslinepos
    let sourceid = if isfirst:
                     fmap.sourceid
                   else:
                     fmap.sourceid - prevmap.sourceid
    let line = if isfirst:
                 fmap.fexpr.span.line - 1
               else:
                 fmap.fexpr.span.line - prevmap.fexpr.span.line
    let linepos = if isfirst:
                    fmap.fexpr.span.linepos - 1
                  else:
                    fmap.fexpr.span.linepos - prevmap.fexpr.span.linepos
    # echo fmap.fexpr
    # echo @[jslinepos, sourceid, line, linepos]
    result &= encodeBase64VLQ([jslinepos, sourceid, line, linepos])
    prevmap = fmap
    isfirst = false
    newline = false
  result &= ";\", "
  result &= "\"sourcesContent\": "
  result &= "[" & sourcemap.sources.mapIt("\"" & it.src.replace("\"", "\\\"").replace("\\n", "\\\\n").replace($0x0d.char).replace($0x0a.char, "\\n") & "\"").join(", ") & "]"
  result &= "}"
  # echo result
  result = encode(result, newline = "")

proc generateSourcemap*(ctx: JSCodegenContext, filename: string): string =
  return ctx.sourcemap.generateSourcemap(filename)

proc codegenSingle*(ctx: JSCodegenContext, sem: SemanticContext): string =
  var src = initSrcExpr()
  for f in sem.globaltoplevels:
    ctx.codegenToplevel(src, f)
  src &= "\n"
  src &= "function flori_main() {\n"
  ctx.codegenBody(src, sem.globaltoplevels)
  src &= "}\n"
  src &= "flori_main();\n"
  return src.exp
