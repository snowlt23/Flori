
import types, fexpr
import scope, semantic, internal

import tables
import options
import strutils, sequtils
import os

type
  SrcExpr* = object
    headers*: Table[string, bool]
    prev*: string
    exp*: string
  CCodegenContext* = ref object
    modulesrcs*: Table[Name, SrcExpr]
    tmpcount*: int
  
proc codegenFExpr*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr)

proc initSrcExpr*(): SrcExpr =
  result.headers = initTable[string, bool]()
  result.prev = ""
  result.exp = ""
proc `&=`*(src: var SrcExpr, s: string) = src.exp &= s
proc getSrc*(src: SrcExpr): string =
  toSeq(src.headers.keys).mapIt("#include \"$#\"" % it).join("\n") & "\n\n" & src.exp
proc addHeader*(src: var SrcExpr, s: string) =
  src.headers[s] = true

proc newCCodegenContext*(): CCodegenContext =
  CCodegenContext(modulesrcs: initTable[Name, SrcExpr](), tmpcount: 0)
proc gentmpsym*(ctx: CCodegenContext): string =
  result = "__floritmp" & $ctx.tmpcount
  ctx.tmpcount.inc

proc generateInitDecls*(ctx: CCodegenContext): string =
  result = ""
  for si, module in ctx.modulesrcs:
    result &= "void " & $si & "_init();\n"
proc generateInits*(ctx: CCodegenContext): string =
  result = ""
  for si, module in ctx.modulesrcs:
    result &= $si & "_init();\n"
proc generateMain*(ctx: CCodegenContext): string =
  result = ""
  result &= ctx.generateInitDecls()
  result &= "int main() {\n"
  result &= ctx.generateInits()
  result &= "}\n"

proc codegenSymbol*(sym: Symbol): string =
  ($sym).replace(".", "_")
proc codegenSymbol*(fexpr: FExpr): string =
  if fexpr.kind != fexprSymbol:
    fexpr.error("$# isn't symbol." % $fexpr)
  return codegenSymbol(fexpr.symbol)

proc codegenBody*(ctx: CCodegenContext, src: var SrcExpr, body: seq[FExpr], ret: string = nil) =
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
  src &= newsrc.exp & ";\n"
proc codegenBody*(ctx: CCodegenContext, src: var SrcExpr, body: FExpr, ret: string = nil) =
  ctx.codegenBody(src, toSeq(body.items), ret)

proc codegenDefn*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.internalPragma.header.isSome:
    src.addHeader(fexpr.internalPragma.header.get)
  if fexpr.internalPragma.importc.isNone:
    let fn = fexpr.internalDefnExpr
    src &= "\n"
    src &= codegenSymbol(fn.ret)
    src &= " "
    src &= codegenSymbol(fn.name)
    src &= "("
    if fn.args.len >= 1:
      let t = fn.args[0]
      let n = fn.args[1]
      src &= codegenSymbol(t)
      src &= " "
      src &= $n
    for i in countup(2, fn.args.len-1, 2):
      let t = fn.args[i]
      let n = fn.args[i+1]
      src &= ", "
      src &= codegenSymbol(t)
      src &= " "
      src &= $n
    src &= ") {\n"
    if fn.body[^1].typ.get.isVoidType:
      ctx.codegenBody(src, fn.body)
    else:
      ctx.codegenBody(src, fn.body, ret = "return ")
    src &= "}\n"

proc codegenDeftype*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.internalPragma.header.isSome:
    src.addHeader(fexpr.internalPragma.header.get)
  if fexpr.internalPragma.importc.isSome:
    let deftype = fexpr.internalDeftypeExpr
    let fname = fexpr.internalPragma.importc.get
    src &= "typedef "
    src &= fname
    src &= " "
    src &= codegenSymbol(deftype.name)
    src &= ";\n"
  else:
    fexpr.error("unsupported struct in currently.")

proc codegenIf*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let tmpret = ctx.gentmpsym()

  if not fexpr.typ.get.isVoidType: # temporary return variable declaration.
    src.prev &= codegenSymbol(fexpr.typ.get) & " " & tmpret & ";\n"

  var condsrc = initSrcExpr()
  var tbodysrc = initSrcExpr()
  var fbodysrc = initSrcExpr()
  ctx.codegenFExpr(condsrc, fexpr.internalIfExpr.cond)
  ctx.codegenFExpr(tbodysrc, fexpr.internalIfExpr.tbody)
  ctx.codegenFExpr(fbodysrc, fexpr.internalIfExpr.fbody)

  src.prev &= condsrc.prev
  src.prev &= "if ("
  src.prev &= condsrc.exp
  src.prev &= ") {\n"

  # tbody
  src.prev &= tbodysrc.prev
  if not fexpr.typ.get.isVoidType:
    src.prev &= tmpret & " = " & tbodysrc.exp & ";\n"
  else:
    src.prev &= tbodysrc.exp & ";\n"

  # fbody
  src.prev &= "} else {\n"
  src.prev &= fbodysrc.prev
  if not fexpr.typ.get.isVoidType:
    src.prev &= tmpret & " = " & fbodysrc.exp & ";\n"
  else:
    src.prev &= fbodysrc.exp & ";\n"
  src.prev &= "}"

  # return temporary variable.
  if not fexpr.typ.get.isVoidType:
    src &= tmpret

proc codegenWhile*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= "while ("
  ctx.codegenFExpr(src, fexpr.internalWhileExpr.cond)
  src &= ") {\n"
  ctx.codegenBody(src, fexpr.internalWhileExpr.body)
  src &= "}"

proc codegenDef*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= codegenSymbol(fexpr.internalDefExpr.value.typ.get)
  src &= " "
  src &= codegenSymbol(fexpr.internalDefExpr.name)
  src &= " = "
  ctx.codegenFExpr(src, fexpr.internalDefExpr.value)

proc codegenDefDecl*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= codegenSymbol(fexpr.internalDefExpr.value.typ.get)
  src &= " "
  src &= codegenSymbol(fexpr.internalDefExpr.name)
  src &= ";"
proc codegenDefValue*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  src &= codegenSymbol(fexpr.internalDefExpr.name)
  src &= " = "
  ctx.codegenFExpr(src, fexpr.internalDefExpr.value)

proc codegenInternal*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr, toplevel: bool) =
  case fexpr.internalMark
  of internalDefn:
    if toplevel:
      ctx.codegenDefn(src, fexpr)
  of internalDeftype:
    if toplevel:
      ctx.codegenDeftype(src, fexpr)
  of internalIf:
    if not toplevel:
      ctx.codegenIf(src, fexpr)
  of internalWhile:
    if not toplevel:
      ctx.codegenWhile(src, fexpr)
  of internalDef:
    if toplevel:
      ctx.codegenDefDecl(src, fexpr)
      fexpr.internalToplevel = true
    elif fexpr.hasinternalToplevel and fexpr.internalToplevel:
      ctx.codegenDefValue(src, fexpr)
    else:
      ctx.codegenDef(src, fexpr)

# proc getRawName*(fexpr: FExpr): string =
#   if fexpr.kind == fexprQuote:
#     return $fexpr.quoted.symbol.name
#   else:
#     return $fexpr.symbol.name

proc codegenCCall*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let fn = fexpr[0].symbol.fexpr
  let fname = fn.internalPragma.importc.get
  if fn.internalPragma.infixc:
    if fexpr.len != 3:
      fexpr.error("$# is not infix expression." % $fexpr)
    src &= "("
    ctx.codegenFExpr(src, fexpr[1])
    src &= " "
    src &= fname
    src &= " "
    ctx.codegenFExpr(src, fexpr[2])
    src &= ")"
  else:
    src &= fname
    src &= "("
    if fexpr.len > 1:
      ctx.codegenFExpr(src, fexpr[1])
    for arg in fexpr[2..^1]:
      src &= ", "
      ctx.codegenFExpr(src, arg)
    src &= ")"

proc codegenCall*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr[0].kind != fexprSymbol:
    fexpr[0].error("$# isn't symbol." % $fexpr[0])
  let fn = fexpr[0].symbol.fexpr
  if fn.hasinternalPragma and fn.internalPragma.importc.isSome:
    ctx.codegenCCall(src, fexpr)
  else: # normal call
    src &= codegenSymbol(fexpr[0])
    src &= "("
    if fexpr.len > 1:
      ctx.codegenFExpr(src, fexpr[1])
    if fexpr.len > 2:
      for arg in fexpr[2..^1]:
        src &= ", "
        ctx.codegenFExpr(src, arg)
    src &= ")"

proc codegenFExpr*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  case fexpr.kind
  of fexprIdent:
    src &= $fexpr
  of fexprSymbol:
    src &= codegenSymbol(fexpr)
  of fexprIntLit:
    src &= $fexpr
  of fexprStrLit:
    src &= $fexpr
  of fexprList:
    if fexpr.hasinternalMark:
      ctx.codegenInternal(src, fexpr, toplevel = false)
    else:
      ctx.codegenCall(src, fexpr)
  of fexprArray..fexprMap:
    fexpr.error("unsupported $# in C Codegen." % $fexpr.kind)
  else:
    fexpr.error("$# is unsupported expression in eval." % $fexpr.kind)

proc codegenToplevel*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  case fexpr.kind
  of fexprList:
    if fexpr.hasinternalMark:
      ctx.codegenInternal(src, fexpr, toplevel = true)
  else:
    discard

proc codegenModule*(ctx: CCodegenContext, name: Name, scope: Scope) =
  var modsrc = initSrcExpr()
  for f in scope.toplevels:
    ctx.codegenToplevel(modsrc, f)

  modsrc &= "\n"
  modsrc &= "void $#_init() {\n" % $name
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
  writeFile(dir / "main.c", ctx.generateMain())
