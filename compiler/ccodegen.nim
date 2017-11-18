
import types
import fexpr
import scope
import semantic

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
  ($fexpr).replace(".", "_")

proc codegenBody*(ctx: CCodegenContext, src: var SrcExpr, body: FExpr, ret: string = nil) =
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

proc codegenFn*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.internalPragma.header.isSome:
    src.addHeader(fexpr.internalPragma.header.get)
  if not fexpr.internalPragma.importc:
    let fn = fexpr.internalFnExpr
    src &= "\n"
    src &= codegenSymbol(fn.ret)
    src &= " "
    src &= codegenSymbol(fn.name)
    src &= "("
    if fn.args.len >= 1:
      let (n, t) = fn.args[0]
      src &= codegenSymbol(t)
      src &= " "
      src &= $n
    for arg in fn.args[1..^1]:
      let (n, _) = arg
      src &= ", "
      src &= $n.typ.get
      src &= " "
      src &= $n
    src &= ") {\n"
    if fn.body.get[^1].typ.get.isVoidType:
      ctx.codegenBody(src, fn.body.get)
    else:
      ctx.codegenBody(src, fn.body.get, ret = "return ")
    src &= "}\n"

proc codegenStruct*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  if fexpr.internalPragma.header.isSome:
    src.addHeader(fexpr.internalPragma.header.get)
  if fexpr.internalPragma.importc:
    let struct = fexpr.internalStructExpr
    let fname = if fexpr.internalPragma.importname.isSome:
                  fexpr.internalPragma.importname.get
                else:
                  codegenSymbol(struct.name)
    src &= "typedef "
    src &= fname
    src &= " "
    ctx.codegenFExpr(src, struct.name)
    src &= ";\n"
  else:
    fexpr.error("unsupported struct in currently.")

proc codegenIfBranch*(ctx: CCodegenContext, src: var SrcExpr, fexpr: (FExpr, FExpr), ret: string) =
  let (tcond, tbody) = fexpr
  var condsrc = initSrcExpr()
  var bodysrc = initSrcExpr()
  ctx.codegenFExpr(condsrc, tcond[0])
  ctx.codegenBody(bodysrc, tbody, ret)
  src.prev &= "("
  src.prev &= condsrc.prev
  src.prev &= condsrc.exp
  src.prev &= ") {\n"
  src.prev &= bodysrc.prev
  src.prev &= bodysrc.exp
  src.prev &= "}"

proc codegenIf*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let tmpret = ctx.gentmpsym()
  let ret = if fexpr.typ.get.isVoidType:
              nil
            else:
              tmpret & " = "

  if not fexpr.typ.get.isVoidType:
    src.prev &= codegenSymbol(fexpr.typ.get) & " " & tmpret & ";\n"

  src.prev &= "if "
  ctx.codegenIfBranch(src, fexpr.internalIfExpr.tbody, ret)
  for e in fexpr.internalIfExpr.ebody:
    src.prev &= " else if "
    ctx.codegenIfBranch(src, e, ret)
  if fexpr.internalIfExpr.fbody.isSome:
    src.prev &= " else {\n"
    var bodysrc = initSrcExpr()
    ctx.codegenBody(bodysrc, fexpr.internalIfExpr.fbody.get, ret)
    src.prev &= bodysrc.prev
    src.prev &= bodysrc.exp
    src.prev &= "}\n"
  if not fexpr.typ.get.isVoidType:
    src &= tmpret

proc codegenInternal*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr, toplevel: bool) =
  case fexpr.internalMark
  of internalFn:
    if toplevel:
      ctx.codegenFn(src, fexpr)
  of internalStruct:
    if toplevel:
      ctx.codegenStruct(src, fexpr)
  of internalIf:
    if not toplevel:
      ctx.codegenIf(src, fexpr)

proc getRawName*(fexpr: FExpr): string =
  if fexpr.kind == fexprQuote:
    return $fexpr.quoted.symbol.name
  else:
    return $fexpr.symbol.name

proc codegenCCall*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let fn = fexpr[0].symbol.fexpr
  let fname = if fn.internalPragma.importname.isSome:
                fn.internalPragma.importname.get
              else:
                getRawName(fn[1])
  if fn.internalPragma.infix:
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
  if fn.hasinternalPragma and fn.internalPragma.importc:
    ctx.codegenCCall(src, fexpr)
  else: # normal call
    src &= codegenSymbol(fexpr[0])
    src &= "("
    if fexpr.len > 1:
      ctx.codegenFExpr(src, fexpr[1])
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
  of fexprSeq:
    if fexpr.hasinternalMark:
      ctx.codegenInternal(src, fexpr ,toplevel = false)
    else:
      fexpr.error("undeclared $# macro. (unsupported macro in currently)" % $fexpr[0])
  of fexprArray..fexprBlock:
    fexpr.error("unsupported $# in C Codegen." % $fexpr.kind)
  of fexprCall:
    ctx.codegenCall(src, fexpr)
  else:
    fexpr.error("$# is unsupported expression in eval." % $fexpr.kind)

proc codegenToplevel*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  case fexpr.kind
  of fexprSeq:
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
  ctx.codegenBody(modsrc, fblock(internalSpan, scope.toplevels))
  modsrc &= "}\n"

  ctx.modulesrcs[name] = modsrc

proc codegen*(ctx: CCodegenContext, sem: SemanticContext) =
  for name, module in sem.modules:
    ctx.codegenModule(name, module)

proc filenames*(ctx: CCodegenContext, dir: string): seq[string] =
  result = @[]
  for si, modsrc in ctx.modulesrcs:
    result.add(dir / $si & ".c")
  result.add(dir / "main.c")

proc write*(ctx: CCodegenContext, dir: string) =
  if not existsDir(dir):
    createDir(dir)
  for si, modsrc in ctx.modulesrcs:
    writeFile(dir / $si & ".c", modsrc.getSrc)
  writeFile(dir / "main.c", ctx.generateMain())
