
import types
import fexpr
import scope
import semantic
import tables
import options
import strutils, sequtils

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

proc codegenBody*(ctx: CCodegenContext, src: var SrcExpr, body: FExpr, ret: string = nil) =
  if body.len == 0:
    return
  if body.len >= 2:
    for b in body[0..^2]:
      var newsrc = initSrcExpr()
      ctx.codegenFExpr(newsrc, b)
      src &= newsrc.prev
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
    src &= $fn.ret.typ.get
    src &= " "
    src &= $fn.name
    src &= "("
    if fn.args.len >= 1:
      let (n, _) = fn.args[0]
      src &= $n.typ.get
      src &= " "
      src &= $n
    for arg in fn.args[1..^1]:
      let (n, _) = arg
      src &= ", "
      src &= $n.typ.get
      src &= " "
      src &= $n
    src &= ") {\n"
    ctx.codegenBody(src, fn.body.get)
    src &= "}\n"

proc codegenStruct*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  discard # TODO:

proc codegenIfBranch*(ctx: CCodegenContext, src: var SrcExpr, fexpr: (FExpr, FExpr), ret: string) =
  let (tcond, tbody) = fexpr
  var condsrc = initSrcExpr()
  var bodysrc = initSrcExpr()
  ctx.codegenFExpr(condsrc, tcond)
  ctx.codegenBody(bodysrc, tbody, ret)
  src.prev &= "("
  src.prev = $condsrc
  src.prev &= ") {\n"
  src.prev &= $bodysrc
  src.prev &= "}"

proc codegenIf*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let tmp = ctx.gentmpsym()
  let ret = if fexpr.typ.get.isVoidType:
              nil
            else:
              tmp & " = "
  src.prev &= "if "
  ctx.codegenIfBranch(src, fexpr.internalIfExpr.tbody, ret)
  for e in fexpr.internalIfExpr.ebody:
    src.prev &= " else if "
    ctx.codegenIfBranch(src, e, ret)
  if fexpr.internalIfExpr.fbody.isSome:
    src.prev &= " else {\n"
    var bodysrc = initSrcExpr()
    ctx.codegenBody(bodysrc, fexpr.internalIfExpr.fbody.get, ret)
    src.prev &= ""
    src.prev &= " else {\n"
  if not fexpr.typ.get.isVoidType:
    src &= tmp

proc codegenInternal*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  case fexpr.internalMark
  of internalFn:
    ctx.codegenFn(src, fexpr)
  of internalStruct:
    ctx.codegenStruct(src, fexpr)
  of internalIf:
    ctx.codegenIf(src, fexpr)

proc codegenCCall*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let fname = if fexpr.internalPragma.importname.isSome:
                fexpr.internalPragma.importname.get
              else:
                $fexpr[0]
  src &= fname
  src &= "("
  if fexpr.len > 1:
    ctx.codegenFExpr(src, fexpr[1])
  for arg in fexpr[2..^1]:
    src &= ", "
    ctx.codegenFExpr(src, arg)
  src &= ")"

proc codegenCall*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  let fn = fexpr[0]
  if fn.hasinternalPragma and fn.internalPragma.importc:
    ctx.codegenCCall(src, fexpr)
  else: # normal call
    src &= $fn
    src &= "("
    if fexpr.len > 1:
      ctx.codegenFExpr(src, fn[1])
    for arg in fexpr[2..^1]:
      src &= ", "
      ctx.codegenFExpr(src, arg)
    src &= ")"

proc codegenFExpr*(ctx: CCodegenContext, src: var SrcExpr, fexpr: FExpr) =
  case fexpr.kind
  of fexprIdent:
    src &= $fexpr
  of fexprSymbol:
    src &= $fexpr
    if fexpr.hasinternalMark: # TODO: symbol codegen support of type and importctype

  of fexprIntLit:
    src &= $fexpr
  of fexprStrLit:
    src &= $fexpr
  of fexprSeq:
    if fexpr.hasinternalMark:
      ctx.codegenInternal(src, fexpr)
    else:
      fexpr.error("undeclared $# macro. (unsupported macro in currently)" % $fexpr[0])
  of fexprArray..fexprBlock:
    fexpr.error("unsupported $# in C Codegen." % $fexpr.kind)
  of fexprCall:
    ctx.codegenCall(src, fexpr)
  else:
    fexpr.error("$# is unsupported expression in eval." % $fexpr.kind)
