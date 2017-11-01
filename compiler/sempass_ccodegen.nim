
import sast
import semtree, sempass
import walker
import tables
import strutils, sequtils
import os
import options

type
  CCodegenError* = object of Exception
  SrcExpr* = object
    headers*: Table[string, bool]
    src*: string
  CCodegenPass* = ref object of SemPass
    modulesrcs*: Table[ScopeIdent, SrcExpr]

proc initSrcExpr*(): SrcExpr =
  result.headers = initTable[string, bool]()
  result.src = ""
proc `&=`*(src: var SrcExpr, s: string) = src.src &= s
proc `$`*(src: SrcExpr): string =
  toSeq(src.headers.keys).mapIt("#include \"$#\"" % it).join("\n") & "\n" & src.src
proc addHeader*(src: var SrcExpr, s: string) =
  src.headers[s] = true

proc newCCodegenPass*(): CCodegenPass =
  CCodegenPass(modulesrcs: initTable[ScopeIdent, SrcExpr]())

proc generateInitDecls*(pass: CCodegenPass): string =
  result = ""
  for si, module in pass.modulesrcs:
    result &= "void " & si.name & "_init();\n"
proc generateInits*(pass: CCodegenPass): string =
  result = ""
  for si, module in pass.modulesrcs:
    result &= si.name & "_init();\n"
proc generateMain*(pass: CCodegenPass): string =
  result = ""
  result &= pass.generateInitDecls()
  result &= "int main() {\n"
  result &= pass.generateInits()
  result &= "}\n"

proc codegenExpr*(pass: CCodegenPass, src: var SrcExpr, semexpr: SemExpr)

proc codegenSym*(pass: CCodegenPass, src: var SrcExpr, semsym: SemSym) =
  case semsym.kind
  of symUnresolve:
    semsym.name.sexpr.error("codegenSym: unresolve symbol")
  of symSemFunc:
    case semsym.sf.kind
    of sfFunc:
      src &= semsym.sf.funcname
    of sfCFunc:
      src &= semsym.sf.cfuncname
  of symSemType:
    case semsym.st.kind
    of stCType:
      src &= semsym.st.ctypename
    of stStruct:
      discard # TODO:
    of stProtocol:
      discard # TODO:

proc codegenFuncImpl*(pass: CCodegenPass, src: var SrcExpr, semfunc: SemFunc) =
  pass.codegenSym(src, semfunc.functype.returntype)
  src &= " "
  src &= semfunc.funcname
  src &= "("
  if semfunc.funcargs.len > 0:
    pass.codegenSym(src, semfunc.functype.argtypes[0])
    src &= " "
    pass.codegenExpr(src, semfunc.funcargs[0])
    for i in 1..<semfunc.funcargs.len:
      src &= ", "
      pass.codegenSym(src, semfunc.functype.argtypes[i])
      src &= " "
      pass.codegenExpr(src, semfunc.funcargs[i])
  src &= ") {\n"
  for b in semfunc.funcbody[0..^2]:
    pass.codegenExpr(src, b)
    src &= ";\n"
  if not semfunc.getReturnType().isVoidType():
    src &= "return "
  pass.codegenExpr(src, semfunc.funcbody[^1])
  src &= ";\n"
  src &= "}\n"

proc codegenFunc*(pass: CCodegenPass, src: var SrcExpr, semfunc: SemFunc) =
  case semfunc.kind
  of sfCFunc:
    if semfunc.cfuncheader.isSome:
      src.addHeader(semfunc.cfuncheader.get)
  of sfFunc:
    pass.codegenFuncImpl(src, semfunc)

proc codegenType*(pass: CCodegenPass, src: var SrcExpr, semtype: SemType) =
  case semtype.kind
  of stCType:
    if semtype.ctypeheader.isSome:
      src.addHeader(semtype.ctypeheader.get)
  of stStruct:
    discard # TODO:
  of stProtocol:
    discard # TODO:

proc codegenFuncCall*(pass: CCodegenPass, src: var SrcExpr, semexpr: SemExpr) =
  if semexpr.fn.sf.kind == sfCFunc and semexpr.fn.sf.cfuncinfix:
    src &= "("
    pass.codegenExpr(src, semexpr.args[0])
    src &= " "
    pass.codegenSym(src, semexpr.fn)
    src &= " "
    pass.codegenExpr(src, semexpr.args[1])
    src &= ")"
  else:
    pass.codegenSym(src, semexpr.fn)
    src &= "("
    if semexpr.args.len > 0:
      pass.codegenExpr(src, semexpr.args[0])
      for arg in semexpr.args[1..^1]:
        src &= ", "
        pass.codegenExpr(src, arg)
    src &= ")"

proc codegenExpr*(pass: CCodegenPass, src: var SrcExpr, semexpr: SemExpr) =
  case semexpr.kind
  of seIdent:
    src &= semexpr.idname
  of seFuncCall:
    pass.codegenFuncCall(src, semexpr)
  of seInt:
    src &= $semexpr.intval
  of seString:
    src &= "\"" & semexpr.strval & "\""
  else:
    semexpr.sexpr.error("$# is unsupported expression in C codegen." % $semexpr.kind)

method execute*(pass: CCodegenPass, ctx: SemPassContext) =
  for si, module in ctx.walkModule:
    var modsrc = initSrcExpr()
    for tp in module.walkType:
      pass.codegenType(modsrc, tp)
    for fn in module.walkFunc:
      pass.codegenFunc(modsrc, fn)

    modsrc &= "void $#_init() {\n" % si.name
    for top in module.walkTopExpr:
      pass.codegenExpr(modsrc, top)
      modsrc &= ";\n"
    modsrc &= "}\n"
    
    pass.modulesrcs[si] = modsrc

proc write*(pass: CCodegenPass, dir: string) =
  if not existsDir(dir):
    createDir(dir)
  for si, modsrc in pass.modulesrcs:
    writeFile(dir / si.name & ".c", $modsrc)
  writeFile(dir / "main.c", pass.generateMain())
