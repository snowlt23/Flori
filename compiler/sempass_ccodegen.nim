
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
    prev*: string
    exp*: string
  CCodegenPass* = ref object of SemPass
    modulesrcs*: Table[ScopeIdent, SrcExpr]
    tmpcount*: int

proc initSrcExpr*(): SrcExpr =
  result.headers = initTable[string, bool]()
  result.prev = ""
  result.exp = ""
proc `&=`*(src: var SrcExpr, s: string) = src.exp &= s
proc getSrc*(src: SrcExpr): string =
  toSeq(src.headers.keys).mapIt("#include \"$#\"" % it).join("\n") & "\n\n" & src.exp
proc addHeader*(src: var SrcExpr, s: string) =
  src.headers[s] = true

proc newCCodegenPass*(): CCodegenPass =
  CCodegenPass(modulesrcs: initTable[ScopeIdent, SrcExpr](), tmpcount: 0)
proc gentmpsym*(pass: CCodegenPass): string =
  result = "__floritmp" & $pass.tmpcount
  pass.tmpcount.inc

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

proc codegenSym*(pass: CCodegenPass, src: var SrcExpr, semsym: SemSym)
proc codegenExpr*(pass: CCodegenPass, src: var SrcExpr, semexpr: SemExpr)

proc codegenFuncSym*(pass: CCodegenPass, src: var SrcExpr, scope: SemScope, name: string, types: seq[SemSym]) =
  src &= scope.name
  src &= "_"
  src &= name
  for typ in types:
    src &= "_"
    pass.codegenSym(src, typ)

proc codegenSym*(pass: CCodegenPass, src: var SrcExpr, semsym: SemSym) =
  case semsym.kind
  of symUnresolve:
    semsym.name.sexpr.error("codegenSym: unresolve symbol")
  of symSemFunc:
    case semsym.sf.kind
    of sfFunc:
      pass.codegenFuncSym(src, semsym.sf.scope, semsym.sf.funcname, semsym.sf.functype.argtypes)
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

proc codegenBody*(pass: CCodegenPass, src: var SrcExpr, body: seq[SemExpr], ret = false) =
  if body.len == 0:
    return
  if body.len >= 2:
    for b in body[0..^2]:
      var newsrc = initSrcExpr()
      pass.codegenExpr(newsrc, b)
      src &= newsrc.prev
      src.prev = ""
      src &= newsrc.exp & ";\n"
  var newsrc = initSrcExpr()
  pass.codegenExpr(newsrc, body[^1])
  src &= newsrc.prev
  src.prev = ""
  if ret:
    src &= "return "
  src &= newsrc.exp & ";\n"

proc codegenFuncImpl*(pass: CCodegenPass, src: var SrcExpr, semfunc: SemFunc) =
  pass.codegenSym(src, semfunc.functype.returntype)
  src &= " "
  pass.codegenFuncSym(src, semfunc.scope, semfunc.funcname, semfunc.functype.argtypes)
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
  pass.codegenBody(src, semfunc.funcbody, ret = not semfunc.getReturnType().isVoidType())
  src &= "}\n\n"

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

proc codegenVar*(pass: CCodegenPass, src: var SrcExpr, semexpr: SemExpr) =
  pass.codegenSym(src, semexpr.typ.get)
  src &= " "
  src &= semexpr.varname
  src &= " = "
  pass.codegenExpr(src, semexpr.varvalue)
proc codegenIf*(pass: CCodegenPass, src: var SrcExpr, semexpr: SemExpr) =
  var condsrc = initSrcExpr()
  pass.codegenExpr(condsrc, semexpr.ifcond)
  var truesrc = initSrcExpr()
  pass.codegenExpr(truesrc, semexpr.iftrue)
  var falsesrc = initSrcExpr()
  pass.codegenExpr(falsesrc, semexpr.iffalse)
  if semexpr.typ.get.isVoidType:
    src.prev &= "if (" & condsrc.exp & ") {\n"
    src.prev &= truesrc.prev
    src.prev &= truesrc.exp & ";\n"
    src.prev &= "} else {\n"
    src.prev &= falsesrc.prev
    src.prev &= falsesrc.exp & ";\n"
    src.prev &= "}"
  else:
    let tmpsym = pass.gentmpsym()
    var tmptyp = initSrcExpr()
    pass.codegenSym(tmptyp, semexpr.iftrue.typ.get)
    src.prev &= tmptyp.prev
    src.prev &= tmptyp.exp & " " & tmpsym & ";\n"
    src.prev &= "if (" & condsrc.exp & ") {\n"
    src.prev &= truesrc.prev
    src.prev &= tmpsym & " = " & truesrc.exp & ";\n"
    src.prev &= "} else {\n"
    src.prev &= falsesrc.prev
    src.prev &= tmpsym & " = " & falsesrc.exp & ";\n"
    src.prev &= "}"
    src &= tmpsym
proc codegenWhile*(pass: CCodegenPass, src: var SrcExpr, semexpr: SemExpr) =
  src &= "while ("
  pass.codegenExpr(src, semexpr.whilecond)
  src &= ") {\n"
  pass.codegenBody(src, semexpr.whilebody)
  src &= "}"

proc codegenExpr*(pass: CCodegenPass, src: var SrcExpr, semexpr: SemExpr) =
  case semexpr.kind
  of seIdent:
    src &= semexpr.idname
  of seFuncCall:
    pass.codegenFuncCall(src, semexpr)
  of seVar:
    pass.codegenVar(src, semexpr)
  of seIf:
    pass.codegenIf(src, semexpr)
  of seWhile:
    pass.codegenWhile(src, semexpr)
  of seInt:
    src &= $semexpr.intval
  of seString:
    src &= "\"" & semexpr.strval & "\""
  else:
    semexpr.sexpr.error("$# is unsupported expression in C codegen." % $semexpr.kind)

proc codegenTopVarDecl*(pass: CCodegenPass, src: var SrcExpr, semexpr: SemExpr) =
  pass.codegenSym(src, semexpr.varvalue.typ.get)
  src &= " "
  src &= semexpr.scope.name & "_" & semexpr.varname
  src &= ";\n"
proc codegenTopVarInit*(pass: CCodegenPass, src: var SrcExpr, semexpr: SemExpr) =
  src &= semexpr.scope.name & "_" & semexpr.varname
  src &= " = "
  pass.codegenExpr(src, semexpr.varvalue)
  src &= ";\n"

method execute*(pass: CCodegenPass, ctx: SemPassContext) =
  for si, module in ctx.walkModule:
    var modsrc = initSrcExpr()
    for tp in module.walkType:
      pass.codegenType(modsrc, tp)
    for fn in module.walkFunc:
      pass.codegenFunc(modsrc, fn)
    for e in module.walkTopVar:
      pass.codegenTopVarDecl(modsrc, e)

    modsrc &= "void $#_init() {\n" % si.name
    for e in module.walkTopVar:
      pass.codegenTopVarInit(modsrc, e)
    pass.codegenBody(modsrc, module.toplevels)
    modsrc &= "}\n"
    
    pass.modulesrcs[si] = modsrc

proc filenames*(pass: CCodegenPass, dir: string): seq[string] =
  result = @[]
  for si, modsrc in pass.modulesrcs:
    result.add(dir / si.name & ".c")
  result.add(dir / "main.c")

proc write*(pass: CCodegenPass, dir: string) =
  if not existsDir(dir):
    createDir(dir)
  for si, modsrc in pass.modulesrcs:
    writeFile(dir / si.name & ".c", modsrc.getSrc)
  writeFile(dir / "main.c", pass.generateMain())
