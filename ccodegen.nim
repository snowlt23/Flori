
import sparser
import sast
import semantic

import os
import strutils, sequtils
import tables
import options

type
  CCodegenError* = object of Exception
  CCodegenRes* = object
    prev*: string
    src*: string
  CCodegenModule* = object
    context*: CCodegenContext
    toplevel*: string
    src*: string
    header*: string
    curindent*: int
    scope*: Scope
    symcount*: int
  CCodegenContext* = ref object
    modules*: OrderedTable[string, CCodegenModule]
    mainsrc*: string

proc genModule*(context: CCodegenContext, sym: string, module: Module): CCodegenModule

proc newCCodegenContext*(): CCodegenContext =
  new result
  result.modules = initOrderedTable[string, CCodegenModule]()
  result.mainsrc = ""
proc format*(context: CCodegenContext, s: string): string =
  s.replace("$i", "  ")
proc addMainSrc*(context: CCodegenContext, s: string) =
  context.mainsrc &= context.format(s)
proc getMainSrc*(context: CCodegenContext): string =
  result = ""
  for cgenmodule in context.modules.values:
    result &= cgenmodule.header & "\n"
  result &= "int main() {\n"
  result &= context.mainsrc
  result &= "}\n"

proc newCCodegenModule*(context: CCodegenContext, scope: Scope): CCodegenModule =
  result.context = context
  result.toplevel = ""
  result.src = ""
  result.header = ""
  result.curindent = 0
  result.scope = scope
  result.symcount = 0
template indent*(module: CCodegenModule, body: untyped) =
  module.curindent += 1
  body
  module.curindent -= 1
proc genIndent*(module: CCodegenModule): string =
  return repeat("  ", module.curindent)
proc format*(module: CCodegenModule, s: string): string =
  return s.replace("$i", module.genIndent())
proc addToplevel*(module: var CCodegenModule, s: string) =
  module.toplevel &= module.format(s)
proc addSrc*(module: var CCodegenModule, s: string) =
  module.src &= module.format(s)
proc addSrc*(module: var CCodegenModule, res: CCodegenRes) =
  module.src &= module.format(res.prev)
  module.src &= module.format(res.src)
proc addHeader*(module: var CCodegenModule, s: string) =
  module.header &= module.format(s)
proc addHeader*(module: var CCodegenModule, res: CCodegenRes) =
  module.header &= module.format(res.prev)
  module.header &= module.format(res.src)
proc addCommon*(module: var CCodegenModule, s: string) =
  module.addSrc(s)
  module.addHeader(s)

proc newCCodegenRes*(): CCodegenRes =
  result.prev = ""
  result.src = ""
proc addPrev*(res: var CCodegenRes, s: string) =
  res.prev &= s
proc addSrc*(res: var CCodegenRes, s: string) =
  res.src &= s
proc add*(res: var CCodegenRes, s: CCodegenRes) =
  res.prev &= s.prev
  res.src &= s.src
proc `$`*(res: CCodegenRes): string =
  res.prev & res.src
proc toCCodegenRes*(s: string): CCodegenRes =
  result = newCCodegenRes()
  result.addSrc(s)
proc toCCodegenRes*(res: CCodegenRes): CCodegenRes = res
proc format*(res: var CCodegenRes, s: string, args: varargs[CCodegenRes, toCCodegenRes]) =
  let splitted = s.split("$#")
  for i in 0..<splitted.len:
    res.addSrc(splitted[i])
    if i < args.len():
      res.add(args[i])
proc addPrevs*(res: var CCodegenRes, prevs: openArray[CCodegenRes]) =
  for prev in prevs:
    if prev.prev != "":
      res.addPrev("$i" & prev.prev & ";\n")
proc formatPrev*(res: var CCodegenRes, s: string, args: varargs[CCodegenRes, toCCodegenRes]) =
  let splitted = s.split("$#")
  for i in 0..<splitted.len:
    res.addPrev(splitted[i])
    if i < args.len:
      res.addPrev(args[i].src)

proc gen*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes)

proc genTmpSym*(module: var CCodegenModule, name = "tmp"): string =
  result = "__flori_" & name & $module.symcount
  module.symcount.inc

proc replaceSpecialSymbols*(s: string): string =
  s.replace("+", "_add_").replace("-", "_sub_").replace("*", "_mul_").replace("/", "_div_").replace("!", "excl")

proc genPattern*(pattern: string, args: seq[string]): string =
  var respat = pattern
  for i in 0..<args.len:
    respat = respat.replace("$" & $(i+1), $args[i])
  return respat

proc genSym*(scope: Scope, sym: Symbol): string =
  if sym.semexpr.kind == semanticPrimitiveType:
    return genPattern(sym.semexpr.primtype.primname, sym.semexpr.primtype.argtypes.mapIt(genSym(scope, it)))
  elif sym.semexpr.kind == semanticPrimitiveValue:
    return sym.semexpr.primValue
  elif sym.semexpr.kind == semanticGenerics:
    if sym.semexpr.generics.spec.isNone:
      sym.raiseError("couldn't specialize generics param: $#" % sym.name)
    return $sym.semexpr.generics.spec.get
  elif sym.semexpr.kind == semanticNotType:
    sym.raiseError("can't genSym kind: $#" % $sym.semexpr.kind)
  else:
    return ("$#_$#" % [scope.module.name, sym.name]).replaceSpecialSymbols()

proc genStruct*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  if semexpr.struct.isGenerics:
    return

  let structname = semexpr.struct.name
  let fields = semexpr.struct.fields
  module.addCommon("typedef struct {\n")
  module.indent:
    for field in fields:
      module.addCommon("$$i$# $#;\n" % [genSym(module.scope, field.typesym), field.name])
  module.addCommon("} $#_$#;\n" % [module.scope.module.name, structname])

proc genStructConstructor*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  let values = semexpr.structconstructor.values
  let tmpsym = genTmpSym(module)
  res.addPrev("$# $# = {" % [genSym(module.scope, semexpr.typesym), tmpsym])
  for value in values:
    res.addPrev(".$# = " % value.name)
    var fieldres = newCCodegenRes()
    gen(module, value.value, fieldres)
    res.formatPrev("$#", fieldres)
    res.addPrev(", ")
  res.addPrev("}")
  res.addSrc("$#" % tmpsym)

proc genFieldAccess*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  let valuename = $semexpr.fieldaccess.valuesym
  let fieldname = semexpr.fieldaccess.fieldname
  res.addSrc("$#.$#" % [valuename, fieldname])

proc genVariable*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  let varname = semexpr.variable.name
  let value = semexpr.variable.value
  let vartype = genSym(module.scope, semexpr.typesym)
  res.addSrc("$# $#_$# = " % [vartype, module.scope.module.name, varname])
  gen(module, value, res)

proc genIfExpr*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  let tmpsym = genTmpSym(module)
  let rettype = genSym(module.scope, semexpr.typesym)
  var condres = newCCodegenRes()
  var tbodyres = newCCodegenRes()
  var fbodyres = newCCodegenRes()
  gen(module, semexpr.ifexpr.cond, condres)
  gen(module, semexpr.ifexpr.tbody, tbodyres)
  gen(module, semexpr.ifexpr.fbody, fbodyres)

  if semexpr.typesym == notTypeSym or rettype == "void":
    res.formatPrev("if ($#) {\n", condres)
    res.addPrevs([condres, tbodyres, fbodyres])
    res.formatPrev("$i  $#;\n", tbodyres)
    res.formatPrev("$i} else {\n")
    res.formatPrev("$i  $#;\n", fbodyres)
    res.formatPrev("$i}")
  else:
    res.formatPrev("$# $#;\n", rettype, tmpsym)
    res.formatPrev("$iif ($#) {\n", condres)
    res.addPrevs([condres, tbodyres, fbodyres])
    res.formatPrev("$i  $# = $#;\n", tmpsym, tbodyres)
    res.formatPrev("$i} else {\n")
    res.formatPrev("$i  $# = $#;\n", tmpsym, fbodyres)
    res.formatPrev("$i}")
    res.addSrc(tmpsym)

proc genWhileSyntax*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  var condres = newCCodegenRes()
  gen(module, semexpr.whilesyntax.cond, condres)
  var bodyress = newSeq[CCodegenRes]()
  for b in semexpr.whilesyntax.body:
    var bodyres = newCCodegenRes()
    gen(module, b, bodyres)
    bodyress.add(bodyres)
  res.formatPrev("while ($#) {\n", condres)
  for bres in bodyress:
    res.formatPrev("$i  $#;\n", bres)
  res.formatPrev("$i}")

proc genSetSyntax*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  var varres = newCCodegenRes()
  gen(module, semexpr.setsyntax.variable, varres)
  var valueres = newCCodegenRes()
  gen(module, semexpr.setsyntax.value, valueres)
  res.format("$# = $#", varres, valueres)

proc genFunction*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  if semexpr.function.isGenerics:
    return

  let funcname = semexpr.function.name.replaceSpecialSymbols()
  let argnames = semexpr.function.argnames
  let argtypes = semexpr.function.fntype.argtypes.mapIt(genSym(module.scope, it))
  let funchash = funcname & "_" & semexpr.function.fntype.argtypes.mapIt($it).join("_")
  let rettype = genSym(module.scope, semexpr.function.fntype.returntype)
  var argsrcs = newSeq[string]()
  for i in 0..<argnames.len:
    argsrcs.add("$# $#_$#" % [argtypes[i], module.scope.module.name, argnames[i]])
  var ress = newSeq[CCodegenRes]()
  for e in semexpr.function.body:
    var res = newCCodegenRes()
    gen(module, e, res)
    ress.add(res)
  module.addSrc("$# $#_$#($#) {\n" % [rettype, module.scope.module.name, funchash, argsrcs.join(", ")])
  module.indent:
    for i in 0..<ress.len-1:
      module.addSrc("$i")
      module.addSrc(ress[i])
      module.addSrc(";\n")
    if ress[^1].prev != "":
      module.addSrc("$$i$#;\n" % ress[^1].prev)
    if semexpr.function.body[^1].typesym == module.scope.getSymbol(module.scope.newSemanticIdent(semexpr.span, "Void", @[])):
      module.addSrc("$$i$#;\n" % ress[^1].src)
    else:
      module.addSrc("$$ireturn $#;\n" % ress[^1].src)
  module.addSrc("}\n")
  module.addHeader("$# $#_$#($#);\n" % [rettype, module.scope.module.name, funchash, argsrcs.join(", ")])

proc genPrimitiveFuncCall*(module: var CCodegenModule, funcsemexpr: SemanticExpr, res: var CCodegenRes, argress: seq[CCodegenRes]) =
  case funcsemexpr.primfunc.kind
  of primitiveCall:
    if funcsemexpr.primfunc.pattern:
      res.addSrc(genPattern(funcsemexpr.primfunc.name, argress.mapIt($it)))
    else:
      res.addSrc("$#($#)" % [funcsemexpr.primfunc.name, argress.mapIt($it).join(", ")])
  of primitiveInfix:
    var src = "($# $# $#)" % [$argress[0], funcsemexpr.primfunc.name, $argress[1]]
    for i in 2..<argress.len:
      src = "($# $# $#)" % [src, funcsemexpr.primfunc.name, $argress[i]]
    res.addSrc(src)

proc genFuncCall*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  var argress = newSeq[CCodegenRes]()
  for arg in semexpr.funccall.args:
    var res = newCCodegenRes()
    gen(module, arg, res)
    argress.add(res)

  let funcsemexpr = semexpr.funccall.callfunc.semexpr
  if funcsemexpr.kind == semanticPrimitiveFunc:
    genPrimitiveFuncCall(module, funcsemexpr, res, argress)
  elif funcsemexpr.kind == semanticProtocolFunc:
    let semid = module.scope.newSemanticIdent(semexpr.span, semexpr.funccall.callfunc.name, semexpr.funccall.args.mapIt(it.typesym).getSemanticTypeArgs)
    let specfuncsym = module.scope.getSpecSymbol(semid)
    if specfuncsym.semexpr.kind == semanticPrimitiveFunc:
      genPrimitiveFuncCall(module, specfuncsym.semexpr, res, argress)
    else:
      let callfunc = semexpr.funccall.callfunc
      let argtypes = callfunc.semexpr.function.fntype.argtypes
      let funchash = callfunc.name & "_" & argtypes.mapIt($it).join("_")
      res.addSrc("$#_$#($#)" % [callfunc.scope.module.name.replaceSpecialSymbols(), funchash.replaceSpecialSymbols(), argress.mapIt($it).join(", ")])
  elif funcsemexpr.kind == semanticFunction:
    let callfunc = semexpr.funccall.callfunc
    let argtypes = callfunc.semexpr.function.fntype.argtypes
    let funchash = callfunc.name & "_" & argtypes.mapIt($it).join("_")
    res.addSrc("$#_$#($#)" % [callfunc.scope.module.name.replaceSpecialSymbols(), funchash.replaceSpecialSymbols(), argress.mapIt($it).join(", ")])
  elif funcsemexpr.kind == semanticPrimitiveType:
    res.addSrc(genSym(module.scope, semexpr.funccall.callfunc))
  else:
    funcsemexpr.raiseError("$# is not function kind" % $funcsemexpr.kind)

proc gen*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  case semexpr.kind
  of semanticNotType:
    discard
  of semanticIdent:
    res.addSrc(semexpr.ident)
  of semanticSymbol:
    res.addSrc(genSym(module.scope, semexpr.symbol))
  of semanticProtocol:
    discard
  of semanticStruct:
    genStruct(module, semexpr, res)
  of semanticStructConstructor:
    genStructConstructor(module, semexpr, res)
  of semanticFieldAccess:
    genFieldAccess(module, semexpr, res)
  of semanticVariable:
    genVariable(module, semexpr, res)
  of semanticIfExpr:
    genIfExpr(module, semexpr, res)
  of semanticWhileSyntax:
    genWhileSyntax(module, semexpr, res)
  of semanticSetSyntax:
    genSetSyntax(module, semexpr, res)
  of semanticFunction:
    genFunction(module, semexpr, res)
  of semanticPrimitiveValue, semanticPrimitiveType, semanticPrimitiveFunc, semanticPrimitiveEval:
    discard
  of semanticRequire:
    let retmodule = module.context.genModule(semexpr.requiremodule.name, semexpr.requiremodule)
    module.addToplevel(retmodule.header)
  of semanticFuncCall:
    genFuncCall(module, semexpr, res)
  of semanticInt:
    res.addSrc($semexpr.intval)
  of semanticString:
    res.addSrc("\"" & semexpr.strval & "\"")
  else:
    raise newException(CCodegenError, "$# is unsupport codegen kind" % $semexpr.kind)

proc genToplevelVariable*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  let varname = semexpr.variable.name
  let value = semexpr.variable.value
  let vartype = genSym(module.scope, semexpr.typesym)
  module.addToplevel("$# $#_$#;\n" % [vartype, module.scope.module.name, varname])
  module.addHeader("extern $# $#_$#;\n" % [vartype, module.scope.module.name, varname])
  res.addSrc("$#_$# = " % [module.scope.module.name, varname])
  gen(module, value, res)

proc genHeaders*(context: CCodegenContext, cgenmodule: var CCodegenModule, sym: string, module: Module) =
  for header in module.ccodegeninfo.headers.keys:
    cgenmodule.addCommon("#include \"$#\"\n" % header)

proc genCffis*(context: CCodegenContext, cgenmodule: var CCodegenModule, sym: string, module: Module) =
  for cffi in module.ccodegeninfo.cffis:
    let primname = cffi.primname
    let argtypes = cffi.argtypes.mapIt(genSym(cgenmodule.scope, it))
    let rettype = genSym(cgenmodule.scope, cffi.rettype)
    var argsrcs = newSeq[string]()
    for i in 0..<argtypes.len():
      argsrcs.add("$# arg$#" % [argtypes[i], $i])
    let declsrc = "$# $#($#);\n" % [rettype, primname, argsrcs.join(",")]
    cgenmodule.addSrc(declsrc)
    cgenmodule.addHeader(declsrc)

proc genToplevelCalls*(context: CCodegenContext, cgenmodule: var CCodegenModule, sym: string, module: Module) =
  let initfuncname = sym & "_flori_main"
  cgenmodule.addSrc("void $#() {\n" % initfuncname)
  cgenmodule.indent:
    for semexpr in module.toplevelcalls:
      if semexpr.kind == semanticVariable:
        var res = newCCodegenRes()
        genToplevelVariable(cgenmodule, semexpr, res)
        cgenmodule.addSrc("$$i$#;\n" % $res)
      elif semexpr.kind != semanticNotType:
        var res = newCCodegenRes()
        gen(cgenmodule, semexpr, res)
        cgenmodule.addSrc("$$i$#;\n" % $res)
  cgenmodule.addSrc("}\n")
  cgenmodule.addheader("void $#();\n" % initfuncname)
  context.addMainSrc("$$i$#();\n" % initfuncname)

proc genModule*(context: CCodegenContext, sym: string, module: Module): CCodegenModule =
  var cgenmodule = newCCodegenModule(context, newScope(module))
  genHeaders(context, cgenmodule, sym, module)
  for symbol in module.symbols:
    var res = newCCodegenRes()
    if not symbol.isImported:
      gen(cgenmodule, symbol.semexpr, res)
  genToplevelCalls(context, cgenmodule, sym, module)
  context.modules[sym] = cgenmodule
  return cgenmodule

proc genContext*(context: CCodegenContext, semcontext: SemanticContext) =
  discard genModule(context, semcontext.topmodule.name, semcontext.topmodule)
