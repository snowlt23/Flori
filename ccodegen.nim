
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
  CCodegenModule* = ref object
    src*: string
    header*: string
    curindent*: int
    scope*: Scope
    symcount*: int
  CCodegenContext* = ref object
    modules*: OrderedTable[string, CCodegenModule]
    mainsrc*: string

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

proc newCCodegenModule*(scope: Scope): CCodegenModule =
  new result
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

proc genTmpSym*(module: CCodegenModule, name = "tmp"): string =
  result = "__flori_" & name & $module.symcount
  module.symcount.inc

proc replaceSpecialSymbols*(s: string): string =
  s.replace("+", "_add_").replace("-", "_minus_")

proc genSym*(sym: Symbol): string =
  let semexpr = trySemanticExpr(sym)
  if semexpr.isSome and semexpr.get.kind == semanticPrimitiveType:
    let splitted = semexpr.get.primTypeName.split("$#")
    result = ""
    for i in 0..<splitted.len:
      result &= splitted[i]
      if i < semexpr.get.primTypeGenerics.len:
        result &= genSym(semexpr.get.primTypeGenerics[i])
  elif semexpr.isSome and semexpr.get.kind == semanticPrimitiveValue:
    return semexpr.get.primValue
  elif semexpr.isSome and semexpr.get.kind == semanticGenerics:
    return $semexpr.get.generics.spec.get
  else:
    return ($sym).replaceSpecialSymbols()
proc genSymbolArg*(symarg: SymbolArg): string =
  if symarg.kind == symbolargName:
    return $symarg.namesym
  elif symarg.kind == symbolargGenerics:
    let semexpr = symarg.genericssym.getSemanticExpr()
    return $semexpr.generics.spec.get
  else:
    raise newException(CCodegenError, "couldn't genSymbolArg: $#" % $symarg.kind)

proc genStruct*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  let structname = semexpr.struct.name
  let fields = semexpr.struct.fields
  module.addCommon("typedef struct {\n")
  module.indent:
    for field in fields:
      module.addCommon("$$i$# $#;\n" % [genSym(field.typesym), field.name])
  module.addCommon("} $#_$#;\n" % [module.scope.module.name, structname])

proc genFieldAccess*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  let valuename = $semexpr.fieldaccess.valuesym
  let fieldname = semexpr.fieldaccess.fieldname
  res.addSrc("$#.$#" % [valuename, fieldname])

proc genVariable*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  let varname = semexpr.variable.name
  let value = semexpr.variable.value
  let vartype = genSym(semexpr.typesym)
  res.addSrc("$# $#_$# = " % [vartype, module.scope.module.name, varname])
  gen(module, value, res)

proc genIfExpr*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  let tmpsym = genTmpSym(module)
  let rettype = genSym(semexpr.typesym)
  var condres = newCCodegenRes()
  var tbodyres = newCCodegenRes()
  var fbodyres = newCCodegenRes()
  gen(module, semexpr.ifexpr.cond, condres)
  gen(module, semexpr.ifexpr.tbody, tbodyres)
  gen(module, semexpr.ifexpr.fbody, fbodyres)

  if semexpr.typesym == notTypeSym:
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

proc genFunction*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  if semexpr.function.isGenerics:
    return

  let funcname = semexpr.function.name.replaceSpecialSymbols()
  let argnames = semexpr.function.argnames
  let argtypes = semexpr.function.fntype.argtypes.mapIt(genSym(it))
  let funchash = funcname & "_" & semexpr.function.fntype.argtypes.mapIt($it).join("_")
  let rettype = genSym(semexpr.function.fntype.returntype)
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
    if semexpr.function.body[^1].typesym == module.scope.getSymbol(newSNil(), "Void"):
      module.addSrc("$$i$#;\n" % ress[^1].src)
    else:
      module.addSrc("$$ireturn $#;\n" % ress[^1].src)
  module.addSrc("}\n")
  module.addHeader("$# $#_$#($#);\n" % [rettype, module.scope.module.name, funchash, argsrcs.join(", ")])

proc genFuncCall*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  var args = newSeq[CCodegenRes]()
  for arg in semexpr.funccall.args:
    var res = newCCodegenRes()
    gen(module, arg, res)
    args.add(res)
  # for sym, value in module.scope.module.semanticexprs:
  #   for value in value.symbols:
  #     echo value.sym, ":", value.value.kind
  let funcsemexpr = getSemanticExpr(semexpr.funccall.callfunc)
  if funcsemexpr.kind == semanticPrimitiveFunc:
    case funcsemexpr.primFuncKind
    of primitiveCall:
      res.addSrc("$#($#)" % [funcsemexpr.primFuncName, args.mapIt($it).join(", ")])
    of primitiveInfix:
      res.addSrc("($# $# $#)" % [$args[0], funcsemexpr.primFuncName, $args[1]])
  elif funcsemexpr.kind == semanticProtocolFunc:
    let sym = newSymbol(semexpr.sexpr, module.scope, semexpr.funccall.callfunc.name, semexpr.funccall.args.mapIt(getSymbolArg(it.getType.getType)))
    let specsemexpr = getSemanticExpr(sym, {semanticFunction, semanticPrimitiveFunc})
    if specsemexpr.kind == semanticPrimitiveFunc:
      case specsemexpr.primFuncKind
      of primitiveCall:
        res.addSrc("$#($#)" % [specsemexpr.primFuncName, args.mapIt($it).join(", ")])
      of primitiveInfix:
        res.addSrc("($# $# $#)" % [$args[0], specsemexpr.primFuncName, $args[1]])
    else:
      let callfunc = semexpr.funccall.callfunc
      let funchash = callfunc.name & "_" & callfunc.args.mapIt(genSymbolArg(it)).join("_")
      res.addSrc("$#_$#($#)" % [callfunc.scope.module.name.replaceSpecialSymbols(), funchash, args.mapIt($it).join(", ")])
  else:
    let callfunc = semexpr.funccall.callfunc
    let funchash = callfunc.name & "_" & callfunc.args.mapIt(genSymbolArg(it)).join("_")
    # TODO: Generics
    res.addSrc("$#_$#($#)" % [callfunc.scope.module.name.replaceSpecialSymbols(), funchash, args.mapIt($it).join(", ")])

proc gen*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  case semexpr.kind
  of semanticSymbol:
    res.addSrc(genSym(semexpr.symbol))
  of semanticProtocol:
    discard
  of semanticStruct:
    genStruct(module, semexpr, res)
  of semanticFieldAccess:
    genFieldAccess(module, semexpr, res)
  of semanticVariable:
    genVariable(module, semexpr, res)
  of semanticIfExpr:
    genIfExpr(module, semexpr, res)
  of semanticFunction:
    genFunction(module, semexpr, res)
  of semanticPrimitiveValue, semanticPrimitiveType, semanticPrimitiveFunc, semanticPrimitiveMacro, semanticPrimitiveEval:
    discard
  of semanticFuncCall:
    genFuncCall(module, semexpr, res)
  of semanticInt:
    res.addSrc($semexpr.intval)
  of semanticString:
    res.addSrc("\"" & semexpr.strval & "\"")
  else:
    raise newException(CCodegenError, "$# is unsupport codegen kind" % $semexpr.kind)

proc genHeaders*(context: CCodegenContext, cgenmodule: var CCodegenModule, sym: string, module: Module) =
  for header in module.ccodegeninfo.headers.keys:
    cgenmodule.addCommon("#include \"$#\"\n" % header)

proc genCffis*(context: CCodegenContext, cgenmodule: var CCodegenModule, sym: string, module: Module) =
  for cffi in module.ccodegeninfo.cffis:
    let primname = cffi.primname
    let argtypes = cffi.argtypes.mapIt(genSym(it))
    let rettype = genSym(cffi.rettype)
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
      var res = newCCodegenRes()
      gen(cgenmodule, semexpr, res)
      cgenmodule.addSrc("$$i$#;\n" % $res)
  cgenmodule.addSrc("}\n")
  cgenmodule.addheader("void $#();\n" % initfuncname)
  context.addMainSrc("$$i$#();\n" % initfuncname)

proc genModule*(context: CCodegenContext, sym: string, module: Module) =
  var cgenmodule = newCCodegenModule(newScope(module))
  context.modules[sym] = cgenmodule
  genHeaders(context, cgenmodule, sym, module)
  for semexpr in module.semanticexprs.values:
    for gsym in semexpr.symbols:
      var res = newCCodegenRes()
      gen(cgenmodule, gsym.value, res)
  genToplevelCalls(context, cgenmodule, sym, module)

proc genContext*(context: CCodegenContext, semcontext: SemanticContext) =
  for sym, module in semcontext.modules.pairs:
    genModule(context, sym, module)