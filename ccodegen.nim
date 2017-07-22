
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
    src*: string
  CCodegenModule* = ref object
    src*: string
    header*: string
    curindent*: int
    scope*: Scope
  CCodegenContext* = ref object
    modules*: OrderedTable[string, CCodegenModule]
    mainsrc*: string

const preincludesrc* = """
#include <stdint.h>
"""

proc newCCodegenContext*(): CCodegenContext =
  new result
  result.modules = initOrderedTable[string, CCodegenModule]()
  result.mainsrc = ""
proc format*(context: CCodegenContext, s: string): string =
  s.replace("$i", "  ")
proc addMainSrc*(context: CCodegenContext, s: string) =
  context.mainsrc &= context.format(s)
proc getMainSrc*(context: CCodegenContext): string =
  result = preincludesrc
  for cgenmodule in context.modules.values:
    result &= cgenmodule.header & "\n"
  result &= "int main() {\n"
  result &= context.mainsrc
  result &= "}\n"

proc newCCodegenModule*(scope: Scope): CCodegenModule =
  new result
  result.src = preincludesrc
  result.header = preincludesrc
  result.curindent = 0
  result.scope = scope
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
  module.src &= res.src
proc addHeader*(module: var CCodegenModule, s: string) =
  module.header &= module.format(s)
proc addHeader*(module: var CCodegenModule, res: CCodegenRes) =
  module.header &= res.src
proc addCommon*(module: var CCodegenModule, s: string) =
  module.addSrc(s)
  module.addHeader(s)

proc newCCodegenRes*(): CCodegenRes =
  result.src = ""
proc add*(res: var CCodegenRes, s: string) =
  res.src &= s
proc `$`*(res: CCodegenRes): string =
  res.src

proc gen*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes)

proc genSym*(scope: Scope, sym: Symbol): string =
  let semexpr = scope.trySemanticExpr(sym)
  if semexpr.isSome and semexpr.get.kind == semanticPrimitiveType:
    return semexpr.get.primTypeName
  else:
    return $sym

proc genFunction*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  let funcname = semexpr.function.hash
  let argnames = semexpr.function.argnames
  let argtypes = semexpr.function.argtypes.mapIt(genSym(module.scope, it))
  let rettype = genSym(module.scope, semexpr.function.rettype)
  var argsrcs = newSeq[string]()
  for i in 0..<argnames.len:
    argsrcs.add("$# $#" % [argtypes[i], argnames[i]])
  var ress = newSeq[CCodegenRes]()
  for e in semexpr.function.body:
    var res = newCCodegenRes()
    gen(module, e, res)
    ress.add(res)
  module.addSrc("$# $#($#) {\n" % [rettype, funcname, argsrcs.join(",")])
  module.indent:
    for i in 0..<ress.len-1:
      module.addSrc("$i")
      module.addSrc(ress[i])
      module.addSrc(";\n")
    module.addSrc("$$ireturn $#;\n" % $ress[^1])
  module.addSrc("}\n")
  module.addHeader("$# $#($#);\n" % [rettype, funcname, argsrcs.join(",")])

proc genFuncCall*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  var args = newSeq[CCodegenRes]()
  for arg in semexpr.funccall.args:
    var res = newCCodegenRes()
    gen(module, arg, res)
    args.add(res)
  let funcsemexpr = module.scope.getSemanticExpr(semexpr.funccall.callfunc)
  if funcsemexpr.kind == semanticPrimitiveFunc:
    case funcsemexpr.primFuncKind
    of primitiveCall:
      res &= "$#($#)" % [funcsemexpr.primFuncName, args.mapIt($it).join(", ")]
    of primitiveInfix:
      res &= "($# $# $#)" % [$args[0], funcsemexpr.primFuncName, $args[1]]
  else:
    let callfunc = $semexpr.funccall.callfunc
    res &= "$#($#)" % [callfunc, args.mapIt($it).join(", ")]

proc genCFFI*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  let primname = semexpr.cffi.primname
  let argtypes = semexpr.cffi.argtypes.mapIt(genSym(module.scope, it))
  let rettype = genSym(module.scope, semexpr.cffi.rettype)
  var argsrcs = newSeq[string]()
  for i in 0..<argtypes.len():
    argsrcs.add("$# arg$#" % [argtypes[i], $i])
  let declsrc = "$# $#($#);\n" % [rettype, primname, argsrcs.join(",")]
  module.addSrc(declsrc)
  module.addHeader(declsrc)

proc gen*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  case semexpr.kind
  of semanticSymbol:
    res &= genSym(module.scope, semexpr.symbol)
  of semanticFunction:
    genFunction(module, semexpr, res)
  of semanticPrimitiveValue, semanticPrimitiveType, semanticPrimitiveFunc, semanticPrimitiveMacro:
    discard
  of semanticFuncCall:
    genFuncCall(module, semexpr, res)
  of semanticCFFI:
    genCFFI(module, semexpr, res)
  of semanticInt:
    res &= $semexpr.intval
  of semanticString:
    res &= "\"" & semexpr.strval & "\""
  else:
    raise newException(CCodegenError, "$# is unsupport codegen kind" % $semexpr.kind)

proc genHeaders*(context: CCodegenContext, cgenmodule: var CCodegenModule, sym: string, module: Module) =
  for header in module.ccodegeninfo.headers.keys:
    cgenmodule.addSrc("#include \"$#\"\n" % header)

proc genToplevelCalls*(context: CCodegenContext, cgenmodule: var CCodegenModule, sym: string, module: Module) =
  let initfuncname = sym & "_main"
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
    var res = newCCodegenRes()
    gen(cgenmodule, semexpr, res)
  genToplevelCalls(context, cgenmodule, sym, module)

proc genContext*(context: CCodegenContext, semcontext: SemanticContext) =
  for sym, module in semcontext.modules.pairs:
    genModule(context, sym, module)