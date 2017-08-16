
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

proc genModule*(context: CCodegenContext, sym: string, module: Module, compiletime = false): CCodegenModule

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
  s.replace("+", "_add_").replace("-", "_sub_").replace("*", "_mul_").replace("/", "_div_").replace("!", "_excl_")

proc genPattern*(pattern: string, args: seq[string]): string =
  var respat = pattern
  for i in 0..<args.len:
    respat = respat.replace("$" & $(i+1), $args[i])
  return respat
  
proc genSym*(scope: Scope, sym: TypeSymbol): string
proc genSym*(scope: Scope, sym: Symbol): string

proc genSymHash*(typesym: TypeSymbol): string =
  case typesym.kind
  of typesymSpec:
    let sym = typesym.getSymbol()
    if sym.semexpr.kind == semanticPrimitiveType:
      return $sym & "_" & sym.semexpr.primtype.argtypes.mapIt(genSymHash(it)).join("_")
    elif sym.semexpr.kind == semanticStruct:
      return $sym & "_" & sym.semexpr.struct.argtypes.mapIt(genSymHash(it)).join("_")
    else:
      return $sym
  of typesymTypeGenerics:
    return $typesym.getSymbol() & "_" & typesym.genericstypes.mapIt(genSymHash(it)).join("_")
  of typesymTypedesc:
    return "Typedesc_" & genSymHash(typesym.typedescsym)
  of typesymVarargs:
    return "Varargs_" & genSymHash(typesym.varargssym)
  of typesymReftype:
    return "Ref_" & genSymHash(typesym.reftypesym)
  else:
    raise newException(CCodegenError, "$# can't genSymHash" % typesym.debug)

proc genSymHash*(name: string, argtypes: seq[TypeSymbol]): string =
  result = name
  for argtype in argtypes:
    result &= "_" & genSymHash(argtype)

proc genSym*(scope: Scope, sym: Symbol): string =
  if sym.semexpr.kind == semanticPrimitiveType:
    return genPattern(sym.semexpr.primtype.primname, sym.semexpr.primtype.argtypes.mapIt(genSym(scope, it)))
  elif sym.semexpr.kind == semanticPrimitiveValue:
    return sym.semexpr.primValue
  elif sym.semexpr.kind == semanticStruct:
    return genSymHash($sym, sym.semexpr.struct.argtypes)
  elif sym.semexpr.kind == semanticReftype:
    return "$#*" % genSym(scope, sym.semexpr.reftype.typ)
  elif sym.semexpr.kind == semanticGenerics or sym.semexpr.kind == semanticTypeGenerics:
    sym.raiseError("couldn't specialize generics param: $#" % sym.debug)
  elif sym.semexpr.kind == semanticNotType:
    sym.raiseError("can't genSym kind: $#" % $sym.semexpr.kind)
  # elif sym.semexpr.kind in {semanticArgType, semanticSymbol}:
  #   return ("$#_$#" % [scope.module.name, sym.name]).replaceSpecialSymbols()
  else:
    return ($sym).replaceSpecialSymbols()
proc genSym*(scope: Scope, sym: TypeSymbol): string =
  return genSym(scope, sym.getSymbol())

proc genStruct*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  if semexpr.struct.isGenerics:
    return

  var structhash = genSymHash($semexpr.struct.sym, semexpr.struct.argtypes)
  let fields = semexpr.struct.fields
  module.addCommon("typedef struct {\n")
  module.indent:
    for field in fields:
      module.addCommon("$$i$# $#;\n" % [genSym(module.scope, field.typesym), field.name])
  module.addCommon("} $#;\n" % [structhash])

proc genStructConstructor*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  let values = semexpr.structconstructor.values
  let tmpsym = genTmpSym(module)
  res.addPrev("$$i$# $# = {" % [genSym(module.scope, semexpr.typesym), tmpsym])
  for value in values:
    res.addPrev(".$# = " % value.name)
    var fieldres = newCCodegenRes()
    gen(module, value.value, fieldres)
    res.formatPrev("$#", fieldres)
    res.addPrev(", ")
  res.addPrev("};\n")
  res.addSrc("$#" % tmpsym)

proc genFieldAccess*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  module.gen(semexpr.fieldaccess.valuesym, res)
  if semexpr.fieldaccess.valuesym.kind == semanticSymbol and semexpr.fieldaccess.valuesym.symbol.semexpr.typesym.kind == typesymReftype:
    res.addSrc("->")
  else:
    res.addSrc(".")
  res.addSrc(semexpr.fieldaccess.fieldname)

proc genVariable*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  let varname = semexpr.variable.name
  let value = semexpr.variable.value
  let vartype = genSym(module.scope, semexpr.typesym)
  res.addSrc("$# $#_$# = " % [vartype, semexpr.variable.scope.module.name, varname])
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

  if semexpr.typesym.isVoid:
    res.formatPrev("$iif ($#) {\n", condres)
    res.addPrevs([condres, tbodyres, fbodyres])
    res.formatPrev("$i  $#;\n", tbodyres)
    res.formatPrev("$i} else {\n")
    res.formatPrev("$i  $#;\n", fbodyres)
    res.formatPrev("$i}\n")
  else:
    res.formatPrev("$i$# $#;\n", rettype, tmpsym)
    res.formatPrev("$iif ($#) {\n", condres)
    res.addPrevs([condres, tbodyres, fbodyres])
    res.formatPrev("$i  $# = $#;\n", tmpsym, tbodyres)
    res.formatPrev("$i} else {\n")
    res.formatPrev("$i  $# = $#;\n", tmpsym, fbodyres)
    res.formatPrev("$i}\n")
    res.addSrc(tmpsym)

proc genWhileSyntax*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  var condres = newCCodegenRes()
  gen(module, semexpr.whilesyntax.cond, condres)
  var bodyress = newSeq[CCodegenRes]()
  for b in semexpr.whilesyntax.body:
    var bodyres = newCCodegenRes()
    gen(module, b, bodyres)
    bodyress.add(bodyres)
  res.formatPrev("$iwhile ($#) {\n", condres)
  for bres in bodyress:
    res.formatPrev("$i  $#;\n", bres)
  res.formatPrev("$i}\n")

proc genSetSyntax*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  var varres = newCCodegenRes()
  gen(module, semexpr.setsyntax.variable, varres)
  var valueres = newCCodegenRes()
  gen(module, semexpr.setsyntax.value, valueres)
  res.format("$# = $#", varres, valueres)

proc genDecl*(module: var CCodegenModule, semexpr: SemanticExpr): string =
  let funcname = module.scope.genSym(semexpr.function.sym).replaceSpecialSymbols()
  var argnames = newSeq[string]()
  var argtypes = newSeq[string]()
  for i, argtype in semexpr.function.fntype.argtypes:
    if argtype.kind == typesymTypedesc:
      continue
    elif argtype.kind == typesymReftype:
      argnames.add(semexpr.function.argnames[i])
      argtypes.add(genSym(module.scope, argtype) & "*")
    else:
      argnames.add(semexpr.function.argnames[i])
      argtypes.add(genSym(module.scope, argtype))
  let funchash = genSymHash(funcname, semexpr.function.fntype.argtypes)
  let rettype = genSym(module.scope, semexpr.function.fntype.returntype)
  var argsrcs = newSeq[string]()
  for i in 0..<argnames.len:
    argsrcs.add("$# $#_$#" % [argtypes[i], semexpr.function.sym.scope.module.name, argnames[i]])
  return "$# $#($#)" % [rettype, funchash, argsrcs.join(", ")]

proc genFunction*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  if semexpr.function.isGenerics:
    return

  var ress = newSeq[CCodegenRes]()
  for e in semexpr.function.body:
    var res = newCCodegenRes()
    gen(module, e, res)
    ress.add(res)

  let decl = genDecl(module, semexpr)
  module.addSrc(decl & " {\n")
  module.indent:
    if ress.len != 0:
      for i in 0..<ress.len-1:
        module.addSrc("$i")
        module.addSrc(ress[i])
        module.addSrc(";\n")

      # generate return
      if module.scope.isReturnType(semexpr.function.body[^1].typesym, semexpr.function.fntype.returntype):
        module.addSrc("$#" % ress[^1].prev)
        module.addSrc("$$ireturn $#;\n" % ress[^1].src)
      else:
        module.addSrc("$#" % ress[^1].prev)
        module.addSrc("$$i$#;\n" % ress[^1].src)
  module.addSrc("}\n")
  module.addHeader(decl & ";\n")

proc genFuncDecl*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  if semexpr.funcdecl.fndef.get.semexpr.function.isGenerics:
    return
  module.addToplevel(genDecl(module, semexpr.funcdecl.fndef.get.semexpr) & ";\n")
  genFunction(module, semexpr.funcdecl.fndef.get.semexpr, res)

proc genMacro*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  let funcsemexpr = semexpr.semmacro.funcsemexpr
  genFunction(module, funcsemexpr, res)
  let funcname = module.scope.genSym(funcsemexpr.function.sym).replaceSpecialSymbols()
  let funchash = genSymHash(funcname, funcsemexpr.function.fntype.argtypes)
  module.addSrc("__flori_SExpr $#(__flori_SExpr sexpr) {\n" % [funchash & "_macro"])
  module.indent:
    var args = newSeq[string]()
    for i, argtype in funcsemexpr.function.fntype.argtypes:
      args.add("sexpr$#.first" % repeat(".rest", i)) # FIXME:
    module.addSrc("$$ireturn $#($#)" % [funchash, args.join(", ")])
  module.addSrc("}\n")
  module.addHeader("__flori_SExpr $#(__flori_SExpr sexpr);\n" % [funchash & "_macro"])

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

proc genFuncCall*(module: var CCodegenModule, semexpr: SemanticExpr, funcsemexpr: SemanticExpr, argress: seq[CCodegenRes], res: var CCodegenRes) =
  let callfunc = semexpr.funccall.callfunc
  let argtypes = funcsemexpr.function.fntype.argtypes
  let funchash = genSymhash($callfunc, argtypes)
  var finalargress = newSeq[string]()
  for i in 0..<argress.len:
    if argtypes[i].kind == typesymTypedesc:
      continue
    elif argtypes[i].kind == typesymReftype:
      finalargress.add("&" & $argress[i])
    else:
      finalargress.add($argress[i])
  res.addSrc("$#($#)" % [funchash.replaceSpecialSymbols(), finalargress.mapIt($it).join(", ")])

proc genFuncCall*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  var argress = newSeq[CCodegenRes]()
  for i, arg in semexpr.funccall.args:
    var res = newCCodegenRes()
    gen(module, arg, res)
    argress.add(res)

  let funcsemexpr = semexpr.funccall.callfunc.getSemExpr()

  # TODO: Typedesc for PrimitiveFunc
  if funcsemexpr.kind == semanticPrimitiveFunc:
    genPrimitiveFuncCall(module, funcsemexpr, res, argress)
  elif funcsemexpr.kind == semanticFunction:
    genFuncCall(module, semexpr, funcsemexpr, argress, res)
  elif funcsemexpr.kind == semanticFuncDecl:
    genFuncCall(module, semexpr, funcsemexpr.funcdecl.fndef.get.semexpr, argress, res)
  elif funcsemexpr.kind == semanticPrimitiveType:
    res.addSrc(genSym(module.scope, semexpr.funccall.callfunc))
  else:
    funcsemexpr.raiseError("$# is not function kind: ($# $#)" % [$funcsemexpr.kind, semexpr.funccall.callfunc.debug, semexpr.funccall.args.mapIt($it.typesym).join(" ")])

proc genPrimitiveTypeExpr*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  if semexpr.primtype.isGenerics:
    return
  res.addSrc(genPattern(semexpr.primtype.primname, semexpr.primtype.argtypes.mapIt(genSym(module.scope, it))))

proc gen*(module: var CCodegenModule, semexpr: SemanticExpr, res: var CCodegenRes) =
  case semexpr.kind
  of semanticNotType:
    discard
  of semanticArgType:
    res.addSrc(semexpr.argtypeident)
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
  of semanticFuncDecl:
    if semexpr.funcdecl.fndef.isNone:
      semexpr.raiseError("not define implementation")
    genFuncDecl(module, semexpr, res)
  of semanticFunction:
    genFunction(module, semexpr, res)
  of semanticMacro:
    genMacro(module, semexpr, res)
  of semanticPrimitiveType:
    genPrimitiveTypeExpr(module, semexpr, res)
  of semanticPrimitiveValue, semanticPrimitiveFunc, semanticPrimitiveEval:
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
        if $res != "":
          cgenmodule.addSrc("$$i$#;\n" % $res)
  cgenmodule.addSrc("}\n")
  cgenmodule.addheader("void $#();\n" % initfuncname)
  context.addMainSrc("$$i$#();\n" % initfuncname)

proc genModule*(context: CCodegenContext, sym: string, module: Module, compiletime = false): CCodegenModule =
  var cgenmodule = newCCodegenModule(context, newScope(module))
  genHeaders(context, cgenmodule, sym, module)
  for symbol in module.symbols:
    var res = newCCodegenRes()
    if not symbol.isImported:
      if compiletime:
        gen(cgenmodule, symbol.semexpr, res)
      elif not symbol.semexpr.compiletime:
        gen(cgenmodule, symbol.semexpr, res)
      
  genToplevelCalls(context, cgenmodule, sym, module)
  context.modules[sym] = cgenmodule
  return cgenmodule

proc genContext*(context: CCodegenContext, semcontext: SemanticContext) =
  discard genModule(context, semcontext.topmodule.name, semcontext.topmodule)
