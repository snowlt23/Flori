
import image

import sequtils, strutils
import options

proc scope*(sym: Symbol): FScope =
  sym.obj.scope
proc name*(sym: Symbol): IString =
  sym.obj.name
proc fexpr*(sym: Symbol): var FExpr =
  sym.obj.fexpr
proc kind*(sym: Symbol): SymbolKind =
  sym.obj.kind
proc types*(sym: Symbol): IArray[Symbol] =
  sym.obj.types
proc argtypes*(sym: Symbol): IArray[Symbol] =
  sym.obj.argtypes
proc rettype*(sym: Symbol): Symbol =
  sym.obj.rettype
proc wrapped*(sym: Symbol): Symbol =
  sym.obj.wrapped
proc instance*(sym: Symbol): Option[Symbol] =
  sym.obj.instance
proc `instance=`*(sym: Symbol, value: Option[Symbol]) =
  sym.obj.instance = value

proc symbol*(scope: FScope, name: IString, kind: SymbolKind, fexpr: FExpr): Symbol =
  var s: Symbolobj
  s.scope = scope
  s.name = name
  s.fexpr = fexpr
  s.kind = kind
  return genSymbol(s)
proc symbol*(scope: FScope, name: string, kind: SymbolKind, fexpr: FExpr): Symbol =
  scope.symbol(istring(name), kind, fexpr)
proc refsym*(sym: Symbol): Symbol =
  result = sym.scope.symbol(sym.name, symbolRef, sym.fexpr)
  result.obj.wrapped = sym
proc varsym*(sym: Symbol): Symbol =
  result = sym.scope.symbol(sym.name, symbolVar, sym.fexpr)
  result.obj.wrapped = sym

proc isSpecSymbol*(sym: Symbol): bool =
  if sym.kind == symbolType:
    return true
  elif sym.kind == symbolGenerics:
    return false
  elif sym.kind in {symbolRef, symbolVar}:
    return sym.wrapped.isSpecSymbol()
  elif sym.kind == symbolFuncType:
    for t in sym.argtypes:
      if not t.isSpecSymbol:
        return false
    if not sym.rettype.isSpecSymbol: return false
    return true
  elif sym.kind == symbolTypeGenerics:
    for t in sym.types:
      if not t.isSpecSymbol:
        return false
    return true
  else:
    echo "$# kind is not type." % $sym.kind
    assert(false)
proc isSpecTypes*(types: seq[Symbol]): bool =
  for t in types:
    if not t.isSpecSymbol:
      return false
  return true

proc toString*(sym: Symbol, desc: bool): string =
  case sym.kind
  of symbolTypeGenerics:
    if desc:
      $sym.scope.obj.name & "." & $sym.name & "[" & sym.types.mapIt(toString(it, desc)).join(",") & "]"
    else:
      $sym.name & "[" & sym.types.mapIt(toString(it, desc)).join(",") & "]"
  of symbolVar:
    toString(sym.wrapped, desc)
  of symbolRef:
    "ref " & toString(sym.wrapped, desc)
  of symbolFuncType:
    "Fn[$#] $#" % [sym.argtypes.mapIt(toString(it, desc)).join(", "), toString(sym.rettype, desc)]
  else:
    if desc:
      $sym.scope.obj.name & "." & $sym.name
    else:
      $sym.name
proc `$`*(sym: Symbol): string = toString(sym, false)

proc symcopy*(sym: SymbolObj): SymbolObj =
  result.scope = sym.scope
  result.name = sym.name
  result.fexpr = sym.fexpr
  result.instance = sym.instance
  case sym.kind
  of symbolTypeGenerics:
    result.types = sym.types
  of symbolVar, symbolRef:
    result.wrapped = sym.wrapped
  of symbolSyntax, symbolMacro:
    discard
  of symbolFUncType:
    result.argtypes = sym.argtypes
  of symbolConstant:
    result.constvalue = sym.constvalue
  else:
    discard
proc symcopy*(sym: Symbol): Symbol =
  genSymbol(sym.obj.symcopy)
