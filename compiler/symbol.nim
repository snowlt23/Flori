
import strutils, sequtils
import linmem, image

proc `==`*(a, b: Scope): bool =
  a.name == b.name and a.level == b.level

proc symbol*(scope: Scope, name: IString, kind: SymbolKind, fexpr: FExpr): Symbol =
  result = genSymbol(SymbolObj(scope: scope, name: name, kind: kind, fexpr: fexpr))
  if kind == symbolTypeGenerics:
    result.types = iarray[Symbol](@[])
proc `==`*(a, b: Symbol): bool =
  a.name == b.name and a.scope == b.scope
proc toString*(sym: Symbol, desc: bool): string =
  case sym.kind
  of symbolTypeGenerics:
    if desc:
      $sym.scope.name & "." & $sym.name & "[" & sym.types.mapIt(toString(it, desc)).join(",") & "]"
    else:
      $sym.name & "[" & sym.types.mapIt(toString(it, desc)).join(",") & "]"
  of symbolVar:
    toString(sym.wrapped, desc)
  of symbolRef:
    "ref " & toString(sym.wrapped, desc)
  of symbolFuncType:
    "Fn[$#] $#" % [sym.argtypes.mapIt(toString(it, desc)).join(", "), toString(sym.rettype, desc)]
  of symbolIntLit:
    $sym.intval
  else:
    if desc:
      $sym.scope.name & "." & $sym.name
    else:
      $sym.name
proc `$`*(sym: Symbol): string = toString(sym, false)

proc refsym*(scope: Scope, sym: Symbol): Symbol =
  result = scope.symbol(sym.name, symbolRef, sym.fexpr)
  result.wrapped = sym
proc varsym*(scope: Scope, sym: Symbol): Symbol =
  if sym.kind == symbolVar:
    result = sym
  else:
    result = scope.symbol(sym.name, symbolVar, sym.fexpr)
    result.wrapped = sym
proc symcopy*(sym: Symbol): Symbol =
  result = sym.scope.symbol(sym.name, sym.kind, sym.fexpr)
  result.instance = sym.instance
  if sym.kind == symbolArg:
    result.argpos = sym.argpos
  elif sym.kind == symbolTypeGenerics:
    result.types = sym.types
  elif sym.kind == symbolIntLit:
    result.intval = sym.intval
  elif sym.kind == symbolFuncType:
    result.argtypes = sym.argtypes
    result.rettype = sym.rettype
  elif sym.kind in {symbolVar, symbolRef}:
    result.wrapped = sym.wrapped.symcopy
proc intsym*(scope: Scope, fexpr: FExpr): Symbol =
  result = scope.symbol(istring("IntLit"), symbolIntLit, fexpr)
  result.intval = fexpr.intval

proc isRef*(sym: Symbol): bool =
  if sym.kind == symbolRef:
    true
  elif sym.kind == symbolVar and sym.wrapped.kind == symbolRef:
    true
  else:
    false

#
# is spec
#
  
proc isSpecSymbol*(sym: Symbol): bool =
  if sym.kind == symbolType:
    return true
  elif sym.kind == symbolGenerics:
    return false
  elif sym.kind in {symbolRef, symbolVar}:
    return sym.wrapped.isSpecSymbol()
  elif sym.kind == symbolIntLit:
    return true
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
proc isSpecTypes*(types: FExpr): bool =
  for t in types.sons:
    if t.kind != fexprSymbol: return false
    if not t.symbol.isSpecSymbol: return false
  return true

