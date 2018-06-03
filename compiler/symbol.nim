
import sequtils, strutils

import image

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
