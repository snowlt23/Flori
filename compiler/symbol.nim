
import sequtils, strutils
import image

proc toString*(sym: Symbol, desc: bool): string =
  case sym.kind
  of symbolTypeGenerics:
    if desc:
      sym.scope.obj.name & "." & $sym.name & "[" & sym.types.mapIt(toString(it, desc)).join(",") & "]"
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
