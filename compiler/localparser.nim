
import options
import linmem, image, fexpr

template defineLocal(name, pos) =
  proc name*(f: FExpr): var FExpr =
    f[pos]
  proc `name =`*(f: FExpr, v: FExpr) =
    f[pos] = v

defineLocal(fnName, 1)
defineLocal(fnGenerics, 2)
defineLocal(fnArguments, 3)
defineLocal(fnPragma, 5)
defineLocal(fnReturn, 6)
defineLocal(fnBody, 7)

defineLocal(typeName, 1)
defineLocal(typeGenerics, 2)
defineLocal(typePragma, 4)
defineLocal(typeBody, 5)

proc getIfBranches*(f: FExpr): seq[tuple[cond: Option[FExpr], body: FExpr]] =
  result = @[]
  var pos = 0
  while true:
    if f.len <= pos: break
    if $f[pos] == "if":
      result.add((some(f[pos+1]), f[pos+2]))
      pos += 3
    elif $f[pos] == "elif":
      result.add((some(f[pos+1]), f[pos+2]))
      pos += 3
    elif $f[pos] == "else":
      result.add((none(FExpr), f[pos+2]))
      pos += 2
    else:
      f[pos].error("unexpected expression in if")
