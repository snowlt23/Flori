
import macros
import types
import tables

macro defMetadata*(key: untyped, T: typed): untyped =
  let tsym = genSym(nskType)
  let keyhas = ident("has" & $key)
  let keyget = nnkAccQuoted.newTree(key)
  let keyset = nnkAccQuoted.newTree((ident(($key) & "=")))
  let keystr = newLit($key)
  result = quote do:
    type `tsym`* = ref object of Metadata
      data*: `T`
    proc `keyhas`*(fexpr: FExpr): bool =
      fexpr.metadata.hasKey(`keystr`)
    proc `keyget`*(fexpr: FExpr): `T` =
      `tsym`(fexpr.metadata[`keystr`]).data
    proc `keyset`*(fexpr: FExpr, value: `T`) =
      `tsym`(fexpr.metadata[`keystr`]).data = value
  echo result.repr

defMetadata(b, bool)
