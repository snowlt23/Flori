
import macros
import types
import tables

macro defMetadata*(key: untyped, T: typed): untyped =
  let tsym = ident("Metadata" & $T)
  let keyhas = ident("has" & $key)
  let keyget = key
  let keyset = nnkAccQuoted.newTree((ident(($key) & "=")))
  let keystr = newLit($key)
  result = quote do:
    type `tsym` = ref object of Metadata
      data: `T`
    proc `keyhas`*(fexpr: FExpr): bool =
      fexpr.metadata.hasKey(`keystr`)
    proc `keyget`*(fexpr: FExpr): `T` =
      `tsym`(fexpr.metadata[`keystr`]).data
    proc `keyset`*(fexpr: FExpr, value: `T`) =
      fexpr.metadata[`keystr`] = `tsym`(data: value)
  # echo result.repr
