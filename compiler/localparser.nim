
import options
import linmem, image, fexpr

proc getName*(f: FExpr): Option[FExpr] =
  var pos = 1
  if f.len <= pos: return none(FExpr)
  if f[pos].kind != fexprident: return none(FExpr)
  return some(f[pos])

proc getReturn*(f: FExpr): Option[FExpr] =
  var pos = 1
  if f.len <= pos: return none(FExpr)
  if f[pos].kind != fexprident: return none(FExpr)
  pos.inc
  if f.len <= pos: return none(FExpr)
  if f[pos].kind == fexprArray: pos.inc
  if f.len <= pos: return none(FExpr)
  if f[pos].kind != fexprList: return none(FExpr)
  pos.inc
  if f.len <= pos: return none(FExpr)
  return some(f[pos])

