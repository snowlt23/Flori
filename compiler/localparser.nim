
import options
import linmem, image, fexpr

proc getName*(f: FExpr): Option[FExpr] =
  var pos = 1
  if f.len <= pos: return none(FExpr)
  if f[pos].kind != fexprIdent: return none(FExpr)
  return some(f[pos])

proc getGenerics*(f: FExpr): Option[FExpr] =
  var pos = 1
  if f.len <= pos: return none(FExpr)
  if f[pos].kind != fexprIdent: return none(FExpr)
  if f.len <= pos: return none(FExpr)
  if f[pos].kind != fexprArray: return none(FExpr)
  return some(f[pos])

proc getArguments*(f: FExpr): Option[FExpr] =
  var pos = 1
  if f.len <= pos: return none(FExpr)
  if f[pos].kind != fexprident: return none(FExpr)
  pos.inc
  if f.len <= pos: return none(FExpr)
  if f[pos].kind == fexprArray: pos.inc
  if f.len <= pos: return none(FExpr)
  if f[pos].kind != fexprList: return none(FExpr)
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

proc getFnBody*(f: FExpr): Option[FExpr] =
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
  if f[pos].kind == fexprPrefix: pos.inc
  if f.len <= pos: return none(FExpr)
  if f[pos].kind == fexprArray: pos.inc
  if f.len <= pos: return none(FExpr)
  return some(f[pos])

proc getTypeBody*(f: FExpr): Option[FExpr] =
  var pos = 1
  if f.len <= pos: return none(FExpr)
  if f[pos].kind != fexprident: return none(FExpr)
  pos.inc
  if f.len <= pos: return none(FExpr)
  if f[pos].kind == fexprArray: pos.inc
  if f.len <= pos: return none(FExpr)
  if f[pos].kind == fexprPrefix: pos.inc
  if f.len <= pos: return none(FExpr)
  if f[pos].kind == fexprArray: pos.inc
  if f.len <= pos: return none(FExpr)
  return some(f[pos])

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
