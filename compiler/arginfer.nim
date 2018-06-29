
import fcore
import options
import tables
import sequtils

proc arginfer*(scope: FScope, tbl: var Table[string, bool], fexpr: FExpr) =
  case fexpr.kind
  of fexprIdent:
    if scope.getDecl($fexpr).isNone:
      tbl[$fexpr] = true
  of fexprBlock:
    for son in fexpr.sons:
      scope.arginfer(tbl, son)
  else:
    discard
proc arginfer*(scope: FScope, fexpr: FExpr): seq[string] =
  var tbl = initTable[string, bool]()
  scope.arginfer(tbl, fexpr)
  return toSeq(tbl.keys)
