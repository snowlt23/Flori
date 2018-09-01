
import linmem, image, fexpr, scope

import macros
import strutils, sequtils

var rootPass*: proc (scope: Scope, fexpr: var FExpr) = nil

macro definePass*(passname: untyped, rootname: untyped, argtypes: untyped, passes: untyped): untyped =
  var args = newSeq[string]()
  var callargs = newSeq[string]()
  for i, argtype in argtypes:
    args.add("arg$#: $#" % [$i, argtype.repr])
    callargs.add("arg$#" % $i)
  result = parseExpr("proc $#*($#) = discard" % [$passname, args.join(", ")])
  var procbody = newStmtList()
  procbody.add(quote do:
    if `rootname`.isNil:
      `rootname` = `passname`
  )
  for pass in passes:
    pass.expectKind(nnkIdent)
    procbody.add(parseExpr("if not $#($#): return" % [$pass, callargs.join(", ")]))
  result[6] = procbody

template thruInternal*(fexpr: FExpr) =
  if fexpr.metadata.internal != internalNone:
    return true
