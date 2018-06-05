
import fcore

import macros
import strutils, sequtils

var rootPass*: proc (scope: FScope, fexpr: var FExpr) = nil

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

var internals* = newSeq[string]()

template thruInternal*(fexpr: FExpr) =
  if fexpr.kind == fexprSeq and fexpr.len >= 1:
    for internal in internals:
      if $fexpr[0] == internal:
        return true
