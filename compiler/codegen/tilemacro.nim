
import macros
import strutils, sequtils
import options

import tacode, x86code

macro defTile*(name: untyped, body: untyped): untyped =
  let bufid = ident"buf"
  let ctxid = ident"ctx"

  var patopt = none(NimNode)
  var codeopt = none(NimNode)
  var matchopt = newStmtList()
  for b in body:
    b.expectKind(nnkCall)
    if $b[0] == "PATTERN":
      patopt = some(b[1])
    elif $b[0] == "CODE":
      codeopt = some(b[1])
    elif $b[0] == "MATCH":
      matchopt = b[1]
    else:
      error("unknown defTile arms: $#" % $b[0], b)
  let pat = patopt.get
  let code = codeopt.get

  result = quote do:
    proc `name`[B, C](`bufid`: var B, `ctxid`: var C): bool =
      tilegen(`pat`, `code`, `matchopt`)
macro tilegen*(pat: untyped, code: untyped, match: untyped): untyped =
  let ctxid = ident"ctx"
  let patlen = pat.len
  var patcond = newStmtList()
  var patgetsrc = newSeq[string]()
  for i, p in pat:
    patcond.add(parseExpr("""
if code$1.kind != $2:
  return false
""" % [$(i+1), p.repr]))
    patgetsrc.add("let code$# = ctx[ctx.pos + $#]" % [$(i+1), $i])
  for m in match:
    patcond.add(parseExpr("""
if not ($#):
  return false
""" % [m.repr]))
  let patget = parseStmt(patgetsrc.join("\n"))

  var codebody = newStmtList()
  for c in code:
    if c.kind == nnkCall and $c[0] == "CODEBLOCK":
      codebody.add(c[1])
    else:
      codebody.add(parseExpr("""
buf.add($#)
""" % c.repr))

  result = quote do:
    if ctx.pos+`patlen` <= ctx.len:
      `patget`
      `patcond`
      `codebody`
      `ctxid`.pos += `patlen`
      return true
    else:
      return false
  # echo result.repr

template addCode*(code: untyped) =
  buf.add(code)

macro defTileset*(name: untyped, body: untyped): untyped =
  let bufid = ident"buf"
  let ctxid = ident"ctx"
  let procbody = newStmtList()
  for b in body:
    procbody.add(quote do:
      if `b`(`bufid`, `ctxid`):
        return
    )
  result = quote do:
    proc `name`*[B, C](`bufid`: var B, `ctxid`: var C) = `procbody`
  # echo result.repr
