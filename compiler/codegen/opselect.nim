
import macros
import strutils, sequtils
import options

import tacode, x86code

macro defTile*(name: untyped, body: untyped): untyped =
  let bufid = ident"buf"
  let ctxid = ident"ctx"

  var patopt = none(NimNode)
  var codeopt = none(NimNode)
  for b in body:
    b.expectKind(nnkCall)
    if $b[0] == "PATTERN":
      patopt = some(b[1])
    elif $b[0] == "CODE":
      codeopt = some(b[1])
    else:
      error("unknown defTile arms: $#" % $b[0], b)
  let pat = patopt.get
  let code = codeopt.get
  
  result = quote do:
    proc `name`[B, C](`bufid`: var B, `ctxid`: var C): bool =
      tilegen(`pat`, `code`)
macro tilegen*(pat: untyped, code: untyped): untyped =
  let ctxid = ident"ctx"
  let patlen = pat.len
  var patcondsrc = newSeq[string]()
  var patgetsrc = newSeq[string]()
  for i, p in pat:
    patcondsrc.add("ctx.pos+$1 < ctx.len and ctx[ctx.pos+$1].kind == $2" % [$i, p.repr])
    patgetsrc.add("let code$# = ctx[ctx.pos + $#]" % [$(i+1), $i])
  let patcond = parseExpr(patcondsrc.join(" and "))
  let patget = parseStmt(patgetsrc.join("\n"))
  
  var codebody = newStmtList()
  for c in code:
    codebody.add(parseExpr("buf.add($#)" % c.repr))
    
  result = quote do:
    if `patcond`:
      `patget`
      `codebody`
      `ctxid`.pos += `patlen`
      return true
    else:
      return false
  echo result.repr

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
  echo result.repr

defTile tileX86Add:
  PATTERN:
    TACodeKind.Add
  CODE:
    initX86CodeAVar(code1.add.name, 4)
    initX86CodeMov(initX86AtomTemp(code1.add.name), toX86Atom(code1.add.left))
    initX86CodeAdd(initX86AtomTemp(code1.add.name), toX86Atom(code1.add.right))

defTileset x86Tiling:
  tileX86Add

type TileTAFn* = object
  codes*: seq[TACode]
  pos*: int
proc `[]`*(fn: TileTAFn, i: int): TACode = fn.codes[i]
proc len*(fn: TileTAFn): int = fn.codes.len

proc tiling*(buf: var X86Context, ctx: TAContext) =
  for fn in ctx.fns:
    var fn = TileTAFn(codes: fn.body)
    while fn.pos < fn.len:
      x86Tiling(buf, fn)
