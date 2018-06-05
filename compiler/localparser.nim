
import image, parser, fexpr, scope

import strutils, sequtils
import options
import macros

type
  Defn* = object
    name*: int
    generics*: Option[int]
    argdecls*: int
    retpre*: Option[int]
    ret*: Option[int]
    retgen*: Option[int]
    pragma*: Option[int]
    body*: Option[int]
  Deftype* = object
    name*: int
    generics*: Option[int]
    pragma*: Option[int]
    body*: Option[int]
  IfExpr* = object
    elifbranches*: seq[(int, int)]
    elsebody*: Option[int]

proc parseDefn*(fexpr: FExpr): Defn =
  var pos = 1

  # name
  if pos >= fexpr.len or fexpr[pos].kind notin {fexprIdent, fexprSymbol, fexprQuote}:
    fexpr.error("fn syntax expect function name.")
  result.name = pos
  pos.inc

  # generics
  if fexpr[pos].kind == fexprArray:
    result.generics = some(pos)
    pos.inc

  # args
  if pos >= fexpr.len or fexpr[pos].kind != fexprList:
    fexpr.error("fn syntax expect function arguments.")
  result.argdecls = pos
  pos.inc

  # ret ref
  if pos < fexpr.len and ($fexpr[pos] == "ref"):
    result.retpre = some(pos)
    pos.inc

  # ret
  if pos < fexpr.len and fexpr[pos].kind in {fexprIdent, fexprSymbol}:
    result.ret = some(pos)
    pos.inc

  # ret generics
  if pos < fexpr.len and fexpr[pos].kind == fexprArray:
    result.retgen = some(pos)
    pos.inc

  # pragma
  if pos < fexpr.len and $fexpr[pos] == "$":
    pos.inc
    if pos >= fexpr.len or fexpr[pos].kind != fexprArray:
      fexpr.error("fn syntax expect function pragma after `$.")
    result.pragma = some(pos)
    pos.inc

  # body
  if pos < fexpr.len and fexpr[pos].kind == fexprBlock:
    result.body = some(pos)
    pos.inc

proc parseDeftype*(fexpr: FExpr): Deftype =
  var pos = 1

  # name
  if pos >= fexpr.len or fexpr[pos].kind notin {fexprIdent, fexprSymbol}:
    fexpr.error("type syntax expect name.")
  result.name = pos
  pos.inc

  # generics
  if pos < fexpr.len and fexpr[pos].kind == fexprArray:
    result.generics = some(pos)
    pos.inc

  # pragma
  if pos < fexpr.len and $fexpr[pos] == "$":
    pos.inc
    if pos >= fexpr.len or fexpr[pos].kind != fexprArray:
      fexpr.error("type syntax expect pragma after `$.")
    result.pragma = some(pos)
    pos.inc

  # body
  if pos < fexpr.len and fexpr[pos].kind == fexprBlock:
    result.body = some(pos)
    pos.inc

proc parseIf*(fexpr: FExpr): IfExpr =
  if fexpr.len < 3:
    fexpr.error("if expression require greater than 3 arguments.")
  result.elifbranches = @[]
  
  var pos = 1

  if fexpr[pos].kind != fexprList or fexpr[pos].len != 1:
    fexpr[pos].error("if cond should be single FList.")
  if fexpr[pos+1].kind != fexprBlock:
    fexpr[pos+1].error("if body should be FBlock.")
  result.elifbranches.add((pos, pos+1))
  pos += 2

  while fexpr.len > pos:
    if $fexpr[pos] == "elif":
      pos.inc
      if fexpr[pos].kind != fexprList or fexpr[pos].len != 1:
        fexpr[pos].error("elif cond should be single FList.")
      let cond = pos
      pos.inc
      if fexpr[pos].kind != fexprBlock:
        fexpr[pos].error("elif body should be FBlock.")
      let body = pos
      pos.inc
      result.elifbranches.add((cond, body))
    elif $fexpr[pos] == "else":
      pos.inc
      if fexpr[pos].kind != fexprBlock:
        fexpr[pos].error("else body should be FBlock.")
      let body = pos
      pos.inc
      result.elsebody = some(body)
      break
    else:
      fexpr[pos].error("unknown if branch name: $#" % $fexpr[pos])
