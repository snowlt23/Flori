
import ast
import semtree

import tables
import strutils, sequtils
import options

proc requireMinLen*(fexpr: FExpr, name: string, len: int) =
  if fexpr.len < len:
    fexpr.error("$# is require $# len seq" % [name, $len])

proc createBody*(scope: SemScope, body: FExpr): seq[SemExpr]

proc createExpr*(scope: SemScope, fexpr: FExpr): SemExpr =
  case fexpr.kind
  of fexprSeq:
    if fexpr.len >= 2:
      case fexpr[1].kind
      of fexprSpecial: # infix
        let left = scope.createExpr(fexpr[0])
        let right = scope.createBody(fexpr[2..^1])
        result = SemExpr(
          scope: scope,
          fexpr: fexpr,
          typ: none(SemSym),
          kind: semCall,
          callfn: scope.createExpr(fexpr[1]),
          callargs: @[left] & right
        )
      of fexprList: # call
        if fexpr.len >= 4 and fexpr[2].kind == fexprSpecial: # infix
          let body = scope.createBody(fexpr[1])
          let call = SemExpr(
            scope: scope,
            fexpr: fexpr,
            typ: none(SemSym),
            kind: semCall,
            callfn: scope.createExpr(fexpr[0]),
            callargs: body
          )
          let right = scope.createExpr(fexpr[3..^1])
          result = SemExpr(
            scope: scope,
            fexpr: fexpr,
            typ: none(SemSym),
            kind: semCall,
            callfn: scope.createExpr(fexpr[2]),
            callargs: @[call] & right
          )
        else:
          let body = scope.createBody(fexpr[1..^1])
          let call = SemExpr(
            scope: scope,
            fexpr: fexpr,
            typ: none(SemSym),
            kind: semCall,
            callfn: scope.createExpr(fexpr[0]),
            callargs: if body.len == 1: body[0].body else: body
          )
          result = call
      else:
        result = SemExpr(
          scope: scope,
          fexpr: fexpr,
          typ: none(SemSym),
          kind: semCall,
          callfn: scope.createExpr(fexpr[0]),
          callargs: scope.createBody(fexpr[1..^1])
        )
    else:
      result = scope.createExpr(fexpr[0])
  of fexprList:
    result = SemExpr(scope: scope, fexpr: fexpr, typ: none(SemSym), kind: semList, body: scope.createBody(fexpr))
  of fexprBlock:
    result = SemExpr(scope: scope, fexpr: fexpr, typ: none(SemSym), kind: semBlock, body: scope.createBody(fexpr))
  of fexprIdent, fexprSpecial:
    result = SemExpr(scope: scope, fexpr: fexpr, typ: none(SemSym), kind: semIdent, idname: $fexpr)
  of fexprIntLit:
    result = SemExpr(scope: scope, fexpr: fexpr, typ: none(SemSym), kind: semIntLit, intval: fexpr.intval)
  of fexprStrLit:
    result = SemExpr(scope: scope, fexpr: fexpr, typ: none(SemSym), kind: semStrLit, strval: fexpr.strval)
  else:
    fexpr.error("$# is not expression." % $fexpr.kind)

proc createBody*(scope: SemScope, body: FExpr): seq[SemExpr] =
  result = @[]
  for b in body:
    result.add(scope.createExpr(b))

proc toFuncType*(scope: SemScope, argtypes: seq[FExpr], rettype: FExpr): FuncType =
  FuncType(
    argtypes: argtypes.mapIt(scope.createExpr(it)),
    returntype: argtypes.mapIt(scope.createExpr(rettype)),
  )

proc createTop*(scope: SemScope, fexpr: FExpr) =
  scope.top.addTopExpr(scope.createExpr(fexpr))

proc createModule*(ctx: SemContext, modulename: SemName, topfexprs: seq[FExpr]) =
  let scope = newSemScope(modulename)
  for sexpr in topfexprs:
    scope.createTop(sexpr)
  ctx.modules[modulename] = scope
