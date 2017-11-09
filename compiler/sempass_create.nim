
import ast
import semtree
import sempass

import tables
import strutils, sequtils
import options

type
  CreateError* = object of Exception

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
          kind: seFuncCall,
          fn: scope.semsym(scope.createExpr(fexpr[1])),
          args: @[left] & right
        )
      of fexprList: # call
        if fexpr.len >= 4 and fexpr[2].kind == fexprSpecial: # infix
          let body = scope.createBody(fexpr[1])
          let call = SemExpr(
            scope: scope,
            fexpr: fexpr,
            typ: none(SemSym),
            kind: seFuncCall,
            fn: scope.semsym(scope.createExpr(fexpr[0])),
            args: body
          )
          let right = scope.createExpr(fexpr[3..^1])
          result = SemExpr(
            scope: scope,
            fexpr: fexpr,
            typ: none(SemSym),
            kind: seFuncCall,
            fn: scope.semsym(scope.createExpr(fexpr[2])),
            args: @[call] & right
          )
        else:
          let body = scope.createBody(fexpr[1..^1])
          let call = SemExpr(
            scope: scope,
            fexpr: fexpr,
            typ: none(SemSym),
            kind: seFuncCall,
            fn: scope.semsym(scope.createExpr(fexpr[0])),
            args: if body.len == 1: body[0].body else: body
          )
          result = call
      else:
        result = SemExpr(
          scope: scope,
          fexpr: fexpr,
          typ: none(SemSym),
          kind: seFuncCall,
          fn: scope.semsym(scope.createExpr(fexpr[0])),
          args: scope.createBody(fexpr[1..^1])
        )
    else:
      result = scope.createExpr(fexpr[0])
  of fexprList:
    result = SemExpr(scope: scope, fexpr: fexpr, typ: some(scope.semsym(scope.semident("Void"))), kind: seList, body: scope.createBody(fexpr))
  of fexprBlock:
    result = SemExpr(scope: scope, fexpr: fexpr, typ: some(scope.semsym(scope.semident("Void"))), kind: seBlock, body: scope.createBody(fexpr))
  of fexprIdent, fexprSpecial:
    result = SemExpr(scope: scope, fexpr: fexpr, typ: none(SemSym), kind: seIdent, idname: $fexpr)
  of fexprIntLit:
    result = SemExpr(scope: scope, fexpr: fexpr, typ: some(scope.semsym(scope.semident("Int32"))), kind: seInt, intval: fexpr.intval)
  of fexprStrLit:
    result = SemExpr(scope: scope, fexpr: fexpr, typ: some(scope.semsym(scope.semident("CString"))), kind: seString, strval: fexpr.strval)
  else:
    fexpr.error("$# is not expression." % $fexpr.kind)

proc createBody*(scope: SemScope, body: FExpr): seq[SemExpr] =
  result = @[]
  for b in body:
    result.add(scope.createExpr(b))

proc toFuncType*(scope: SemScope, argtypes: seq[FExpr], rettype: FExpr): FuncType =
  FuncType(
    argtypes: argtypes.mapIt(scope.semsym(scope.createExpr(it))),
    returntype: scope.semsym(scope.createExpr(rettype)),
  )

proc createFn*(scope: SemScope, fexpr: FExpr) =
  let scope = scope.extendSemScope()
  let fname = fexpr[1]
  let fargs = fseq(fexpr.span, fexpr[2].sons.mapIt(it[0]))
  let argtypes = fexpr[2].sons.mapIt(it[1])
  let rettype = fexpr[3]
  let fbody = fexpr[4]
  let ftype = scope.toFuncType(argtypes, rettype)
  let fdecl = SemFunc(scope: scope, fexpr: fexpr, kind: sfFunc, funcname: $fname, functype: ftype, funcargs: scope.createBody(fargs), funcbody: scope.createBody(fbody))
  let status = scope.top.addProc(fdecl.toProcIdentDecl)
  if not status:
    fexpr.error("redefinition func $#" % $fname)
  
proc createCFunc*(scope: SemScope, fexpr: FExpr) =
  let f = fexpr[1]
  let argtypes = fexpr[2].sons
  let rettype = fexpr[3]
  let name = fexpr[4][0].strval
  let headerstr = fexpr[4][1]
  let param = if fexpr[4].len >= 3:
                $fexpr[4][2]
              else:
                "none"
  let header = if $headerstr == "nodecl":
                 none(string)
               else:
                 some(headerstr.strval)
  let infix = if param == "infix": true else: false
  let ftype = scope.toFuncType(argtypes, rettype)
  let fdecl = SemFunc(
    scope: scope,
    fexpr: fexpr,
    kind: sfCFunc,
    cfuncname: name,
    cfunctype: ftype,
    cfuncheader: header,
    cfuncinfix: infix
  )
  let status = scope.top.addProc(ProcIdentDecl(name: $f, args: ftype.argtypes, value: fdecl))
  if not status:
    fexpr.error("redefinition C func $#" % $f)

proc createTopVar*(scope: SemScope, fexpr: FExpr) =
  fexpr.requireMinLen("var", 4)
  if fexpr[1].kind != fexprIdent:
    fexpr[1].error("variable name should be ident.")
  if $fexpr[2] != "=":
    fexpr[2].error("variable require '= symbol.")
  let name = fexpr[1]
  let value = fexpr[3..^1]
  let vardecl = SemExpr(
    fexpr: fexpr,
    scope: scope,
    typ: none(SemSym),
    kind: seVar,
    varname: $name,
    varvalue: scope.createExpr(value),
    vartoplevel: true
  )
  let status = scope.top.addVar(VarIdent(name: $name), vardecl)
  if not status:
    fexpr.error("redefinition $# variable." % $name)
# TODO:
proc createDefStruct*(scope: SemScope, fexpr: FExpr) = discard

proc createCType*(scope: SemScope, fexpr: FExpr) =
  fexpr.requireMinLen("var", 3)
  if fexpr[1].kind != fexprIdent:
    fexpr[1].error("ctype name should be ident.")
  if fexpr[2].kind != fexprArray:
    fexpr[2].error("ctype expect array expression.")
  let t = fexpr[1]
  let name = fexpr[2][0].strval
  let headerstr = fexpr[2][1]
  let header = if headerstr.kind == fexprStrLit:
                 some(headerstr.strval)
               else:
                 none(string)
  let status = scope.top.addType(TypeIdent(name: $t), SemType(scope: scope, fexpr: fexpr, kind: stCType, ctypename: name, ctypeheader: header))
  if not status:
    fexpr.error("redefinition C type $#" % $t)

proc createTopDecl*(scope: SemScope, fexpr: FExpr) =
  case $fexpr[0]
  of "fn":
    scope.createFn(fexpr)
  of "cfn":
    scope.createCFunc(fexpr)
  of "var":
    scope.createTopVar(fexpr)
  of "struct":
    scope.createDefStruct(fexpr)
  of "ctype":
    scope.createCType(fexpr)
  else:
    scope.top.addTopExpr(scope.createExpr(fexpr))

proc createTop*(scope: SemScope, fexpr: FExpr) =
  assert(fexpr.kind == fexprSeq)
  case fexpr[0].kind
  of fexprIdent:
    scope.createTopDecl(fexpr)
  else:
    scope.top.addTopExpr(scope.createExpr(fexpr))

proc createModuleFromSExpr*(ctx: SemPassContext, modulename: string, topfexprs: seq[FExpr]) =
  let scope = newSemScope(modulename)
  for sexpr in topfexprs:
    scope.createTop(sexpr)
  ctx.modules[ScopeIdent(name: modulename)] = scope
