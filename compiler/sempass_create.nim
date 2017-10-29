
import sast
import semtree
import sempass

import tables
import strutils, sequtils
import options

type
  CreateError* = object of Exception

proc createBody*(scope: SemScope, body: SExpr): seq[SemExpr]

proc parseTypeAnnotation*(sexpr: SExpr, isAnnot = true): tuple[argtypes: seq[SExpr], rettype: SExpr, body: SExpr] =
  var argtypes = newSeq[SExpr]()
  var rettype: SExpr
  let body = sexpr.last
  sexpr.rest.each(arg):
    if $arg.first == "->":
      rettype = arg.rest.first
      break
    elif isAnnot and arg.rest.kind == sexprNil:
      break
    else:
      argtypes.add(arg.first)
  if rettype == nil:
    rettype = newSIdent(sexpr.span, "Void")
  return (argtypes, rettype, body)

proc createExpr*(scope: SemScope, sexpr: SExpr): SemExpr =
  case sexpr.kind
  of sexprList:
    result = SemExpr(
      typ: none(SemType),
      kind: seFuncCall,
      fn: SemType(kind: stIdent, idscope: scope, idname: scope.createExpr(sexpr.first)),
      args: scope.createBody(sexpr.rest)
    )
  of sexprIdent:
    result = SemExpr(typ: none(SemType), kind: seIdent, nameid: $sexpr)
  of sexprInt:
    result = SemExpr(typ: none(SemType), kind: seInt, intval: sexpr.intval)
  of sexprString:
    result = SemExpr(typ: none(SemType), kind: seString, strval: sexpr.strval)
  else:
    sexpr.error("$# is not expression." % $sexpr.kind)

proc createBody*(scope: SemScope, body: SExpr): seq[SemExpr] =
  result = @[]
  for b in body:
    result.add(scope.createExpr(b))

proc createDefn*(scope: SemScope, sexpr: SExpr, argtypes: seq[SExpr], rettype: SExpr) =
  let fname = sexpr.rest.first
  let fargs = sexpr.rest.rest.first
  let fbody = sexpr.rest.rest.rest
  let ftype = FuncType(
    argtypes: argtypes.mapIt(SemType(kind: stIdent, idscope: scope, idname: scope.createExpr(it))),
    returntype: SemType(kind: stIdent, idscope: scope, idname: scope.createExpr(rettype)),
  )
  let fdecl = SemDecl(kind: sdFunc, funcname: $fname, functype: ftype, funcargs: fargs, funcbody: scope.createBody(fbody))
  let status = scope.top.addProc(fdecl.toProcIdentDecl)
  if not status:
    sexpr.error("redefinition proc $#" % $fname)
  
proc createCFunc*(scope: SemScope, sexpr: SExpr, argtypes: seq[SExpr], rettype: SExpr) = discard

proc createTypeAnnot*(scope: SemScope, sexpr: SExpr) =
  let (argtypes, rettype, body) = parseTypeAnnotation(sexpr)
  case $body.first
  of "defn":
    scope.createDefn(body, argtypes, rettype)
  of "c-func":
    scope.createCFunc(body, argtypes, rettype)

proc createTopVar*(scope: SemScope, sexpr: SExpr) = discard
proc createDefStruct*(scope: SemScope, sexpr: SExpr) = discard
proc createCType*(scope: SemScope, sexpr: SExpr) = discard

proc createTopDecl*(scope: SemScope, sexpr: SExpr) =
  case $sexpr.first
  of "defn", "c-func":
    sexpr.error("$# requires type annotation." % $sexpr.first)
  of "var":
    scope.createTopVar(sexpr)
  of "defstruct":
    scope.createDefStruct(sexpr)
  of "c-type":
    scope.createCType(sexpr)
  # TODO: generics
  # of "^":
  #   scope.createGenerics(sexpr)
  of ":":
    scope.createTypeAnnot(sexpr)

proc createTop*(scope: SemScope, sexpr: SExpr) =
  case sexpr.kind
  of sexprList:
    scope.createTopDecl(sexpr)
  else:
    sexpr.error("couldn't eval $# at toplevel expression." % $sexpr.kind)

proc createModuleFromSExpr*(ctx: SemPassContext, modulename: string, topsexprs: seq[SExpr]) =
  let scope = newSemScope()
  for sexpr in topsexprs:
    scope.createTop(sexpr)
  ctx.modules[ScopeIdent(name: modulename)] = scope
