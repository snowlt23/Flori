
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
      sexpr: sexpr,
      typ: none(SemSym),
      kind: seFuncCall,
      fn: scope.semsym(scope.createExpr(sexpr.first)),
      args: scope.createBody(sexpr.rest)
    )
  of sexprIdent:
    result = SemExpr(sexpr: sexpr, typ: none(SemSym), kind: seIdent, nameid: $sexpr)
  of sexprInt:
    result = SemExpr(sexpr: sexpr, typ: some(scope.semsym(semident("Int32"))), kind: seInt, intval: sexpr.intval)
  of sexprString:
    result = SemExpr(sexpr: sexpr, typ: some(scope.semsym(semident("CString"))), kind: seString, strval: sexpr.strval)
  else:
    sexpr.error("$# is not expression." % $sexpr.kind)

proc createBody*(scope: SemScope, body: SExpr): seq[SemExpr] =
  result = @[]
  for b in body:
    result.add(scope.createExpr(b))

proc toFuncType*(scope: SemScope, argtypes: seq[SExpr], rettype: SExpr): FuncType =
  FuncType(
    argtypes: argtypes.mapIt(scope.semsym(scope.createExpr(it))),
    returntype: scope.semsym(scope.createExpr(rettype)),
  )

proc createDefn*(scope: SemScope, sexpr: SExpr, argtypes: seq[SExpr], rettype: SExpr) =
  let fname = sexpr.rest.first
  let fargs = sexpr.rest.rest.first
  let fbody = sexpr.rest.rest.rest
  let ftype = scope.toFuncType(argtypes, rettype)
  let fdecl = SemFunc(sexpr: sexpr, kind: sfFunc, funcname: $fname, functype: ftype, funcargs: scope.createBody(fargs), funcbody: scope.createBody(fbody))
  let status = scope.top.addProc(fdecl.toProcIdentDecl)
  if not status:
    sexpr.error("redefinition func $#" % $fname)
  
proc createCFunc*(scope: SemScope, sexpr: SExpr, argtypes: seq[SExpr], rettype: SExpr) =
  let f = sexpr.rest.first
  let nameopt = sexpr.getAttr("pattern")
  let headeropt = sexpr.getAttr("header")
  let nodecl = sexpr.hasAttr("nodecl")
  let name = if nameopt.isSome:
               nameopt.get.strval
             else:
               $f
  let header = if nodecl:
                 none(string)
               else:
                 some(headeropt.get.strval)
  let ftype = scope.toFuncType(argtypes, rettype)
  let fdecl = SemFunc(sexpr: sexpr, kind: sfCFunc, cfuncname: name, cfunctype: ftype, cfuncheader: header)
  let status = scope.top.addProc(fdecl.toProcIdentDecl)
  if not status:
    sexpr.error("redefinition C func $#" % $f)

proc createTypeAnnot*(scope: SemScope, sexpr: SExpr) =
  let (argtypes, rettype, body) = parseTypeAnnotation(sexpr)
  case $body.first
  of "defn":
    scope.createDefn(body, argtypes, rettype)
  of "c-func":
    scope.createCFunc(body, argtypes, rettype)

# TODO:
proc createTopVar*(scope: SemScope, sexpr: SExpr) = discard
# TODO:
proc createDefStruct*(scope: SemScope, sexpr: SExpr) = discard

proc createCType*(scope: SemScope, sexpr: SExpr) =
  let t = sexpr.rest.first
  let nameopt = sexpr.getAttr("name")
  let headeropt = sexpr.getAttr("header")
  let nodecl = sexpr.hasAttr("nodecl")
  let name = if nameopt.isSome:
               nameopt.get.strval
             else:
               $t
  let header = if nodecl:
                 none(string)
               else:
                 some(headeropt.get.strval)
  let status = scope.top.addType(TypeIdent(name: $t), SemType(sexpr: sexpr, kind: stCType, ctypename: name, ctypeheader: header))
  if not status:
    sexpr.error("redefinition C type $#" % $t)

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
  else:
    scope.top.addTopExpr(scope.createExpr(sexpr))

proc createTop*(scope: SemScope, sexpr: SExpr) =
  case sexpr.kind
  of sexprList:
    scope.createTopDecl(sexpr)
  else:
    scope.top.addTopExpr(scope.createExpr(sexpr))

proc createModuleFromSExpr*(ctx: SemPassContext, modulename: string, topsexprs: seq[SExpr]) =
  let scope = newSemScope()
  for sexpr in topsexprs:
    scope.createTop(sexpr)
  ctx.modules[ScopeIdent(name: modulename)] = scope
