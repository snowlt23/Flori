
import types, fexpr, scope, metadata
import ccodegen

import strutils, sequtils
import tables
import options

proc newMarking*(scope: Scope, typesym: Symbol): Marking =
  new result
  result.scope = scope
  result.typesym = typesym
  result.owned = true
  result.dynamic = dynUnique
  result.fieldbody = initTable[Name, Marking]()
  if typesym.fexpr.hasDeftype:
    for b in typesym.fexpr.deftype.body:
      result.fieldbody[name(b[0])] = newMarking(scope, b[1].symbol)

proc convertToShareWrap*(sym: Symbol) =
  assert(sym.kind == symbolDynamic)
  let opt = sym.scope.getDecl(name("ShareWrap"))
  if opt.isNone:
    sym.fexpr.error("undeclared ShareWrap type, please import core library.")  
  let sw = opt.get
  
  let expanded = sw.fexpr.copy
  expanded.deftype.generics[0].symbol.instance = some(sym.wrapped)
  expanded.deftype.generics[0].symbol = sym.wrapped
  let manglingname = name(codegenMangling(expanded.deftype.name.symbol, @[sym.wrapped], @[]))
  let specopt = sym.scope.getDecl(manglingname)
  if specopt.isSome:
    sym.wrapped = specopt.get
    return
    
  expanded.deftype.body[0][1].symbol = expanded.deftype.body[0][1].symbol.instance.get
  let tsym = sym.scope.symbol(expanded.deftype.name.symbol.name, symbolTypeGenerics, expanded)
  tsym.types = @[sym.wrapped]
  let fsym = fsymbol(sym.fexpr.span, tsym)
  expanded.deftype.name = fsym

  discard sym.scope.addDecl(manglingname, tsym)
  for i in countdown(sym.scope.ctx.globaltoplevels.len-1, 0):
    if sym.scope.ctx.globaltoplevels[i].hasDeftype:
      sym.scope.ctx.globaltoplevels.insert(expanded, i)
      break

  echo expanded

  sym.wrapped = tsym

proc returnFrom*(frm: Marking) =
  if frm.dynamic == dynBorrow:
    returnFrom(frm.origin)
  else:
    if frm.typesym.kind == symbolDynamic:
      frm.typesym.marking.get.owned = false
      frm.typesym.marking.get.dynamic = dynShare
      convertToShareWrap(frm.typesym)
    frm.owned = false
    frm.dynamic = dynShare
    for key, value in frm.fieldbody:
      returnFrom(value)
      
proc getFrom*(mark: Marking, frm: Marking) =
  if frm.dynamic == dynUnique and mark.scope.level >= frm.scope.level:
    frm.owned = false
    frm.dynamic = dynShare
    if frm.typesym.kind == symbolDynamic:
      convertToShareWrap(frm.typesym)
    mark.owned = true
    mark.dynamic = dynUnique
  elif frm.dynamic == dynBorrow and mark.scope.level >= frm.scope.level:
    frm.origin.owned = false
    frm.origin.dynamic = dynShare
    if frm.origin.typesym.kind == symbolDynamic:
      convertToShareWrap(frm.origin.typesym)
    mark.owned = true
    mark.dynamic = dynUnique
  elif frm.dynamic == dynUnique:
    mark.owned = false
    mark.dynamic = dynBorrow
    mark.origin = frm
  elif frm.dynamic == dynBorrow:
    mark.owned = false
    mark.dynamic = dynBorrow
    mark.origin = frm.origin
  else:
    mark.scope = frm.scope
    mark.owned = frm.owned
    mark.dynamic = frm.dynamic
    mark.origin = frm.origin

proc debugDynamicType*(sym: Symbol): string =
  if sym.kind == symbolDynamic:
    "dynamic " & debugDynamicType(sym.wrapped) & "=" & $sym.marking.get.dynamic
  elif sym.kind == symbolTypeGenerics:
    $sym.name & "[" & sym.types.mapIt(debugDynamicType(it)).join(", ") & "]"
  else:
    $sym
  
proc debugMarking*(marking: Marking, indent: int): string =
  result = $marking.dynamic & ":" & debugDynamicType(marking.typesym)
  if marking.fieldbody.len != 0:
    result &= "{\n"
    for key, value in marking.fieldbody:
      result &= " ".repeat(indent+2) & $key & ":" & debugMarking(value, indent+2) & "\n"
    result &= " ".repeat(indent) & "}"

proc copy*(mark: Marking): Marking =
  result = newMarking(mark.scope, mark.typesym)
  result.owned = mark.owned
  result.origin = mark.origin
  result.dynamic = mark.dynamic
  for key, value in result.fieldbody.mpairs:
    value = mark.fieldbody[key].copy
