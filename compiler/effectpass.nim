
import fexpr_core, marking
import newpassmacro

import tables
import strutils, sequtils

proc newMarkingEffect*(): MarkingEffect =
  new result
  result.moved = false
  result.fieldbody = initTable[Name, MarkingEffect]()

proc inferEffect*(scope: Scope, fexpr: var FExpr)

proc inferInternalEffect*(scope: Scope, fexpr: var FExpr) =
  case fexpr.internalMark
  of internalDef:
    scope.inferEffect(fexpr.defexpr.value)
    fexpr.defexpr.name.symbol.fexpr.markeffect = newMarkingEffect()
    fexpr.markeffect = newMarkingEffect()
  of internalVar:
    fexpr[1].markeffect = newMarkingEffect()
    fexpr.markeffect = newMarkingEffect()
  of internalSet:
    scope.inferEffect(fexpr.setexpr.dst)
    scope.inferEffect(fexpr.setexpr.value)
    # echo fexpr, ":", scope.isResource(fexpr.setexpr.value.typ)
    if scope.isResource(fexpr.setexpr.value.typ):
      fexpr.setexpr.value.markeffect.moved = true
    fexpr.markeffect = newMarkingEffect()
  of internalInit:
    for e in fexpr[2].mitems:
      scope.inferEffect(e)
      if scope.isResource(e.typ):
        e.markeffect.moved = true
    fexpr.markeffect = newMarkingEffect()
  of internalFieldAccess:
    scope.inferEffect(fexpr[1])
    if not fexpr[1].markeffect.fieldbody.hasKey(name(fexpr[2])):
      fexpr[1].markeffect.fieldbody[name(fexpr[2])] = newMarkingEffect()
    fexpr.markeffect = fexpr[1].markeffect.fieldbody[name(fexpr[2])]
  else:
    fexpr.markeffect = newMarkingEffect()

proc applyMarkingEffect*(scope: Scope, dst: MarkingEffect, src: MarkingEffect) =
  if src.moved:
    dst.moved = true
  # for key, value in src.fieldbody:
  #   if not dst.fieldbody.hasKey(key):
  #     dst.fieldbody[key] = newMarkingEffect()
  #   scope.applyMarkingEffect(dst.fieldbody[key], value)

proc inferFnEffect*(scope: Scope, fexpr: var FExpr) =
  # if fexpr.hasInternalMark and fexpr.internalMark == internalDefn and not fexpr.internalPragma.inline:
  for argdef in fexpr.defn.args:
    argdef[0].symbol.fexpr.markeffect = newMarkingEffect()
  scope.inferEffect(fexpr.defn.body)
  fexpr.fneffect = FnEffect(argeffs: @[])
  for argdef in fexpr.defn.args:
    fexpr.fneffect.argeffs.add(argdef[0].symbol.fexpr.markeffect)
    
proc inferEffect*(scope: Scope, fexpr: var FExpr) =
  if fexpr.hasInternalMark:
    scope.inferInternalEffect(fexpr)
  elif fexpr.kind in {fexprArray, fexprList}:
    for son in fexpr.mitems:
      scope.inferEffect(son)
    if fexpr.len != 0:
      fexpr.markeffect = fexpr[0].markeffect
    else:
      fexpr.markeffect = newMarkingEffect()
  elif fexpr.kind == fexprBlock:
    for son in fexpr.mitems:
      scope.inferEffect(son)
    if fexpr.len != 0 and fexpr[^1].hasMarkEffect:
      fexpr.markeffect = fexpr[^1].markeffect
    else:
      fexpr.markeffect = newMarkingEffect()
  elif fexpr.kind == fexprSymbol:
    if fexpr.symbol.fexpr.hasMarkEffect:
      fexpr.markeffect = fexpr.symbol.fexpr.markeffect
    else:
      fexpr.markeffect = newMarkingEffect()
  elif fexpr.isNormalFuncCall and fexpr[0].kind == fexprSymbol and not fexpr[0].symbol.fexpr.defn.isGenerics:
    scope.inferFnEffect(fexpr[0].symbol.fexpr)
    scope.inferEffect(fexpr[1])
    for i, argeff in fexpr[0].symbol.fexpr.fneffect.argeffs:
      scope.applymarkingEffect(fexpr[1][i].markeffect, argeff)
    fexpr.markeffect = newMarkingEffect()
  elif fexpr.isGenericsFuncCall and fexpr[0].kind == fexprSymbol and not fexpr[0].symbol.fexpr.defn.isGenerics:
    scope.inferFnEffect(fexpr[0].symbol.fexpr)
    scope.inferEffect(fexpr[2])
    for i, argeff in fexpr[0].symbol.fexpr.fneffect.argeffs:
      scope.applymarkingEffect(fexpr[2][i].markeffect, argeff)
    fexpr.markeffect = newMarkingEffect()
  elif fexpr.isInfixFuncCall and fexpr[0].kind == fexprSymbol and not fexpr[0].symbol.fexpr.defn.isGenerics:
    scope.inferFnEffect(fexpr[0].symbol.fexpr)
    scope.inferEffect(fexpr[1])
    scope.inferEffect(fexpr[2])
    scope.applyMarkingEffect(fexpr[1].markeffect, fexpr[0].symbol.fexpr.fneffect.argeffs[0])
    scope.applyMarkingEffect(fexpr[2].markeffect, fexpr[0].symbol.fexpr.fneffect.argeffs[1])
    fexpr.markeffect = newMarkingEffect()
  else:
    fexpr.markeffect = newMarkingEffect()

proc inferEffectPass*(scope: Scope, fexpr: var FExpr): bool =
  scope.inferEffect(fexpr)
  return true

proc inferFnEffectPass*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  if fexpr.isFuncCall:
    if fexpr[0].kind != fexprSymbol:
      return true
    if not fexpr[0].symbol.fexpr.hasFnEffect:
      for argdef in fexpr[0].symbol.fexpr.defn.args:
        argdef[0].symbol.fexpr.markeffect = newMarkingEffect()
      scope.inferEffect(fexpr[0].symbol.fexpr.defn.body)
      fexpr[0].symbol.fexpr.fneffect = FnEffect(argeffs: @[])
      for argdef in fexpr[0].symbol.fexpr.defn.args:
        fexpr[0].symbol.fexpr.fneffect.argeffs.add(argdef[0].symbol.fexpr.markeffect)
  return true

proc applyEffect*(scope: Scope, marking: Marking, markeff: MarkingEffect): bool =
  if markeff.moved:
    if not marking.owned:
      return false
    marking.owned = false
  for key, value in markeff.fieldbody:
    if not scope.applyEffect(marking.fieldbody[key], value):
      return false
  return true

proc earlySetDestruct*(scope: Scope, fexpr: var FExpr): bool =
  if fexpr.hasInternalMark and fexpr.internalMark == internalSet:
    var body = fblock(fexpr.span)
    if scope.isDestructable(fexpr.setexpr.dst.typ) and not scope.nodestruct:
      var destcall = fexpr.span.quoteFExpr("destruct(`embed)", [fexpr.setexpr.dst])
      scope.rootPass(destcall)
      body.addSon(destcall)
      body.addSon(fexpr)

    if body.len != 0:
      fexpr = body
      scope.rootPass(fexpr)
        
  return true
