
import fexpr_core, marking
import newpassmacro

import tables

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
    fexpr.setexpr.value.markeffect.moved = true
    fexpr.markeffect = newMarkingEffect()
  of internalFieldAccess:
    scope.inferEffect(fexpr[1])
    if not fexpr[1].markeffect.fieldbody.hasKey(name(fexpr[2])):
      fexpr[1].markeffect.fieldbody[name(fexpr[2])] = newMarkingEffect()
    fexpr.markeffect = fexpr[1].markeffect.fieldbody[name(fexpr[2])]
  else:
    fexpr.markeffect = newMarkingEffect()

proc applyMarkingEffect*(scope: Scope, dst: MarkingEffect, src: MarkingEffect) =
  dst.moved = src.moved
  for key, value in src.fieldbody:
    if not dst.fieldbody.hasKey(key):
      dst.fieldbody[key] = newMarkingEffect()
    scope.applyMarkingEffect(dst.fieldbody[key], value)
    
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
    if fexpr.len != 0:
      fexpr.markeffect = fexpr[^1].markeffect
    else:
      fexpr.markeffect = newMarkingEffect()
  elif fexpr.kind == fexprSymbol:
    if fexpr.symbol.fexpr.hasMarkEffect:
      fexpr.markeffect = fexpr.symbol.fexpr.markeffect
    else:
      fexpr.markeffect = newMarkingEffect()
  elif fexpr.isNormalFuncCall and fexpr[0].kind == fexprSymbol:
    scope.inferEffect(fexpr[1])
    for i, argeff in fexpr[0].symbol.fexpr.fneffect.argeffs:
      scope.applymarkingEffect(fexpr[1][i].markeffect, argeff)
    fexpr.markeffect = newMarkingEffect()
  elif fexpr.isGenericsFuncCall and fexpr[0].kind == fexprSymbol:
    scope.inferEffect(fexpr[2])
    for i, argeff in fexpr[0].symbol.fexpr.fneffect.argeffs:
      scope.applymarkingEffect(fexpr[2][i].markeffect, argeff)
  # elif fexpr.isInfixFuncCall:
  #   discard # TODO:
    fexpr.markeffect = newMarkingEffect()
  else:
    fexpr.markeffect = newMarkingEffect()

proc inferFnEffectPass*(scope: Scope, fexpr: var FExpr): bool =
  if fexpr.hasInternalMark and fexpr.internalMark == internalDefn and not fexpr.internalPragma.inline:
    for argdef in fexpr.defn.args:
      argdef[0].markeffect = newMarkingEffect()
    scope.inferEffect(fexpr.defn.body)
    fexpr.fneffect = FnEffect(argeffs: @[])
    for argdef in fexpr.defn.args:
      fexpr.fneffect.argeffs.add(argdef[0].markeffect)
  return true

proc applyEffect*(scope: Scope, marking: Marking, markeff: MarkingEffect) =
  if markeff.moved:
    marking.owned = false
  for key, value in markeff.fieldbody:
    scope.applyEffect(marking.fieldbody[key], value)

proc earlySetDestruct*(scope: Scope, fexpr: var FExpr): bool =
  if fexpr.hasInternalMark and fexpr.internalMark == internalSet:
    if fexpr.setexpr.dst.hasMarking:
      var body = fblock(fexpr.span)
      if fexpr.setexpr.dst.marking.owned and scope.isDestructable(fexpr.setexpr.dst.typ):
        var destcall = fexpr.span.quoteFExpr("destruct(`embed)", [fexpr.setexpr.dst])
        scope.rootPass(destcall)
        body.addSon(destcall)
        body.addSon(fexpr)
        returnFrom(fexpr.setexpr.dst.marking)
        
      if fexpr.setexpr.value.hasMarking:
        fexpr.setexpr.dst.marking.moveFrom(fexpr.setexpr.value.marking)
      else:
        fexpr.setexpr.dst.marking = newMarking(scope, fexpr.setexpr.dst.typ)

      if body.len != 0:
        fexpr = body
        scope.rootPass(fexpr)
        
  return true

proc applyEffectPass*(scope: Scope, fexpr: var FExpr): bool =
  thruInternal(fexpr)
  if fexpr.isInfixFuncCall:
    discard
  elif fexpr.isFuncCall:
    let args = if fexpr.isNormalFuncCall:
                 fexpr[1]
               else:
                 fexpr[2]
    if fexpr[0].symbol.fexpr.hasFnEffect:
      for i, argeff in fexpr[0].symbol.fexpr.fneffect.argeffs:
        if args[i].hasMarking:
          scope.applyEffect(args[i].marking, argeff)
      
  return true
