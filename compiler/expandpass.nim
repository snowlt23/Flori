
import parser, types, fexpr, scope, metadata, ctrc, effect
import passutils

import options
import strutils, sequtils

proc expandDeftype*(scope: Scope, fexpr: var FExpr, argtypes: seq[Symbol]): FExpr

proc applyInstance*(sym: Symbol, instance: Symbol) =
  if instance.kind in {symbolVar, symbolRef}:
    sym.applyInstance(instance.types[0])
  elif sym.kind == symbolGenerics:
    sym.instance = some(instance)
  elif sym.kind == symbolTypeGenerics:
    assert(sym.types.len == instance.types.len)
    sym.instance = some(instance)
    for i in 0..<sym.types.len:
      sym.types[i].applyInstance(instance.types[i])
  else:
    sym.instance = some(instance)

proc expandSymbol*(scope: Scope, sym: Symbol): Symbol =
  if sym.kind == symbolVar:
    result = scope.varsym(scope.expandSymbol(sym.types[0]))
  elif sym.kind == symbolRef:
    result = scope.refsym(scope.expandSymbol(sym.types[0]))
  elif sym.kind == symbolGenerics:
    if sym.instance.isNone:
      sym.fexpr.error("cannot instantiate $#." % $sym)
    result = sym.instance.get
  elif sym.kind == symbolTypeGenerics:
    var extypes = newSeq[Symbol]()
    for t in sym.types:
      extypes.add(scope.expandSymbol(t))
    result = scope.expandDeftype(sym.fexpr, extypes).symbol
  else:
    result = sym

proc expandType*(scope: Scope, fexpr: FExpr): FExpr =
  fexpr.assert(fexpr.kind == fexprSymbol)
  return fsymbol(fexpr.span, scope.expandSymbol(fexpr.symbol))

proc expandGenerics*(fexpr: FExpr): FExpr =
  result = farray(fexpr.span)
  for g in fexpr:
    g.assert(g.kind == fexprSymbol)
    if g.symbol.instance.isSome:
      result.addSon(fsymbol(g.span, g.symbol.instance.get))
    else:
      result.addSon(g)

proc checkInstantiateGenerics*(generics: FExpr) =
  for g in generics:
    g.assert(g.kind == fexprSymbol)
    if g.symbol.instance.isNone:
      g.error("cannot instantiate $#." % $g)
      
proc expandDeftype*(scope: Scope, fexpr: var FExpr, argtypes: seq[Symbol]): FExpr =
  fexpr.assert(fexpr.hasDeftype)

  for i, arg in argtypes:
    fexpr.deftype.generics[i].symbol.applyInstance(arg)
  if argtypes.isSpecTypes:
    checkInstantiateGenerics(fexpr.deftype.generics)

  let typename = fexpr.deftype.name.symbol.name
  let manglingname = genManglingName(typename, argtypes)
  # TODO: recursive type support

  let specopt = fexpr.internalScope.getDecl(manglingname)
  if specopt.isSome:
    let fsym = fsymbol(fexpr.span, specopt.get)
    return fsym
  
  let exbody = fblock(fexpr.deftype.body.span)
  for b in fexpr.deftype.body:
    let extype = scope.expandType(b[1])
    exbody.addSon(fseq(b.span, @[b[0], extype]))
  let generics = fexpr.deftype.generics.expandGenerics()

  let tsym = fexpr.internalScope.symbol(
    typename,
    symbolTypeGenerics,
    fexpr.span.quoteFExpr("type `embed `embed $ `embed `embed", [fexpr.deftype.name, generics, fexpr.deftype.pragma, exbody])
    # fseq(fexpr.span, @[fident(fexpr.span, name("type")), fexpr.deftype.name, generics, fexpr.deftype.pragma, exbody])
  )
  tsym.types = generics.mapIt(it.symbol)
  let fsym = fsymbol(fexpr.span, tsym)
  
  let deftype = DeftypeExpr(
    name: fsym,
    generics: generics,
    pragma: fexpr.deftype.pragma,
    body: exbody
  )
  tsym.fexpr.metadata = fexpr.metadata
  tsym.fexpr.internalMark = internalDeftype
  tsym.fexpr.deftype = deftype

  discard fexpr.internalScope.addDecl(manglingname, tsym)
  fexpr.internalScope.top.toplevels.add(tsym.fexpr)

  return fsym
    
proc expandDefn*(rootPass: PassProcType, scope: Scope, fexpr: var FExpr, argtypes: seq[Symbol]): FExpr =
  fexpr.assert(fexpr.hasDefn)
  fexpr.assert(fexpr.defn.args.len == argtypes.len)

  for i, arg in fexpr.defn.args:
    arg[1].assert(arg[1].kind == fexprSymbol)
    arg[1].symbol.applyInstance(argtypes[i])
  if argtypes.isSpecTypes:
    checkInstantiateGenerics(fexpr.defn.generics)

  let fname = fexpr.defn.name.symbol.name
  let generics = fexpr.defn.generics.expandGenerics()
  let genericstypes = generics.mapIt(it.symbol)

  let specopt = fexpr.internalScope.getSpecFunc(procname(fname, argtypes, genericstypes))
  if specopt.isSome:
    let fsym = fsymbol(fexpr.span, specopt.get.sym)
    return fsym

  let exscope = fexpr.internalScope.extendScope()
  let args = flist(fexpr.defn.args.span)
  for arg in fexpr.defn.args:
    let extype = scope.expandType(arg[1])
    let argcopy = fsymbol(arg[0].span, arg[0].symbol.symcopy)
    argcopy.metadata = arg[0].metadata
    args.addSon(fseq(arg.span, @[argcopy, extype]))
    argcopy.symbol.fexpr.typ = extype.symbol
    let status = exscope.addDecl(name(argcopy), argcopy.symbol)
    if not status:
      argcopy.error("redefinition $# variable." % $arg[0])
  let ret = scope.expandType(fexpr.defn.ret)

  let sym = fexpr.internalScope.symbol(
    fname,
    fexpr.defn.name.symbol.kind,
    fexpr.span.quoteFExpr("fn `embed `embed `embed `embed $ `embed `embed", [fexpr.defn.name, generics, args, ret, fexpr.defn.pragma, fexpr.defn.body])
    # fseq(fexpr.span, @[fident(fexpr.span, name("fn")), fexpr.defn.name, generics, args, ret, fprefix(fexpr.span ,name("$")), fexpr.defn.pragma, fexpr.defn.body])
  )
  let fsym = fsymbol(fexpr.span, sym)

  let defn = DefnExpr(
    name: fsym,
    generics: generics,
    args: args,
    ret: ret,
    pragma: fexpr.defn.pragma,
    body: fexpr.defn.body
  )

  let pd = ProcDecl(
    isInternal: false,
    name: fname,
    argtypes: args.mapIt(it[1].symbol),
    generics: genericstypes,
    returntype: ret.symbol,
    sym: sym
  )
  sym.fexpr.metadata = fexpr.metadata
  sym.fexpr.internalMark = internalDefn
  sym.fexpr.defn = defn

  fexpr.internalScope.addSpecFunc(pd)
  fexpr.internalScope.top.toplevels.add(sym.fexpr)

  exscope.importScope(name("flori_current_scope"), scope.top)
  let exbody = if argtypes.isSpecTypes:
                 var copybody: FExpr
                 copybody.deepCopy(fexpr.defn.body)
                 exscope.rootPass(copybody)
                 if copybody.len != 0 and not copybody[^1].typ.isVoidType:
                   if copybody[^1].kind == fexprSymbol:
                     if not copybody[^1].symbol.fexpr.ctrc.inc:
                       copybody[^1].error("value is already destroyed.")
                 copybody
               else:
                 fexpr.defn.body
  sym.fexpr.defn.body = exbody
  sym.fexpr[7] = exbody
  if argtypes.isSpecTypes:
    fnScopeout(rootPass, exscope, sym.fexpr)

  return fsym
