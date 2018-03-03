
import compiler.parser, compiler.types, compiler.fexpr, compiler.scope, compiler.metadata
import passutils

import options
import strutils, sequtils

proc expandDeftype*(scope: Scope, fexpr: var FExpr, argtypes: seq[Symbol]): FExpr

proc genManglingName*(name: Name, types: seq[Symbol]): Name =
  name($name & "_" & types.mapIt($it).join("_"))

proc applyInstance*(sym: Symbol, instance: Symbol) =
  if sym.kind == symbolGenerics:
    sym.instance = some(instance)
  elif sym.kind == symbolTypeGenerics:
    sym.instance = some(instance)
    for i in 0..<sym.types.len:
      sym.types[i].applyInstance(instance.types[i])
  else:
    sym.instance = some(instance)

proc expandSymbol*(scope: Scope, sym: Symbol): Symbol =
  if sym.kind == symbolGenerics:
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
  if fexpr.internalPragma.pattern.isNone:
    fexpr.assert(fexpr.deftype.body.len == argtypes.len)

  # FIXME: into generics?
  for i, field in fexpr.deftype.body:
    field[1].assert(field[1].kind == fexprSymbol)
    field[1].symbol.applyInstance(argtypes[i])
  if argtypes.isSpecTypes:
    checkInstantiateGenerics(fexpr.deftype.generics)

  let typename = fexpr.deftype.name.symbol.name
  let manglingname = genManglingName(typename, argtypes)
  # TODO: recursive type support
  
  let exbody = fblock(fexpr.deftype.body.span)
  for b in fexpr.deftype.body:
    let extype = scope.expandType(b[1])
    exbody.addSon(fseq(b.span, @[b[0], extype]))
  let generics = fexpr.deftype.generics.expandGenerics()

  let tsym = scope.symbol(
    typename,
    symbolType,
    fseq(fexpr.span, @[fident(fexpr.span, name("type")), fexpr.deftype.name, generics, fexpr.deftype.pragma, exbody])
  )
  tsym.types = argtypes
  let fsym = fsymbol(fexpr.span, tsym)
  
  let deftype = DeftypeExpr(
    name: fsym,
    generics: generics,
    pragma: fexpr.deftype.pragma,
    body: exbody
  )
  tsym.fexpr.internalMark = internalDeftype
  tsym.fexpr.deftype = deftype

  discard scope.addDecl(manglingname, tsym)
  scope.toplevels.add(fsym)

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

  let args = flist(fexpr.defn.args.span)
  for arg in fexpr.defn.args:
    let extype = scope.expandType(arg[1])
    args.addSon(fseq(arg.span, @[arg[0], extype]))
  let ret = scope.expandType(fexpr.defn.ret)

  let exbody = if argtypes.isSpecTypes:
                 fexpr.internalScope.rootPass(fexpr.defn.body)
                 fexpr.defn.body
               else:
                 fexpr.defn.body

  let sym = fexpr.internalScope.symbol(
    fname,
    symbolFunc,
    fseq(fexpr.span, @[fident(fexpr.span, name("fn")), fexpr.defn.name, generics, args, ret, fprefix(fexpr.span ,name("$")), fexpr.defn.pragma, exbody])
  )
  let fsym = fsymbol(fexpr.span, sym)

  let defn = DefnExpr(
    name: fsym,
    generics: generics,
    args: args,
    ret: ret,
    pragma: fexpr.defn.pragma,
    body: exbody
  )

  let pd = ProcDecl(
    isInternal: false,
    name: fname,
    argtypes: args.mapIt(it[1].symbol),
    generics: genericstypes,
    returntype: ret.symbol,
    sym: sym
  )
  sym.fexpr.defn = defn

  fexpr.internalScope.addSpecFunc(pd)
  fexpr.internalScope.toplevels.add(fsym)

  return fsym
