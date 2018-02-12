
proc genManglingName*(name: Name, types: seq[Symbol]): Name =
  name($name & "_" & types.mapIt($it).join("_"))

proc instantiateDeftype*(ctx: SemanticContext, scope: Scope, fexpr: FExpr, types: seq[Symbol]): FExpr
proc instantiateDefn*(ctx: SemanticContext, scope: Scope, fexpr: FExpr, types: seq[Symbol]): FExpr

proc instantiateSymbol*(ctx: SemanticContext, scope: Scope, sym: Symbol): Symbol =
  if sym.kind == symbolGenerics:
    if sym.instance.isNone:
      sym.fexpr.error("cannot instantiate $#." % $sym, ctx)
    result = sym.instance.get
  elif sym.kind == symbolTypeGenerics:
    var types = newSeq[Symbol]()
    for t in sym.types:
      types.add(ctx.instantiateSymbol(scope, t))
    result = ctx.instantiateDeftype(scope, sym.fexpr, types).symbol
  else:
    result = sym

proc instantiateInternalInit*(ctx: SemanticContext, scope: Scope, fexpr: FExpr) =
  var types = newSeq[Symbol]()
  for arg in fexpr[2]:
     types.add(ctx.instantiateSymbol(scope, arg.typ.get))
  let fsym = ctx.instantiateDeftype(scope, fexpr[1][0].symbol.fexpr, types)
  fexpr.initexpr = InitExpr(
    typ: fsym,
    body: fexpr.initexpr.body
  )
  fexpr.typ = some(fsym.symbol)

proc instantiateFExpr*(ctx: SemanticContext, scope: Scope, fexpr: FExpr): FExpr =
  var fexpr = fexpr
  case fexpr.kind
  of fexprSymbol:
    result = fsymbol(fexpr.span, ctx.instantiateSymbol(scope, fexpr.symbol))
    result.metadata = fexpr.metadata
  of fexprIdent:
    return fexpr
  of fexprContainer:
    ctx.evalFExpr(scope, fexpr)
    let cont = fcontainer(fexpr.span, fexpr.kind)
    for e in fexpr:
      cont.addSon(ctx.instantiateFExpr(scope, e))

    # instantiate function by arguments
    if fexpr.len >= 2 and fexpr[0].kind == fexprSymbol and fexpr[1].kind == fexprList and fexpr[0].symbol.fexpr.hasDefn:
      fexpr[0] = ctx.instantiateDefn(scope, fexpr[0].symbol.fexpr, fexpr[1].mapIt(it.getType))
    # instantiate internal
    elif fexpr.hasInternalMark:
      case fexpr.internalMark
      of internalInit:
        ctx.instantiateInternalInit(scope, fexpr)
      else:
        discard # FIXME:

    if fexpr.typ.isSome:
      cont.typ = some(ctx.instantiateSymbol(scope, fexpr.typ.get))
    cont.metadata = fexpr.metadata # copy metadata
    return cont
  else:
    var f = fexpr
    ctx.evalFExpr(scope, f)
    return f

proc applyInstance*(sym: Symbol, instance: Symbol) =
  if sym.kind == symbolGenerics:
    sym.instance = some(instance)
  elif sym.kind == symbolTypeGenerics:
    sym.instance = some(instance)
    for i in 0..<sym.types.len:
      sym.types[i].applyInstance(instance.types[i])
  else:
    sym.instance = some(instance)

proc instantiateDeftype*(ctx: SemanticContext, scope: Scope, fexpr: FExpr, types: seq[Symbol]): FExpr =
  fexpr.assert(fexpr.hasDeftype)
  if fexpr.internalPragma.pattern.isNone:
    fexpr.assert(fexpr.deftype.body.len == types.len)

  # apply type parameter to generics for instantiate
  for i, field in fexpr.deftype.body:
    field[1].assert(field[1].kind == fexprSymbol)
    field[1].symbol.applyInstance(types[i])
    
  let typename = fexpr.deftype.name.symbol.name
  let manglingname = genManglingName(typename, types)
  let instbody = fblock(fexpr.deftype.body.span)
  for b in fexpr.deftype.body:
    let iexpr = ctx.instantiateFExpr(scope, b[1])
    instbody.addSon(fseq(b.span, @[b[0], iexpr]))
  let instident = fident(fexpr.deftype.name.span, manglingname)

  let sym = fexpr.internalScope.symbol(typename, symbolType, instident)
  sym.types = types
  let fsym = fsymbol(fexpr.span, sym)

  let deftypeexpr = DeftypeExpr(
    name: fsym,
    generics: if types.isSpecTypes(): none(FExpr) else: some(flist(fexpr.span)),
    pragma: fexpr.deftype.pragma,
    body: instbody
  )
  instident.internalPragma = fexpr.internalPragma
  instident.internalMark = internalDeftype
  instident.deftype = deftypeexpr

  if fexpr.internalScope.addDecl(manglingname, sym):
    result = fsym
    fexpr.internalScope.toplevels.add(fsym) # register instantiate type to toplevel of module
  else:
    let opt = fexpr.internalScope.getDecl(manglingname)
    if opt.isNone:
      fexpr.error("cannot find instantiate type.", ctx)
    result = fsymbol(fexpr.span, opt.get)
  result.internalPragma = fexpr.internalPragma
  result.internalMark = internalDeftype
  result.deftype = deftypeexpr
    
proc checkInstantiateGenerics*(generics: FExpr) =
  for g in generics:
    if g.kind == fexprSymbol: continue
    if g.typ.get.instance.isNone:
      g.error("cannot instantiate $#." % $g)

proc instantiateDefn*(ctx: SemanticContext, scope: Scope, fexpr: FExpr, types: seq[Symbol]): FExpr =
  fexpr.assert(fexpr.hasDefn)
  fexpr.assert(fexpr.hasInternalScope)
  fexpr.assert(fexpr.defn.args.len == types.len)

  # apply type parameter to generics for instantiate
  for i, arg in fexpr.defn.args:
    arg[1].assert(arg[1].kind == fexprSymbol)
    arg[1].symbol.applyInstance(types[i])
  if types.isSpecTypes():
    checkInstantiateGenerics(fexpr.defn.generics)
    
  # let manglingname = genManglingName(fexpr.defn.name.symbol.name, types)
  let fname = fexpr.defn.name.symbol.name
  var genericstypes = newSeq[Symbol]()
  let generics = farray(fexpr.defn.generics.span)
  for g in fexpr.defn.generics:
    if g.kind == fexprSymbol:
      generics.addSon(g)
      genericstypes.add(g.symbol)
    else:
      let t = g.typ.get.instance.get
      generics.addSon(fsymbol(g.span, t))
      genericstypes.add(t)
  let specopt = fexpr.internalScope.getSpecFunc(procname(fname, types, genericstypes))
  if specopt.isSome:
    let fsym = fsymbol(fexpr.span, specopt.get.sym)
    fsym.metadata = specopt.get.sym.fexpr.metadata
    return fsym

  let instident = fident(fexpr.defn.name.span, fname)

  let instscope = fexpr.defn.body.internalScope.extendScope()
  let sym = fexpr.internalScope.symbol(fname, symbolFunc, instident)
  let fsym = fsymbol(fexpr.span, sym)
  let args = flist(fexpr.defn.args.span)
  for arg in fexpr.defn.args:
    let iexpr = ctx.instantiateFExpr(fexpr.internalScope, arg[1])
    args.addSon(fseq(arg.span, @[arg[0], iexpr]))
    let status = instscope.addDecl(name(arg[0]), iexpr.symbol)
    if not status:
      arg[0].error("redefinition $# variable." % $arg[0])
  # let ret = fsymbol(fexpr.defn.ret.span, fexpr.defn.ret.symbol.instance.get)
  let ret = ctx.instantiateFExpr(fexpr.internalScope, fexpr.defn.ret)

  var defnexpr = DefnExpr(
    name: fsym,
    generics: generics,
    args: args,
    ret: ret,
    pragma: fexpr.defn.pragma,
    body: fexpr.defn.body
  )
  instident.internalScope = fexpr.internalScope
  instident.internalPragma = fexpr.internalPragma
  instident.internalMark = internalDefn
  instident.defn = defnexpr

  let pd = ProcDecl(
    isInternal: false,
    name: fname,
    argtypes: args.mapIt(it[1].symbol),
    generics: genericstypes,
    returntype: ret.symbol,
    sym: sym
  )
  fexpr.internalScope.addSpecFunc(pd)
  fexpr.internalScope.toplevels.add(fsym) # register instantiate function to toplevel of module

  instscope.importScope(name("flori_current_scope"), scope.top)
  let instbody = if types.isSpecTypes:
                  let i = ctx.instantiateFExpr(instscope, fexpr.defn.body)
                  ctx.expandDestructor(instscope, i)
                  i
                 else:
                  fexpr.defn.body
  defnexpr.body = instbody
  instident.defn = defnexpr

  result = fsym
  result.internalScope = fexpr.internalScope
  result.internalPragma = fexpr.internalPragma
  result.internalMark = internalDefn
  result.defn = defnexpr
