
type
  DefnExpr* = object
    scope*: Scope
    name*: FExpr
    generics*: Option[FExpr]
    args*: FExpr
    ret*: FExpr
    pragma*: FExpr
    body*: FExpr
  DeftypeExpr* = object
    scope*: Scope
    name*: FExpr
    generics*: Option[FExpr]
    pragma*: FExpr
    body*: FExpr
  InitExpr* = object
    typ*: FExpr
    body*: FExpr

defMetadata(internalInitExpr, InitExpr)

defMetadata(internalDefnExpr, DefnExpr)
defMetadata(internalDeftypeExpr, DeftypeExpr)

proc instantiateDeftype*(ctx: SemanticContext, scope: Scope, fexpr: FExpr, types: seq[Symbol]): FExpr
proc instantiateDefn*(ctx: SemanticContext, scope: Scope, fexpr: FExpr, types: seq[Symbol]): FExpr

proc instantiateSymbol*(ctx: SemanticContext, scope: Scope, sym: Symbol): Symbol =
  if sym.kind == symbolGenerics:
    if sym.instance.isNone:
      sym.fexpr.error("cannot instantiate $#." % $sym)
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
  fexpr.internalInitExpr = InitExpr(
    typ: fsym,
    body: fexpr.internalInitExpr.body
  )
  fexpr.typ = some(fsym.symbol)

proc instantiateFExpr*(ctx: SemanticContext, scope: Scope, fexpr: FExpr): FExpr =
  case fexpr.kind
  of fexprSymbol:
    result = fsymbol(fexpr.span, ctx.instantiateSymbol(scope, fexpr.symbol))
    result.metadata = fexpr.metadata
  of fexprContainer:
    let cont = fcontainer(fexpr.span, fexpr.kind)
    for e in fexpr:
      cont.addSon(ctx.instantiateFExpr(scope, e))
    if fexpr.len >= 1 and fexpr[0].kind == fexprSymbol and fexpr[0].symbol.fexpr.hasinternalDefnExpr:
      discard ctx.instantiateDefn(scope, fexpr[0].symbol.fexpr, fexpr[1..^1].mapIt(it.typ.get))
    elif fexpr.hasInternalMark:
      case fexpr.internalMark
      of internalInit:
        ctx.instantiateInternalInit(scope, fexpr)
      else:
        discard # FIXME:
    if fexpr.typ.isSome:
      cont.typ = some(ctx.instantiateSymbol(scope, fexpr.typ.get))
    cont.metadata = fexpr.metadata
    return cont
  else:
    return fexpr

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
  # if types.isSpecTypes:
  let tbody = fexpr.internalDeftypeExpr.body
  if tbody.len != types.len:
    fexpr.error("exists uninitialized field.")
  for i in 0..<tbody.len:
    tbody[i][1].symbol.applyInstance(types[i])
    
  let name = fexpr.internalDeftypeExpr.name.symbol.name
  let manglingname = name($name & "_" & types.mapIt($it).join("_"))
  let fbody = ctx.instantiateFExpr(scope, fexpr.internalDeftypeExpr.body)
  let fname = fident(fexpr.internalDeftypeExpr.name.span, manglingname)

  let sym = fexpr.internalDeftypeExpr.scope.symbol(name, symbolType, fname)
  sym.types = types
  let fsym = fsymbol(fexpr.span, sym)
  fexpr.internalDeftypeExpr.scope.toplevels.add(fsym)

  let deftypeexpr = DeftypeExpr(
    scope: fexpr.internalDeftypeExpr.scope,
    name: fsym,
    generics: if types.isSpecTypes(): none(FExpr) else: some(flist(fexpr.span)),
    pragma: fexpr.internalDeftypeExpr.pragma,
    body: fbody
  )
  fname.internalMark = internalDeftype
  fname.internalDeftypeExpr = deftypeexpr

  if fexpr.internalDeftypeExpr.scope.addDecl(manglingname, sym):
    result = fsym
  else:
    let opt = fexpr.internalDeftypeExpr.scope.getDecl(manglingname)
    if opt.isNone:
      fexpr.error("cannot find instantiate type.")
    result = fsymbol(fexpr.span, opt.get)
  result.internalPragma = fexpr.internalPragma
  result.internalDeftypeExpr = deftypeexpr
  result.internalMark = internalDeftype
    
proc instantiateDefn*(ctx: SemanticContext, scope: Scope, fexpr: FExpr, types: seq[Symbol]): FExpr =
  let fnargs = fexpr.internalDefnExpr.args
  for i in 0..<fnargs.len:
    fnargs[i][1].symbol.applyInstance(types[i])

  let manglingname = name($fexpr.internalDefnExpr.name & "_" & types.mapIt($it).join("_"))
  let fbody = ctx.instantiateFExpr(scope, fexpr.internalDefnExpr.body)
  let fname = fident(fexpr.internalDefnExpr.name.span, manglingname)
  fname.internalMark = internalDefn

  let sym = fexpr.internalDefnExpr.scope.symbol(manglingname, symbolFunc, fname)
  let fsym = fsymbol(fexpr.span, sym)
  fexpr.internalDefnExpr.scope.toplevels.add(fsym)

  let defnexpr = DefnExpr(
    scope: fexpr.internalDefnExpr.scope,
    name: fsym,
    generics: if types.isSpecTypes(): none(FExpr) else: some(flist(fexpr.span)),
    args: ctx.instantiateFExpr(scope, fexpr.internalDefnExpr.args),
    ret: ctx.instantiateFExpr(scope, fexpr.internalDefnExpr.ret),
    pragma: fexpr.internalDefnExpr.pragma,
    body: fbody
  )

  if fexpr.internalDefnExpr.scope.addFunc(ProcDecl(
    isInternal: false,
    name: manglingname,
    argtypes: defnexpr.args.mapIt(it[1].symbol),
    returntype: defnexpr.ret.symbol,
    sym: sym
  )):
    result = fsym
  else:
    let opt = fexpr.internalDefnExpr.scope.getFunc(procname(manglingname, @[]))
    if opt.isNone:
      fexpr.error("cannot find instantiate function.")
    result = fsymbol(fexpr.span, opt.get.sym)
  result.internalPragma = fexpr.internalPragma
  result.internalDefnExpr = defnexpr
  result.internalMark = internalDefn
