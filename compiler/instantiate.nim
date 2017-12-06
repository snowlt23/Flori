
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
    result = sym.scope.symbol(sym.name, symbolType, sym.fexpr)
    for t in sym.types:
      result.types.add(ctx.instantiateSymbol(scope, t))
    discard ctx.instantiateDeftype(scope, result.fexpr, result.types)
  else:
    result = sym

proc instantiateFExpr*(ctx: SemanticContext, scope: Scope, fexpr: FExpr): FExpr =
  case fexpr.kind
  of fexprSymbol:
    return fsymbol(fexpr.span, ctx.instantiateSymbol(scope, fexpr.symbol))
  of fexprContainer:
    let cont = fcontainer(fexpr.span, fexpr.kind)
    for e in fexpr:
      cont.addSon(ctx.instantiateFExpr(scope, e))
    if fexpr.len >= 1 and fexpr[0].kind == fexprSymbol and fexpr[0].symbol.fexpr.hasinternalDefnExpr:
      discard ctx.instantiateDefn(scope, fexpr[0].symbol.fexpr, fexpr[1..^1].mapIt(it.typ.get))
    return cont
  else:
    return fexpr

proc instantiateDeftype*(ctx: SemanticContext, scope: Scope, fexpr: FExpr, types: seq[Symbol]): FExpr =
  if types.isSpecTypes:
    let tbody = fexpr.internalDeftypeExpr.body
    if tbody.len != types.len:
      fexpr.error("exists uninitialized field.")
    for i in 0..<tbody.len:
      tbody[i][1].symbol.instance = some(types[i])
      
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
      generics: none(FExpr),
      pragma: fexpr.internalDeftypeExpr.pragma,
      body: fbody
    )

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
  else:
    result = fexpr
    
proc instantiateDefn*(ctx: SemanticContext, scope: Scope, fexpr: FExpr, types: seq[Symbol]): FExpr =
  if types.isSpecTypes:
    let fnargs = fexpr.internalDefnExpr.args
    for i in 0..<fnargs.len:
      fnargs[i][1].symbol.instance = some(types[i])

    let manglingname = name($fexpr.internalDefnExpr.name & types.mapIt($it).join("_"))
    let fbody = ctx.instantiateFExpr(scope, fexpr.internalDefnExpr.body)
    let fname = fident(fexpr.internalDefnExpr.name.span, manglingname)

    let sym = fexpr.internalDefnExpr.scope.symbol(manglingname, symbolFunc, fname)
    let fsym = fsymbol(fexpr.span, sym)
    fexpr.internalDefnExpr.scope.toplevels.add(fsym)

    let defnexpr = DefnExpr(
      scope: fexpr.internalDefnExpr.scope,
      name: fsym,
      generics: none(FExpr),
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
  else:
    result = fexpr

# # return fn symbol fexpr
# proc instantiateFuncCall*(ctx: SemanticContext, scope: Scope, fexpr: FExpr, types: seq[Symbol]): FExpr =
#   if types.isSpecTypes:
#     echo fexpr
#     return ctx.instantiateDefn(scope, fexpr[0].symbol.fexpr, types)
#   else:
#     return fexpr[0]
