
import semtree
import sempass
import tables

iterator walkModule*(ctx: SemPassContext): (ScopeIdent, SemScope) =
  for si, module in ctx.modules:
    yield(si, module)

iterator walkFunc*(scope: SemScope): SemFunc =
  for group in scope.procidents.values:
    for f in group.idents:
      yield(f.value)
iterator walkFunc*(ctx: SemPassContext): SemFunc =
  for module in ctx.modules.values:
    for f in module.walkFunc:
      yield(f)

iterator walkType*(scope: SemScope): SemType =
  for t in scope.typeidents.values:
    yield(t)
iterator walkType*(ctx: SemPassContext): SemType =
  for module in ctx.modules.values:
    for t in module.walkType:
      yield(t)

iterator walkFuncExpr*(ctx: SemPassContext): SemExpr =
  for f in ctx.walkFunc:
    if f.kind == sfFunc:
      for e in f.funcbody:
        yield(e)

iterator walkTopExpr*(module: SemScope): SemExpr =
  for e in module.toplevels:
    yield(e)
iterator walkTopExpr*(ctx: SemPassContext): SemExpr =
  for module in ctx.modules.values:
    for e in module.toplevels:
      yield(e)

iterator walkExpr*(ctx: SemPassContext): SemExpr =
  for e in ctx.walkFuncExpr():
    yield(e)
  for e in ctx.walkTopExpr():
    yield(e)
