
import docopt
import os, osproc, strutils
import fexpr_core, passmacro, compile, compileutils, ccodegen, internalpass, elimpass

proc evalFlori*(scope: Scope, s: string) =
  try:
    var fexpr = parseFExpr("<repl>", s)
    fexpr.isToplevel = true
    scope.rootPass(fexpr)
    if fexpr.hasTyp and not fexpr.typ.isVoidType:
      fexpr = fexpr.span.quoteFExpr("println(`embed)", [fexpr])
      fexpr.isToplevel = true
      scope.rootPass(fexpr)
    scope.ctx.globaltoplevels.add(fexpr)
    
    for top in scope.ctx.globaltoplevels.mitems:
      top.internalScope.resetElim(top)
    for top in scope.ctx.globaltoplevels.mitems:
      top.internalScope.processElimPass(top)
    
    let gen = newCCodegenContext()
    let csrc = gen.codegenSingle(scope.ctx)
    writeFile(cachedir / "flori_compiled.c", csrc)
    gen.compileWithTCC(cachedir, "-o$# -I$#" % [cachedir/"flori_repltmp".exe, getAppDir()/"../ffi"])
    discard execShellCmd(cachedir/"flori_repltmp".exe)

    if not fexpr.hasInternalMark or fexpr.internalMark notin {internalDefn, internalMacro, internalImport, internalExport, internalDeftype, internalTypedef}:
      scope.ctx.globaltoplevels.del(scope.ctx.globaltoplevels.high)
    
  except FParseError:
    discard
  except FExprError:
    discard

proc main() =
  echo "======================"
  echo "=   <<Flori REPL>>   ="
  echo "======================"
  echo ""
  
  let semctx = newSemanticContext()
  let scope = newScope(semctx, name("repl"), "<repl>")
  scope.importScope(name("internal"), semctx.internalScope)
  semctx.modules[name("repl")] = scope

  var s = ""
  try:
    while true:
      stdout.write("> ")
      let line = stdin.readLine()
      s &= line
      if line[^1] == ';':
        scope.evalFlori(s)
        s = ""
  except EOFError:
    discard

main()
  
