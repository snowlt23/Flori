
import options
import macros, strutils, sequtils
import types, fexpr, scope, metadata

template definePass*(passname: untyped) =
  var passname {.compileTime.} = newStmtList()
  
macro pass*(passname: untyped, body: untyped): untyped =
  let bodystrlit = newLit(body.repr)
  result = quote do:
    static:
      `passname`.add(parseExpr(`bodystrlit`))

proc lastPass*(scope: Scope, fexpr: var FExpr) = discard
      
proc replaceNextPass*(procnode: NimNode, next: NimNode, root: NimNode): NimNode =
  let procbody = procnode[6]
  result = procnode
  result[6] = quote do:
    template nextPass(scope: Scope, fexpr: var FExpr) {.used.} =
      `next`(scope, fexpr)
    template nextPassProc(): PassProcType {.used.} = `next`
    template rootPass(scope: Scope, fexpr: var FExpr) {.used.} =
      `root`(scope, fexpr)
    template rootPassProc(): PassProcType {.used.} = `root`
    `procbody`
    
proc toDecl*(procnode: NimNode): NimNode =
  result = procnode
  result[6] = newEmptyNode()
        
proc instPassImpl*(procname: string, passnodes: NimNode): NimNode =
  result = newStmtList()
  
  let rootid = ident(procname)

  result.add quote do:
    proc `rootid`*(scope: Scope, fexpr: var FExpr)

  for passnode in passnodes:
    result.add(passnode.copy.toDecl())
  
  for i in 0..<passnodes.len-1:
    let nextid = if passnodes[i+1][0].kind == nnkPostfix:
                   passnodes[i+1][0][1]
                 else:
                   passnodes[i+1][0]
    result.add(passnodes[i].replaceNextPass(nextid, rootid))
  result.add(passnodes[^1].replaceNextPass(ident"lastPass", rootid))

  let firstpass = if passnodes[0][0].kind == nnkPostfix:
                    passnodes[0][0][1]
                  else:
                    passnodes[0][0]
  result.add quote do:
    proc `rootid`*(scope: Scope, fexpr: var FExpr) =
      `firstpass`(scope, fexpr)
  
  # echo result.repr
  
macro instPass*(passname: typed, procname: untyped): untyped =
  let sym = genSym(nskMacro, "instPassGen")
  let procstrlit = newLit($procname)
  result = quote do:
    macro `sym`*(): untyped =
      instPassImpl(`procstrlit`, `passname`)
    `sym`()
