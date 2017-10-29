
import unittest
import tables
import options
import compiler.sast, compiler.sparser
import compiler.semtree
import compiler.sempass, compiler.sempass_create

suite "pass create":
  test "defn":
    let passctx = newSemPassContext()
    let sexprs = parseToplevel("testmodule.flori", """
      @(: Int32 -> Int32)
      (defn add5 [x]
        (+ x 5))
    """)
    passctx.createModuleFromSExpr("testmodule", sexprs)
    let module = passctx.modules[initScopeIdent("testmodule")]
    let semdecl = module.procidents[ProcIdent(name: "add5")].idents[0].value
    check semdecl.kind == sdFunc
    check semdecl.funcname == "add5"
    check semdecl.functype.argtypes.len == 1
    check semdecl.funcargs.len == 1
    check semdecl.funcbody.len == 1
    let firstcall = semdecl.funcbody[0]
    check firstcall.kind == seFuncCall
    check firstcall.args[0].kind == seIdent
    check firstcall.args[1].kind == seInt
  test "c-func":
    let passctx = newSemPassContext()
    let sexprs = parseToplevel("testmodule.flori", """
      @(: Bool -> Bool)
      (c-func not :name "!" :nodecl)
    """)
    passctx.createModuleFromSExpr("testmodule", sexprs)
    let module = passctx.modules[initScopeIdent("testmodule")]
    let semdecl = module.procidents[ProcIdent(name: "not")].idents[0].value
    check semdecl.kind == sdCFunc
    check semdecl.cfuncname == "not"
    check semdecl.cfunctype.argtypes.len == 1
    check semdecl.cfuncheader.isNone
  test "c-type":
    let passctx = newSemPassContext()
    let sexprs = parseToplevel("testmodule.flori", """
      (c-type Int32 :name "int32_t" :header "stdint.h")
    """)
    passctx.createModuleFromSExpr("testmodule", sexprs)
    let module = passctx.modules[initScopeIdent("testmodule")]
    let semdeclopt = module.getType(TypeIdent(name: "Int32"))
    check semdeclopt.isSome
    let semdecl = semdeclopt.get
    check semdecl.kind == sdCType
