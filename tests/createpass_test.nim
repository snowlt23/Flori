
import unittest
import tables
import options
import compiler.ast, compiler.parser
import compiler.semtree
import compiler.sempass, compiler.sempass_create

suite "pass create":
  test "fn":
    let passctx = newSemPassContext()
    let sexprs = parseToplevel("testmodule.flori", """
      fn add5 (x Int32) Int32 {
        x + 5
      }
    """)
    passctx.createModuleFromSExpr("testmodule", sexprs)
    let module = passctx.modules[initScopeIdent("testmodule")]
    let semdecl = module.procidents[ProcIdent(name: "add5")].idents[0].value
    check semdecl.kind == sfFunc
    check semdecl.funcname == "add5"
    check semdecl.functype.argtypes.len == 1
    check semdecl.funcargs.len == 1
    check semdecl.funcbody.len == 1
    let firstcall = semdecl.funcbody[0]
    check firstcall.kind == seFuncCall
    check firstcall.args[0].kind == seIdent
    check firstcall.args[1].kind == seInt
  test "redefinition fn":
    let passctx = newSemPassContext()
    let sexprs = parseToplevel("testmodule.flori", """
      fn add5 (x Int32) Int32 {
        x + 5
      }
      fn add5 (x Int32) Int32 {
        x + 5
      }
    """)
    expect(FExprError):
      passctx.createModuleFromSExpr("testmodule", sexprs)
  test "c-func":
    let passctx = newSemPassContext()
    let sexprs = parseToplevel("testmodule.flori", """
      cfn not(Bool) Bool ["!", nodecl]
    """)
    passctx.createModuleFromSExpr("testmodule", sexprs)
    let module = passctx.modules[initScopeIdent("testmodule")]
    let semdecl = module.procidents[ProcIdent(name: "not")].idents[0].value
    check semdecl.kind == sfCFunc
    check semdecl.cfuncname == "!"
    check semdecl.cfunctype.argtypes.len == 1
    check semdecl.cfuncheader.isNone
  test "c-type":
    let passctx = newSemPassContext()
    let sexprs = parseToplevel("testmodule.flori", """
      ctype Int32 ["int32_t", "stdint.h"]
    """)
    passctx.createModuleFromSExpr("testmodule", sexprs)
    let module = passctx.modules[initScopeIdent("testmodule")]
    let semdeclopt = module.getType(TypeIdent(name: "Int32"))
    check semdeclopt.isSome
    let semdecl = semdeclopt.get
    check semdecl.kind == stCType
    check semdecl.ctypename == "int32_t"
    check semdecl.ctypeheader.get == "stdint.h"
