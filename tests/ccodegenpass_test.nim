
import unittest
import compiler.sast, compiler.sparser
import compiler.semtree
import compiler.sempass, compiler.sempass_create, compiler.sempass_ccodegen

suite "pass create":
  test "defn":
    let passctx = newSemPassContext()
    let genpass = newCCodegenPass()
    passctx.register(genpass)
    let sexprs = parseToplevel("testmodule.flori", """
      (c-type Void :name "void" :nodecl)
      (c-type CString :name "char*" :nodecl)
      (c-type Int32 :name "int32_t" :header "stdint.h")

      @(: CString Int32)
      (c-func printf :header "stdio.h")

      (printf "%d" 5)
    """)
    passctx.createModuleFromSExpr("testmodule", sexprs)
    passctx.execute()
    genpass.write("floricache")
    check readFile("floricache/testmodule.c") == ""
