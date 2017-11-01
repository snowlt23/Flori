
import unittest
import compiler.sast, compiler.sparser
import compiler.semtree
import compiler.sempass, compiler.sempass_create
import compiler.sempass_resolve, compiler.sempass_ccodegen

suite "pass create":
  test "c ffi":
    let passctx = newSemPassContext()
    let resolvepass = newResolvePass()
    let genpass = newCCodegenPass()
    passctx.register(resolvepass)
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
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
void testmodule_init() {
printf("%d", 5);
}
"""
  test "defn":
    let passctx = newSemPassContext()
    let resolvepass = newResolvePass()
    let genpass = newCCodegenPass()
    passctx.register(resolvepass)
    passctx.register(genpass)
    let sexprs = parseToplevel("testmodule.flori", """
      (c-type Void :name "void" :nodecl)
      (c-type CString :name "char*" :nodecl)
      (c-type Int32 :name "int32_t" :header "stdint.h")
      @(: Int32 Int32 -> Int32)
      (c-func + :infix :nodecl)
      @(: CString Int32)
      (c-func printf :header "stdio.h")

      @(: Int32 -> Int32)
      (defn add5 [x]
        (+ x 5))

      (printf "%d" (add5 4))
    """)
    passctx.createModuleFromSExpr("testmodule", sexprs)
    passctx.execute()
    genpass.write("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
int32_t add5(int32_t x) {
return (x + 5);
}
void testmodule_init() {
printf("%d", add5(4));
}
"""
