
import unittest

import compiler.ast, compiler.parser
import compiler.semtree
import compiler.sempass, compiler.sempass_create
import compiler.sempass_default
import compiler.sempass_ccodegen

let prelude = """
ctype Void ["void", nodecl]
ctype CString ["char*", nodecl]
ctype Int32 ["int32_t", "stdint.h"]
ctype Bool ["bool", "stdbool.h"]

cfn +(Int32, Int32) Int32 ["+", nodecl, infix]
cfn ==(Int32, Int32) Bool ["==", nodecl, infix]
cfn printf(CString, Int32) Void ["printf", "stdio.h"]
"""

suite "pass create":
  test "c ffi":
    let passctx = newDefaultSemPassContext()
    let genpass = newCCodegenPass()
    passctx.register(genpass)
    let sexprs = parseToplevel("testmodule.flori", prelude & """
      printf("%d", 5)
    """)
    passctx.createModuleFromSExpr("testmodule", sexprs)
    passctx.execute()
    genpass.write("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"

void testmodule_init() {
printf("%d", 5);
}
"""
  test "fn":
    let passctx = newDefaultSemPassContext()
    let genpass = newCCodegenPass()
    passctx.register(genpass)
    let sexprs = parseToplevel("testmodule.flori", prelude & """
      fn add5(x Int32) Int32 {
        x + 5
      }
      printf("%d", add5(4))
    """)
    passctx.createModuleFromSExpr("testmodule", sexprs)
    passctx.execute()
    genpass.write("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"

int32_t testmodule_add5_int32_t(int32_t x) {
return (x + 5);
}

void testmodule_init() {
printf("%d", testmodule_add5_int32_t(4));
}
"""
  test "var":
    let passctx = newDefaultSemPassContext()
    let genpass = newCCodegenPass()
    passctx.register(genpass)
    let sexprs = parseToplevel("testmodule.flori", prelude & """
      var NINE = 9
    """)
    passctx.createModuleFromSExpr("testmodule", sexprs)
    passctx.execute()
    genpass.write("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"

int32_t testmodule_NINE;
void testmodule_init() {
testmodule_NINE = 9;
}
"""
  test "if":
    let passctx = newDefaultSemPassContext()
    let genpass = newCCodegenPass()
    passctx.register(genpass)
    let sexprs = parseToplevel("testmodule.flori", prelude & """
      if (1 == 2) {
        printf("%d", 4)
      } else {
        printf("%d", 5)
      }
    """)
    passctx.createModuleFromSExpr("testmodule", sexprs)
    passctx.execute()
    genpass.write("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"

void testmodule_init() {
if ((1 == 2)) {
printf("%d", 4);
} else {
printf("%d", 5);
};
}
"""
  test "while":
    let passctx = newDefaultSemPassContext()
    let genpass = newCCodegenPass()
    passctx.register(genpass)
    let sexprs = parseToplevel("testmodule.flori", prelude & """
      while (1 == 2) {
        printf("%d", 9)
      }
    """)
    passctx.createModuleFromSExpr("testmodule", sexprs)
    passctx.execute()
    genpass.write("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"

void testmodule_init() {
while ((1 == 2)) {
printf("%d", 9);
};
}
"""
