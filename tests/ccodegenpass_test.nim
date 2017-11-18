
import unittest

import compiler.fexpr, compiler.parser
import compiler.scope, compiler.semantic
import compiler.ccodegen

let prelude = """
struct Void $[importc "void", nodecl]
struct CString $[importc "char*", nodecl]
struct Int32 $[importc "int32_t", header "stdint.h"]
struct Bool $[importc "bool", header "stdbool.h"]

fn +(Int32, Int32) Int32 $[importc "+", nodecl, infix]
fn ==(Int32, Int32) Bool $[importc "==", nodecl, infix]
fn printf(CString, Int32) Void $[importc "printf", header "stdio.h"]
"""

suite "pass create":
  test "c ffi":
    let semctx = newSemanticContext()
    let genctx = newCCodegenContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      printf("%d", 5)
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    genctx.codegen(semctx)
    genctx.write("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"

void testmodule_init() {
printf("%d", 5);
}
"""
  test "fn":
    let semctx = newSemanticContext()
    let genctx = newCCodegenContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      fn add5(x Int32) Int32 {
        x + 5
      }
      printf("%d", add5(4))
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    genctx.codegen(semctx)
    genctx.write("floricache")
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
    let semctx = newSemanticContext()
    let genctx = newCCodegenContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      var NINE = 9
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    genctx.codegen(semctx)
    genctx.write("floricache")
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
    let semctx = newSemanticContext()
    let genctx = newCCodegenContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      if (1 == 2) {
        printf("%d", 4)
      } else {
        printf("%d", 5)
      }
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    genctx.codegen(semctx)
    genctx.write("floricache")
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
    let semctx = newSemanticContext()
    let genctx = newCCodegenContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      while (1 == 2) {
        printf("%d", 9)
      }
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    genctx.codegen(semctx)
    genctx.write("floricache")
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
