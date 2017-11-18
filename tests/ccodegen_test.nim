
import unittest

import compiler.fexpr, compiler.parser
import compiler.scope, compiler.semantic
import compiler.ccodegen

let prelude = """
struct Void $[importc "void", nodecl]
struct CString $[importc "char*", nodecl]
struct Int32 $[importc "int32_t", header "stdint.h"]
struct Bool $[importc "bool", header "stdbool.h"]

fn `+(a Int32, b Int32) Int32 $[importc "+", nodecl, infix]
fn `==(a Int32, b Int32) Bool $[importc "==", nodecl, infix]
fn printf(fmt CString, value Int32) $[importc "printf", header "stdio.h"]
"""

suite "C codegen":
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

typedef void testmodule_Void;
typedef char* testmodule_CString;
typedef int32_t testmodule_Int32;
typedef bool testmodule_Bool;

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

typedef void testmodule_Void;
typedef char* testmodule_CString;
typedef int32_t testmodule_Int32;
typedef bool testmodule_Bool;

testmodule_Int32 testmodule_add5(testmodule_Int32 x) {
return (x + 5);
}

void testmodule_init() {
printf("%d", testmodule_add5(4));
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

typedef void testmodule_Void;
typedef char* testmodule_CString;
typedef int32_t testmodule_Int32;
typedef bool testmodule_Bool;

void testmodule_init() {
if ((1 == 2)) {
printf("%d", 4);
} else {
printf("%d", 5);
}
;
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
