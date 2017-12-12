
import unittest

import compiler.types, compiler.fexpr, compiler.parser
import compiler.scope, compiler.semantic, compiler.internal
import compiler.ccodegen

let prelude = """
type Void $[importc "void", header nodeclc]
type Bool $[importc "bool", header "stdbool.h"]
type CString $[importc "char*", header nodeclc]
type Int $[importc "int64_t", header "stdint.h"]

fn `+(a Int, b Int) Int $[importc "+", header nodeclc, pattern infixc]
fn `-(a Int, b Int) Int $[importc "-", header nodeclc, pattern infixc]
fn `<(a Int, b Int) Bool $[importc "<", header nodeclc, pattern infixc]
fn `==(a Int, b Int) Bool $[importc "==", header nodeclc, pattern infixc]
fn printf(fmt CString, x Int) $[importc "printf", header "stdio.h"]
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
    genctx.writeModules("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"

typedef void testmodule_Void;
typedef bool testmodule_Bool;
typedef char* testmodule_CString;
typedef int64_t testmodule_Int;


void __flori_testmodule_init() {
printf("%d", 5);
}
"""
  test "defn":
    let semctx = newSemanticContext()
    let genctx = newCCodegenContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      fn add5(x Int) Int {
        x + 5
      }
      printf("%d", add5(4))
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    genctx.codegen(semctx)
    genctx.writeModules("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"

typedef void testmodule_Void;
typedef bool testmodule_Bool;
typedef char* testmodule_CString;
typedef int64_t testmodule_Int;
testmodule_Int testmodule_add5_testmodule_Int(testmodule_Int x);

testmodule_Int testmodule_add5_testmodule_Int(testmodule_Int x) {
return (x + 5);
}

void __flori_testmodule_init() {
printf("%d", testmodule_add5_testmodule_Int(4));
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
    genctx.writeModules("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"

typedef void testmodule_Void;
typedef bool testmodule_Bool;
typedef char* testmodule_CString;
typedef int64_t testmodule_Int;


void __flori_testmodule_init() {
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
    genctx.writeModules("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"

typedef void testmodule_Void;
typedef bool testmodule_Bool;
typedef char* testmodule_CString;
typedef int64_t testmodule_Int;


void __flori_testmodule_init() {
while ((1 == 2)) {
printf("%d", 9);
};
}
"""
  test "toplevel def":
    let semctx = newSemanticContext()
    let genctx = newCCodegenContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      nine := 9
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    genctx.codegen(semctx)
    genctx.writeModules("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"

typedef void testmodule_Void;
typedef bool testmodule_Bool;
typedef char* testmodule_CString;
typedef int64_t testmodule_Int;
extern testmodule_Int testmodule_nine;
testmodule_Int testmodule_nine;
void __flori_testmodule_init() {
testmodule_nine = 9;
}
"""
  test "local def":
    let semctx = newSemanticContext()
    let genctx = newCCodegenContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      fn test() {
        name := "feelsgoodman"
      }
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    genctx.codegen(semctx)
    genctx.writeModules("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"

typedef void testmodule_Void;
typedef bool testmodule_Bool;
typedef char* testmodule_CString;
typedef int64_t testmodule_Int;
testmodule_Void testmodule_test_();

testmodule_Void testmodule_test_() {
testmodule_CString testmodule_name = "feelsgoodman";
}

void __flori_testmodule_init() {
;
}
"""
  test "generics init":
    let semctx = newSemanticContext()
    let genctx = newCCodegenContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      type Wrap[T] {
        x T
      }
      init(Wrap){9}
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    genctx.codegen(semctx)
    genctx.writeModules("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"

typedef void testmodule_Void;
typedef bool testmodule_Bool;
typedef char* testmodule_CString;
typedef int64_t testmodule_Int;
typedef struct {
x testmodule_Int;
} testmodule_Wrap_testmodule_Int;


void __flori_testmodule_init() {
testmodule_Wrap_testmodule_Int{9};
}
"""
  test "generics":
    let semctx = newSemanticContext()
    let genctx = newCCodegenContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      type Wrap[T] {
        x T
      }
      fn wrap[T](x T) Wrap[T] {
        init(Wrap){x}
      }
      wrap(9)
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    genctx.codegen(semctx)
    genctx.writeModules("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"

typedef void testmodule_Void;
typedef bool testmodule_Bool;
typedef char* testmodule_CString;
typedef int64_t testmodule_Int;
typedef struct {
x testmodule_Int;
} testmodule_Wrap_testmodule_Int;
testmodule_Wrap_testmodule_Int testmodule_wrap_testmodule_Int(testmodule_Int x);

testmodule_Wrap_testmodule_Int testmodule_wrap_testmodule_Int(testmodule_Int x) {
return testmodule_Wrap_testmodule_Int{x};
}

void __flori_testmodule_init() {
testmodule_wrap_testmodule_Int(9);
}
"""
  test "field access":
    let semctx = newSemanticContext()
    let genctx = newCCodegenContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      type Wrap[T] {
        x T
      }
      fn wrap[T](x T) Wrap[T] {
        init(Wrap){x}
      }
      wrap(9).x
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    genctx.codegen(semctx)
    genctx.writeModules("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"

typedef void testmodule_Void;
typedef bool testmodule_Bool;
typedef char* testmodule_CString;
typedef int64_t testmodule_Int;
typedef struct {
x testmodule_Int;
} testmodule_Wrap_testmodule_Int;
testmodule_Wrap_testmodule_Int testmodule_wrap_testmodule_Int(testmodule_Int x);

testmodule_Wrap_testmodule_Int testmodule_wrap_testmodule_Int(testmodule_Int x) {
return testmodule_Wrap_testmodule_Int{x};
}

void __flori_testmodule_init() {
testmodule_wrap_testmodule_Int(9).x;
}
"""
  test "recursion generics":
    let semctx = newSemanticContext()
    let genctx = newCCodegenContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      fn fib[T](n T) T {
        if (n < 2) {
          n
        } else {
          fib(n-1) + fib(n-2)
        }
      }
      printf("%d", fib(38))
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    genctx.codegen(semctx)
    genctx.writeModules("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"

typedef void testmodule_Void;
typedef bool testmodule_Bool;
typedef char* testmodule_CString;
typedef int64_t testmodule_Int;
testmodule_Int testmodule_fib_testmodule_Int(testmodule_Int n);

testmodule_Int testmodule_fib_testmodule_Int(testmodule_Int n) {
testmodule_Int __floritmp0;
if ((n < 2)) {
__floritmp0 = n;
} else {
__floritmp0 = (testmodule_fib_testmodule_Int((n - 1)) + testmodule_fib_testmodule_Int((n - 2)));
}return __floritmp0;
}

void __flori_testmodule_init() {
printf("%d", testmodule_fib_testmodule_Int(38));
}
"""
