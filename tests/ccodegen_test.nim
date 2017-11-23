
import unittest

import compiler.types, compiler.fexpr, compiler.parser
import compiler.scope, compiler.semantic
import compiler.ccodegen

let prelude = """
(deftype void ${:importc "void" :header nodeclc})
(deftype bool ${:importc "bool" :header "stdbool.h"})
(deftype cstring ${:importc "char*" :header nodeclc})
(deftype int ${:importc "int64_t" :header "stdint.h"})

(defn + [^int a ^int b] ^int ${:importc "+" :header nodeclc :pattern infixc})
(defn = [^int a ^int b] ^bool ${:importc "==" :header nodeclc :pattern infixc})
(defn printf [^cstring fmt ^int x] ${:importc "printf" :header "stdio.h"})
"""

suite "C codegen":
  test "c ffi":
    let semctx = newSemanticContext()
    let genctx = newCCodegenContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      (printf "%d" 5)
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    genctx.codegen(semctx)
    genctx.writeModules("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"

typedef void testmodule_void;
typedef bool testmodule_bool;
typedef char* testmodule_cstring;
typedef int64_t testmodule_int;

void testmodule_init() {
printf("%d", 5);
}
"""
  test "defn":
    let semctx = newSemanticContext()
    let genctx = newCCodegenContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      (defn add5 [^int x] ^int
        (+ x 5))
      (printf "%d" (add5 4))
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    genctx.codegen(semctx)
    genctx.writeModules("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"

typedef void testmodule_void;
typedef bool testmodule_bool;
typedef char* testmodule_cstring;
typedef int64_t testmodule_int;

testmodule_int testmodule_add5(testmodule_int x) {
return (x + 5);
}

void testmodule_init() {
printf("%d", testmodule_add5(4));
}
"""
  test "if":
    let semctx = newSemanticContext()
    let genctx = newCCodegenContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      (if (= 1 2)
          (printf "%d" 4)
          (printf "%d" 5))
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    genctx.codegen(semctx)
    genctx.writeModules("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"

typedef void testmodule_void;
typedef bool testmodule_bool;
typedef char* testmodule_cstring;
typedef int64_t testmodule_int;

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
      (while (= 1 2)
        (printf "%d" 9))
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    genctx.codegen(semctx)
    genctx.writeModules("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"

typedef void testmodule_void;
typedef bool testmodule_bool;
typedef char* testmodule_cstring;
typedef int64_t testmodule_int;

void testmodule_init() {
while ((1 == 2)) {
printf("%d", 9);
};
}
"""
  test "toplevel def":
    let semctx = newSemanticContext()
    let genctx = newCCodegenContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      (def nine 9)
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    genctx.codegen(semctx)
    genctx.writeModules("floricache")
    check readFile("floricache/testmodule.c") == """
#include "stdint.h"
#include "stdio.h"
#include "stdbool.h"

typedef void testmodule_void;
typedef bool testmodule_bool;
typedef char* testmodule_cstring;
typedef int64_t testmodule_int;

int32_t testmodule_NINE;
void testmodule_init() {
testmodule_NINE = 9;
}
"""
