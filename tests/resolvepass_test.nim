
import unittest
import tables
import options
import compiler.sast, compiler.sparser
import compiler.semtree
import compiler.sempass, compiler.sempass_create, compiler.sempass_default

let prelude = """
(c-type Void :name "void" :nodecl)
(c-type CString :name "char*" :nodecl)
(c-type Int32 :name "int32_t" :header "stdint.h")
(c-type Bool :name "bool" :header "stdbool.h")
@(: Int32 Int32 -> Int32)
(c-func + :infix :nodecl)
@(: Int32 Int32 -> Bool)
(c-func = :name "==" :infix :nodecl)
@(: CString Int32)
(c-func printf :header "stdio.h")
"""

let pointerprelude = """
(c-type Pointer :name "void*" :nodecl)
(c-type Ptr :param [:a] :pattern "$1*" :nodecl)

@(: Int32 -> Pointer)
(c-func alloc :name "malloc" :header "stdlib.h")
@(: (Typedesc :a) :b -> :a)
(c-func cast :pattern "($1)$2" :nodecl)
"""

suite "pass resolve":
  test "generics":
    let passctx = newDefaultSemPassContext()
    let sexprs = parseToplevel("testmodule.flori", prelude & pointerprelude & """
      @(: :a -> (Ptr :a))
      (defn alloc [value]
        (cast (Ptr :a) (alloc (sizeof :a))))
    """)
    passctx.createModuleFromSExpr("testmodule", sexprs)
    passctx.execute()
