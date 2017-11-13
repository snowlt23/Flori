
import unittest
import tables
import options
import compiler.fexpr, compiler.parser, compiler.scope
import compiler.semantic

let prelude = """
struct Void [importc "void", nodecl]
struct CString [importc "char*", nodecl]
struct Int32 [importc "int32_t", header "stdint.h"]
struct Bool [importc "bool", header "stdbool.h"]

fn `+(a Int32, b Int32) Int32 [importc, nodecl, infix]
fn `==(a Int32, b Int32) Bool [importc, nodecl, infix]
fn `printf(fmt: CString, val: Int32) [importc, header "stdio.h"]
"""

let pointerprelude = """
protocol All {}

type Pointer [importc "void*", nodecl]
type Ptr [T All] [importc, nodecl, pattern "$1*"]

fn sizeof(T Typedesc) Int32 [importc, nodecl]
fn alloc(Int32) Pointer [importc "malloc", header "stdlib.h"]
fn cast[F All](T Typedesc, value F) Ptr!T [importc, nodecl, pattern "($1)$2"]
"""

suite "pass resolve":
  test "typeinfer":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      1 + 1
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check fexprs[0].typ.isSome
  # test "generics":
  #   let semctx = newSemanticContext()
  #   let fexprs = parseToplevel("testmodule.flori", prelude & pointerprelude & """
  #     fn alloc[T All](value T) Ptr!T {
  #       p := cast(Ptr!T, alloc(sizeof(T)))
  #       p!0 = value
  #       return p
  #     }
  #   """)
  #   semctx.evalModule(name("testmodule"), fexprs)
