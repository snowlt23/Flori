
import unittest
import tables
import options
import compiler.fexpr, compiler.parser, compiler.scope
import compiler.semantic

let prelude = """
struct Void $[importc "void", nodecl]
struct CString $[importc "char*", nodecl]
struct Int32 $[importc "int32_t", header "stdint.h"]
struct Bool $[importc "bool", header "stdbool.h"]

fn `+(a Int32, b Int32) Int32 $[importc, nodecl, infix]
fn `==(a Int32, b Int32) Bool $[importc, nodecl, infix]
fn `printf(fmt CString, val Int32) $[importc, header "stdio.h"]
"""

let pointerprelude = """
protocol All {}

struct Pointer $[importc "void*", nodecl]
struct Ptr [T All] $[importc, nodecl, pattern "$1*"]

fn sizeof(T Typedesc) Int32 $[importc, nodecl]
fn alloc(size Int32) Pointer $[importc "malloc", header "stdlib.h"]
fn cast[F All](T Typedesc, value F) Ptr!T $[importc, nodecl, pattern "($1)$2"]
"""

suite "semantic":
  test "infix":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      1 + 1
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check fexprs[^1].typ.isSome
    check $fexprs[^1].typ.get == "testmodule.Int32"
  test "call":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      printf("%d", 9)
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check fexprs[^1].typ.isSome
    check $fexprs[^1].typ.get == "testmodule.Void"
  test "if else":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      if (1 == 1) {
        1
      } else {
        2
      }
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check fexprs[^1].typ.isSome
    check $fexprs[^1].typ.get == "testmodule.Int32"
  test "if elif else":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      if (1 == 1) {
        1
      } elif (1 == 2) {
        2
      } else {
        3
      }
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check fexprs[^1].typ.isSome
    check $fexprs[^1].typ.get == "testmodule.Int32"
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
