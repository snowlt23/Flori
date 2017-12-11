
import unittest
import tables
import options
import compiler.types, compiler.fexpr, compiler.parser
import compiler.scope, compiler.semantic, compiler.internal

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

suite "semantic":
  test "infix":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      1 + 1
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check fexprs[^1].typ.isSome
    check $fexprs[^1].typ.get == "testmodule.Int"
  test "call":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      printf("%d", 9)
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check fexprs[^1].typ.isSome
    check $fexprs[^1].typ.get == "testmodule.Void"
  test "if":
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
    check $fexprs[^1].typ.get == "testmodule.Int"
  test "while":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      while (1 == 2) {
        printf("%d", 9)
      }
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check $fexprs[^1].typ.get == "testmodule.Void"
    check $fexprs[^1][0] == "while"
    check fexprs[^1].internalMark == internalWhile
  test "toplevel def":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      nine := 9
      printf("%d", nine)
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check fexprs[^2].internalMark == internalDef
    check $fexprs[^2].typ.get == "testmodule.Void"
    check $fexprs[^1][1][1] == "nine"
    check $fexprs[^1][1][1].typ.get == "testmodule.Int"
  test "local def":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      fn test() {
        name := "feelsgoodman"
      }
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check fexprs[^1][3][0].internalMark == internalDef
    check $fexprs[^1][3][0].typ.get == "testmodule.Void"
  test "generics init":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      type Wrap[T] {
        x T
      }
      init(Wrap){9}
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check $fexprs[^1].typ.get.name == "Wrap"
    check $fexprs[^1].typ.get.types[0] == "testmodule.Int"
    check $fexprs[^1].typ.get == "testmodule.Wrap|(testmodule.Int)"
    let opt = semctx.modules[name("testmodule")].getDecl(name("Wrap_testmodule.Int"))
    check opt.isSome
  test "generics":
    let semctx = newSemanticContext()
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
    let wrapfn = fexprs[^1]
    check $wrapfn.typ.get == "testmodule.Wrap|(testmodule.Int)"
    let topt = semctx.modules[name("testmodule")].getDecl(name("Wrap_testmodule.Int"))
    check topt.isSome
  test "field access":
    let semctx = newSemanticContext()
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
    check $fexprs[^1].typ.get == "testmodule.Int"
  test "generics fn":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      type Wrap[T] {
        x T
      }
      fn wrap[T](x T) Wrap[T] {
        init(Wrap){x}
      }
      fn id[T](x Wrap[T]) Wrap[T] {
        x
      }
      id(wrap(9))
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check $fexprs[^1].typ.get == "testmodule.Wrap|(testmodule.Int)"
  test "recursion call":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", prelude & """
      fn fib(n Int) Int {
        if (n < 2) {
          n
        } else {
          fib(n-1) + fib(n-2)
        }
      }
      
      printf("%d\n", fib(38))
    """)
    semctx.evalModule(name("testmodule"), fexprs)
  test "import":
    let semctx = newSemanticContext()
    let fexprs = parseToplevel("testmodule.flori", """
      import core.prelude

      printf("%d\n", 9)
    """)
    semctx.evalModule(name("testmodule"), fexprs)
    check $fexprs[^1][0] == "core.prelude.printf"
  test "generics cannot instantiate":
    let semctx = newSemanticContext()
    expect(FExprError):
      let fexprs = parseToplevel("testmodule.flori", prelude & """
        type Wrap[T] {
          x T
        }
        fn wrap[T](x Int) Wrap[T] {
        }
        wrap(9)
      """)
      semctx.evalModule(name("testmodule"), fexprs)
  test "recsursion generics":
    let semctx = newSemanticContext()
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

