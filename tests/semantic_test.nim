
import unittest
import ../sast
import ../sparser
import ../semantic

const preincludesrc = """
(defprotocol All)
(c-type Void :name "void" :nodecl)
(c-type Int32 :name "int32_t" :header "stdint.h")
(c-type Bool :name "bool" :header "stdbool.h")
(c-type CString :name "char*" :nodecl)
"""

proc evalSExpr(src: string): SemanticExpr =
  var semcontext = newSemanticContext()
  var module = newModule(semcontext, "test")
  var scope = newScope(module)
  scope.predefine()
  let sexprs = parseToplevel("test.flori", preincludesrc & "\n" & src)
  for sexpr in sexprs:
    result = scope.evalSExpr(sexpr)

suite "Semantic":
  test "function":
    let semexpr = evalSExpr("""
    @(: Bool -> Bool)
    (defn id [b]
      b)
    """)
    check semexpr.kind == semanticSymbol
    check semexpr.symbol.semexpr.kind == semanticFunction
    check semexpr.symbol.semexpr.typesym.name == "Bool"
  test "generics function":
    let semexpr = evalSExpr("""
    @(^ (:a All))
    @(: :a -> :a)
    (defn id [x]
      x)
    (id 1)
    """)
    check semexpr.typesym.name == "Int32"
  test "generics type function":
    let semexpr = evalSExpr("""
    @(^ (:a All))
    (defstruct (Wrap :a)
      (data :a))
    @(^ (:a All))
    @(: (Wrap :a) -> :a)
    (defn get [x]
      (x .data))
    (get (construct (Wrap Int32) :data 1))
    """)
    check semexpr.typesym.name == "Int32"
