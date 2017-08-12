
import unittest
import ../sast
import ../sparser
import ../semantic

const preincludesrc = """
(c-type Void :name "void" :nodecl)
(c-type Int32 :name "int32_t" :header "stdint.h")
(c-type Bool :name "bool" :header "stdbool.h")
(c-type CString :name "char*" :nodecl)
(c-type Size :name "size_t" :nodecl)

(defprotocol All)
@(^ (:a All))
(defprotocol Destructable
  (destructor (: :a)))

(c-type Pointer :name "void*" :nodecl)
@(^ (:a All))
(c-type (Ptr :a) :name "$1*" :nodecl)
@(^ (:a All) (:b All))
@(: :a :b -> :a)
(c-func cast :pattern "(($1)$2)" :nodecl)
@(^ (:a All))
@(: (Ptr :a) Size -> :a)
(c-func nth :pattern "$1[$2]" :nodecl)
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
  test "generics type primitive function":
    let semexpr = evalSExpr("""
    (cast Pointer 1)
    """)
    check semexpr.typesym.name == "Pointer"
  test "generics type and generics function":
    let semexpr = evalSExpr("""
    @(^ (:a All))
    (defstruct (Vec :a)
      (data (Ptr :a)))

    @(^ (:a All))
    @(: (Typedesc :a) -> (Vec :a))
    (defn vec [T]
      (construct (Vec :a)
        :data (cast (Ptr :a) 1)))

    @(^ (:a All))
    @(: &(Vec :a) :a)
    (defn push [vec value])

    (var v (vec Int32))
    (push v 1)
    v
    """)
    check semexpr.typesym.semexpr.kind == semanticStruct
    check semexpr.typesym.name == "Vec"
    check semexpr.typesym.semexpr.struct.argtypes[0].name == "Int32"
  test "generics type and generics function":
    let semexpr = evalSExpr("""
    @(^ (:a All))
    (defstruct (Vec :a)
      (data (Ptr :a)))

    @(^ (:a All))
    @(: (Typedesc :a) -> (Vec :a))
    (defn vec [T]
      (construct (Vec :a)
        :data (cast (Ptr :a) 1)))

    (var v (vec (Vec Int32)))
    v
    """)
    check semexpr.typesym.semexpr.kind == semanticStruct
    check semexpr.typesym.name == "Vec"
    check semexpr.typesym.semexpr.struct.argtypes[0].name == "Vec"
    check semexpr.typesym.semexpr.struct.argtypes[0].semexpr.kind == semanticStruct
    check semexpr.typesym.semexpr.struct.argtypes[0].semexpr.struct.argtypes[0].name == "Int32"
  test "destructable":
    let semexpr = evalSExpr("""
    @(^ (:a All))
    (defstruct (Vec :a)
      (data (Ptr :a)))

    @(^ (:a All))
    @(: (Typedesc :a) -> (Vec :a))
    (defn vec [T]
      (construct (Vec :a)
        :data (cast (Ptr :a) 1)))

    @(^ (:a Destructable))
    @(: :a)
    (defn destroy [val]
      (destructor val))
    @(: (Vec Int32))
    (defn destructor [x])

    (var v (vec Int32))
    v
    """)
    check semexpr.typesym.semexpr.kind == semanticStruct
    check semexpr.typesym.name == "Vec"
    check semexpr.typesym.semexpr.struct.argtypes[0].name == "Int32"
  test "generics destructor":
    let semexpr = evalSExpr("""
    @(^ (:a All))
    (defstruct (Vec :a)
      (data (Ptr :a)))

    @(^ (:a All))
    @(: (Typedesc :a) -> (Vec :a))
    (defn vec [T]
      (construct (Vec :a)
        :data (cast (Ptr :a) 1)))

    @(^ (:a Destructable))
    @(: (Vec :a))
    (defn destructor [vec]
      (destructor (nth (vec .data) (cast Size 0))))
    @(: Int32)
    (defn destructor [x])

    @(:)
    (defn main []
      (var v (vec Int32)))
    """)
    check semexpr.typesym.semexpr.kind == semanticStruct
    # check semexpr.typesym.name == "Vec"
    # check semexpr.typesym.semexpr.struct.argtypes[0].name == "Vec"
    # check semexpr.typesym.semexpr.struct.argtypes[0].semexpr.kind == semanticStruct
    # check semexpr.typesym.semexpr.struct.argtypes[0].semexpr.struct.argtypes[0].name == "Int32"
