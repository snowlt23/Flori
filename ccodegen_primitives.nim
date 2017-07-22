
import tables

type
  CCodegenPrimitiveKind* = enum
    primitiveCall
    primitiveInfix
    primitiveType
  CCodegenPrimitive* = object
    kind*: CCodegenPrimitiveKind
    name*: string

var primitives* = initOrderedTable[string, CCodegenPrimitive]()
primitives["+_Int32_Int32"] = CCodegenPrimitive(kind: primitiveInfix, name: "+")
primitives["Int32"] = CCodegenPrimitive(kind: primitiveType, name: "int32_t")
primitives["Void"] = CCodegenPrimitive(kind: primitiveType, name: "void")
