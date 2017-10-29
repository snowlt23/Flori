
type
  TSpec* = object
  TFun* = object
  SemTypeKind* = enum
    stRef
    stApply
    stVar
  SemType* = ref object
    case kind*: SemTypeKind
    of stRef:
      refer*: SemType
    of stApply:
      discard
    of stVar:
      parent*: SemType


