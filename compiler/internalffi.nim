
import tables

var ffitable = initTable[string, pointer]()

proc getInternalFFI*(name: string): pointer =
  if ffitable.hasKey(name):
    ffitable[name]
  else:
    nil
proc addInternalFFIPointer*(name: string, p: pointer) =
  ffitable[name] = p
template addInternalFFI*(name: string, p) =
  addInternalFFIPointer(name, cast[pointer](p))
