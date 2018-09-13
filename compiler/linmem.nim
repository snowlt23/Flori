
import macros
import patty

var linmemPtr*: pointer
var linmemPos*: int
var linmemSize*: int

proc cmalloc*(size: int): pointer {.importc: "malloc", header: "stdlib.h".}
proc crealloc*(p: pointer, size: int): pointer {.importc: "realloc", header: "stdlib.h".}
proc cfree*(p: pointer) {.importc: "free", header: "stdlib.h".}

proc initLinmem*(size: int) =
  linmemPtr = cmalloc(size)
  # zeroMem(linmemPtr, size)
  if linmemPtr.isNil:
    raise newException(OSError, "linmem: can't alloc memory.")
  linmemPos = 0
  linmemSize = size
proc autoExtend(size: int) =
  if linmemPos+size >= linmemSize:
    let startsize = linmemSize
    while linmemPos+size >= linmemSize:
      linmemSize *= 2
    linmemPtr = crealloc(linmemPtr, linmemSize)
    if linmemPtr.isNil:
      raise newException(OSError, "linmem: can't realloc memory.")
    # zeroMem(cast[pointer](cast[int](linmemPtr) + startSize), linmemSize - startsize)
proc allocLinmem*(size: int): int =
  autoExtend(size)
  result = linmemPos
  linmemPos += size
proc getptr*(idx: int): pointer =
  cast[pointer](cast[int](linmemPtr) + idx)
proc destroyLinmem*() =
  cfree(linmemPtr)

template defineInternal*(name) =
  type name* = object
    index*: int

proc expandField(stmt: var NimNode, t: NimNode, fieldname: Nimnode, fieldtyp: NimNode) =
  let assignname = ident($fieldname & "=")
  stmt.add quote do:
    proc `fieldname`*(t: `t`): `fieldtyp` {.used.} = t.obj.`fieldname`
    proc `assignname`*(t: `t`, value: `fieldtyp`) {.used.} = t.obj.`fieldname` = value

macro expandFields*(t: typed, typ: typed): untyped =
  result = newStmtList()
  let impl = typ.symbol.getImpl()[2][2]
  for field in impl:
    if field.kind == nnkIdentDefs:
      let fieldname = if field[0].kind == nnkPostfix: field[0][1] else: field[0]
      let fieldtyp = field[1]
      result.expandField(t, fieldname, fieldtyp)
    elif field.kind == nnkRecCase:
      let fieldname = if field[0][0].kind == nnkPostfix: field[0][0][1] else: field[0][0]
      let fieldtyp = field[0][1]
      result.expandField(t, fieldname, fieldtyp)
      for i in 1..<field.len:
        field[i][^1].expectKind(nnkRecList)
        for j in 0..<field[i][^1].len:
          let f = field[i][^1][j]
          if f.kind != nnkIdentDefs: continue
          let fieldname = if f[0].kind == nnkPostfix: f[0][1] else: f[0]
          let fieldtyp = f[1]
          result.expandField(t, fieldname, fieldtyp)
    else:
      error("unexpected kind " & $field.kind, field)
  # echo result.repr

template implInternal*(name, typ) =
  proc `alloc name`*(): name =
    name(index: allocLinmem(sizeof(typ)))
  proc obj*(x: name): var typ =
    assert(x.index != -1)
    assert(x.index < linmemPos)
    cast[ptr typ](getptr(x.index))[]
  proc `obj=`*(x: name, value: typ) =
    assert(x.index != -1)
    assert(x.index < linmemPos)
    cast[ptr typ](getptr(x.index))[] = value
  proc `gen name`*(f: typ): name =
    result = `alloc name`()
    result.obj = f
  proc lcopy*(x: name): name =
    `gen name`(x.obj)
  expandFields(name, typ)

proc linmemBinary*(): string =
  result = ""
  for i in 0..<linmemPos:
    result.add(cast[ptr char](cast[int](linmemPtr) + i)[])

#
# internals
#

type
  IString* = object
    index: int
    len*: int
  IArray*[T] = object
    index: int
    len*: int
  IListObj*[T] = object
    value*: T
    next*: IList[T]
  IList*[T] = object
    index: int

proc istring*(s: string): IString =
  result = IString(index: allocLinmem(s.len+1), len: s.len)
  for i, c in s:
    cast[ptr char](getptr(result.index + i))[] = c
  cast[ptr char](getptr(result.index + s.len))[] = '\0'
proc `$`*(fs: IString): string =
  result = ""
  for i in 0..<fs.len:
    result.add(cast[ptr char](getptr(fs.index + i))[])
proc `==`*(a: IString, b: IString): bool =
  $a == $b
proc `==`*(a: IString, b: string): bool =
  $a == b

proc iarray*[T](len: int): IArray[T] =
  result = IArray[T](index: allocLinmem(sizeof(T)*len), len: len)
proc checkBounds*[T](arr: IArray[T], i: int) =
  when not defined(release):
    if i < 0 or i >= arr.len:
      raise newException(Exception, "index out of bounds: $#" % $i)
proc `[]`*[T](arr: IArray[T], i: int): T =
  arr.checkBounds(i)
  cast[ptr T](getptr(arr.index + i*sizeof(T)))[]
proc mget*[T](arr: IArray[T], i: int): var T =
  arr.checkBounds(i)
  cast[ptr T](getptr(arr.index + i*sizeof(T)))[]
proc `[]=`*[T](arr: IArray[T], i: int, val: T) =
  arr.checkBounds(i)
  cast[ptr T](getptr(arr.index + i*sizeof(T)))[] = val
proc iarray*[T](arr: openArray[T]): IArray[T] =
  result = iarray[T](arr.len)
  for i, e in arr:
    result[i] = e
proc iarray*[T](): IArray[T] =
  iarray[T]([])
iterator items*[T](arr: IArray[T]): T =
  for i in 0..<arr.len:
    yield(arr[i])
iterator mitems*[T](arr: IArray[T]): var T =
  for i in 0..<arr.len:
    yield(arr.mget(i))
iterator pairs*[T](arr: IArray[T]): (int, T) =
  for i in 0..<arr.len:
    yield(i, arr[i])
iterator mpairs*[T](arr: IArray[T]): (int, var T) =
  for i in 0..<arr.len:
    yield(i, arr.mget(i))

proc obj*[T](lst: IList[T]): var IListObj[T] =
  assert(lst.index != -1)
  assert(lst.index < linmemPos)
  cast[ptr IListObj[T]](getptr(lst.index))[]
proc value*[T](lst: IList[T]): var T =
  lst.obj.value
import typetraits
proc `value=`*[T](lst: IList[T], v: T) =
  echo name(type(v))
  lst.obj.value = v
proc next*[T](lst: IList[T]): var IList[T] =
  lst.obj.next
proc `next=`*[T](lst: IList[T], n: IList[T]) =
  lst.obj.next = n
proc ilist*[T](value: T, next: IList[T]): IList[T] =
  result = IList[T](index: allocLinmem(sizeof(IListObj[T])))
  result.value = value
  result.next = next
proc ilistNil*[T](): IList[T] =
  IList[T](index: -1)
proc isNil*[T](lst: IList[T]): bool =
  lst.index == -1
proc add*[T](lst: var IList[T], value: T) =
  lst = ilist(value, lst)
proc last*[T](lst: var IList[T]): var IList[T] =
  if lst.isNil:
    return lst
  var cur = lst
  while true:
    if cur.next.isNil:
      return cur.next
    cur = cur.next
  assert(false)
proc `last=`*[T](lst: var IList[T], n: IList[T]) =
  if lst.isNil:
    lst = n
    return
  var cur = lst
  while true:
    if cur.next.isNil:
      cur.next = n
      return
    cur = cur.next
  assert(false)
proc reverse*[T](lst: IList[T]): IList[T] =
  result = ilistNil[T]()
  for e in lst:
    result.add(e)
proc ilist*[T](arr: openArray[T]): IList[T] =
  result = ilistNil[T]()
  for e in arr:
    result.add(e)
  result = result.reverse()
iterator items*[T](lst: IList[T]): T =
  var cur = lst
  while true:
    if cur.isNil:
      break
    yield(cur.value)
    cur = cur.next
iterator mitems*[T](lst: IList[T]): var T =
  var cur = lst
  while true:
    if cur.isNil:
      break
    yield(cur.value)
    cur = cur.next
iterator pairs*[T](lst: IList[T]): (int, T) =
  var cur = lst
  var i = 0
  while true:
    if cur.isNil:
      break
    yield(i, cur.value)
    cur = cur.next
    i.inc
iterator mpairs*[T](lst: IList[T]): (int, var T) =
  var cur = lst
  var i = 0
  while true:
    if cur.isNil:
      break
    yield(i, cur.value)
    cur = cur.next
    i.inc
proc len*[T](lst: IList[T]): int =
  result = 0
  for e in lst:
    result.inc
proc `[]`*[T](lst: IList[T], idx: int): var T =
  assert(idx < lst.len)
  for i, e in lst.mpairs:
    if i == idx:
      return e
  assert(false)
proc `[]=`*[T](lst: IList[T], idx: int, value: T) =
  assert(idx < lst.len)
  for i, e in lst.mpairs:
    if i == idx:
      e = value
      return
  assert(false)

