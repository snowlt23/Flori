
import macros
import patty

var linmem*: seq[uint8]

proc initLinmem*(size: int) =
  linmem = newSeq[uint8](size)

template defineInternal*(name) =
  type name* = object
    index: int

template implInternal*(name, typ) =
  proc `alloc name`*(): name =
    result = name(index: linmem.len)
    for i in 0..<sizeof(typ):
      linmem.add(0)
  proc obj*(x: name): var typ =
    cast[ptr typ](addr(linmem[x.index]))[]
  proc `obj=`*(x: name, value: typ) =
    cast[ptr typ](addr(linmem[x.index]))[] = value
  proc `gen name`*(f: typ): name =
    result = `alloc name`()
    result.obj = f

proc linmemBinary*(): string =
  result = ""
  for c in linmem:
    result.add(char(c))

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
  result = IString(index: linmem.len, len: s.len)
  for c in s:
    linmem.add(uint8(c))
  linmem.add(0)
proc `$`*(fs: IString): string =
  result = ""
  for i in 0..<fs.len:
    result.add(char(linmem[fs.index + i]))
proc `==`*(a: IString, b: IString): bool =
  $a == $b
proc `==`*(a: IString, b: string): bool =
  $a == b

proc iarray*[T](len: int): IArray[T] =
  result = IArray[T](index: linmem.len, len: len)
  for i in 0..<len * sizeof(T):
    linmem.add(0)
proc checkBounds*[T](arr: IArray[T], i: int) =
  when not defined(release):
    if i < 0 or i >= arr.len:
      raise newException(Exception, "index out of bounds: $#" % $i)
proc `[]`*[T](arr: IArray[T], i: int): T =
  arr.checkBounds(i)
  cast[ptr T](addr(linmem[arr.index + i*sizeof(T)]))[]
proc mget*[T](arr: IArray[T], i: int): var T =
  arr.checkBounds(i)
  cast[ptr T](addr(linmem[arr.index + i*sizeof(T)]))[]
proc `[]=`*[T](arr: IArray[T], i: int, val: T) =
  arr.checkBounds(i)
  cast[ptr T](addr(linmem[arr.index + i*sizeof(T)]))[] = val
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

proc ilist*[T](value: T, next: IList[T]): IList[T] =
  result = IList[T](index: linmem.len)

  for i in 0..<sizeof(IListObj[T]):
    linmem.add(0)

  let p = cast[ptr IListObj[T]](addr(linmem[result.index]))
  p[].value = value
  p[].next = next
proc obj*[T](lst: IList[T]): var IListObj[T] =
  let p = cast[ptr IListObj[T]](addr(linmem[lst.index]))
  p[]
proc value*[T](lst: IList[T]): var T =
  let p = cast[ptr IListObj[T]](addr(linmem[lst.index]))
  p[].value
proc next*[T](lst: IList[T]): var IList[T] =
  let p = cast[ptr IListObj[T]](addr(linmem[lst.index]))
  p[].next
proc ilistNil*[T](): IList[T] =
  IList[T](index: -1)
proc isNil*[T](lst: IList[T]): bool =
  lst.index == -1
proc add*[T](lst: var IList[T], value: T) =
  lst = ilist(value, lst)
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
proc len*[T](lst: IList[T]): int =
  result = 0
  for e in lst:
    result.inc

