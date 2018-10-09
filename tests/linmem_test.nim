
import unittest
import ../compiler/linmem

defineInternal(MyList)
type MyListObj = object
  lst: IList[string]
implInternal(MyList, MyListObj)

suite "linmem":
  initLinmem(defaultLinmemSpace)
  test "ilist":
    var lst = ilistNil[string]()
    for i in 0..<100:
      lst.add($i)
    check $lst.value == "99"
    check $lst.next.value == "98"
    check $lst.next.next.value == "97"
  test "obj":
    let lst = genMyList(MyListObj(lst: ilistNil[string]()))
    for i in 0..<100:
      lst.obj.lst.add($i)
    check $lst.lst.value == "99"
    check $lst.lst.next.value == "98"
    check $lst.lst.next.next.value == "97"

