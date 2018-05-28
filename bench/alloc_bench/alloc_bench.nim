
proc pint(x: int32): ref int32 =
  let i = new(int32)
  i[] = x
  return i

proc main() =
  for i in 1..1000000:
    var v = newSeq[ref int32](10)
    v.add(pint(1))
    v.add(pint(2))
    v.add(pint(3))
    v.add(pint(4))
    v.add(pint(5))

main()
