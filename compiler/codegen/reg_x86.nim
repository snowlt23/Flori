
import strutils, sequtils
import tables
import patty
import options
import ../fcore
import vop, asm_x86, vop_x86

#
# naive register allocation
#

proc expandDoubleRef*(fn: var VOPFn, vop: VOP) =
  template expand(v, l, r) =
    if l.kind == VOPAtomKind.EbpRel and r.kind == VOPAtomKind.EbpRel:
      fn.add(Mov(Reg(eax), r))
      fn.add(v(l, Reg(eax)))
    else:
      fn.add(vop)
  match vop:
    Label(l):
      fn.add(vop)
    Mov(l, r):
      expand(Mov, l, r)
    Add(l, r):
      expand(Add, l, r)
    Sub(l, r):
      expand(Sub, l, r)
    Lesser(l, r):
      expand(Lesser, l, r)
    Cmp(l, r):
      expand(Cmp, l, r)
    Je(l):
      fn.add(vop)
    Jmp(l):
      fn.add(vop)

proc expandDoubleRef*(fn: VOPFn): VOPFn =
  result = initVOPFn()
  for vop in fn.vops:
    result.expandDoubleRef(vop)

proc naiveRegalloc*(temptbl: var Table[string, VOPAtom], name: string, stacksize: var int): VOPAtom =
  if not temptbl.hasKey(name):
    stacksize += 4
    temptbl[name] = EbpRel(-stacksize)
  return temptbl[name]

proc naiveRegalloc*(fn: var VOPFn, temptbl: var Table[string, VOPAtom], stacksize: var int, vop: VOP) =
  template reg(t): VOPAtom =
    if t.kind == VOPAtomKind.Temp:
      temptbl.naiveRegalloc(t.name, stacksize)
    else:
      t
  match vop:
    Label(l):
      fn.add(vop)
    Mov(l, r):
      fn.add(Mov(reg(l), reg(r)))
    Add(l, r):
      fn.add(Add(reg(l), reg(r)))
    Sub(l, r):
      fn.add(Sub(reg(l), reg(r)))
    Lesser(l, r):
      fn.add(Lesser(reg(l), reg(r)))
    Cmp(l, r):
      fn.add(Cmp(reg(l), reg(r)))
    Je(l):
      fn.add(vop)
    Jmp(l):
      fn.add(vop)

proc naiveRegalloc*(fn: VOPFn): VOPFn =
  result = initVopFn()
  var temptbl = initTable[string, VOPAtom]()
  var stacksize = 0
  for vop in fn.vops:
    result.naiveRegalloc(temptbl, stacksize, vop)
  result = result.expandDoubleRef()

#
# graph register allocation
#

