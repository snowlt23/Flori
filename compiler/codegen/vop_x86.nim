
import strutils, sequtils
import patty
import options
import ../fcore
import vop, asm_x86

variantp VOPAtom:
  IntLit(intval: int64)
  Reg(reg: Reg32)
  EbpRel(rel: int)
  Temp(name: string)

variantp VOP:
  Label(label: string)
  Mov(ml: VOPAtom, mr: VOPAtom)
  Add(al: VOPAtom, ar: VOPAtom)
  Sub(sl: VOPAtom, sr: VOPAtom)
  Lesser(ll: VOPAtom, lr: VOPAtom)
  Cmp(cl: VOPAtom, cr: VOPAtom)
  Je(jelabel: string)
  Jmp(jmplabel: string)

#
# VOPFn
#

type
  VOPFn* = object
    vops*: seq[VOP]
    tmpcnt*: int

proc initVOPFn*(): VOPFn =
  VOPFn(vops: @[], tmpcnt: 0)

proc gensym*(fn: var VOPFn, prefix = ".t"): string =
  result = prefix & $fn.tmpcnt
  fn.tmpcnt += 1
proc genlabel*(fn: var VOPFn, prefix = ".L"): string =
  fn.gensym(prefix)

proc add*(fn: var VOPFn, vop: VOP) =
  fn.vops.add(vop)

#
# VOP utils
#

proc `$`*(atom: VOPAtom): string =
  match atom:
    IntLit(x):
      $x
    Reg(r):
      $r
    EbpRel(rel):
      if rel < 0:
        "[ebp$#]" % $rel
      else:
        "[ebp+$#]" % $rel
    Temp(t):
      t

proc `$`*(vop: VOP): string =
  match vop:
    Label(l):
      "$#:" % l
    Mov(l, r):
      "mov $#, $#" % [$l, $r]
    Add(l, r):
      "add $#, $#" % [$l, $r]
    Sub(l, r):
      "sub $#, $#" % [$l, $r]
    Lesser(l, r):
      "lesser $#, $#" % [$l, $r]
    Cmp(l, r):
      "cmp $#, $#" % [$l, $r]
    Je(l):
      "je " & l
    Jmp(l):
      "jmp " & l

proc `$`*(fn: VOPFn): string =
  result = ""
  for vop in fn.vops:
    result &= "  " & $vop & "\n"

#
# VOP definitions
#

proc vop*(ctx: var VOPFn, f: FExpr): VOPAtom

proc eqInternal*(f: FExpr, op: InternalOp): bool =
  f.kind in fexprCalls and f.call.kind == fexprSymbol and f.call.symbol.fexpr.internal.obj.internalop == op

defineVOP vopInternalWord:
  pattern f.kind in fexprCalls and $f.call == "=>" and f.internal.obj.internalOp != internalNone
  generate:
    vopnone

defineVOP vopIntLit:
  pattern f.kind == fexprIntLit
  generate:
    vopret IntLit(f.intval)

defineVOP vopAddInt:
  pattern f.eqInternal(internalAdd)
  temps r
  generate:
    inst Mov(r, vop(f.args[0]))
    inst Add(r, vop(f.args[1]))
    vopret r

defineVOP vopLesser:
  pattern f.eqInternal(internalLess)
  temps r
  generate:
    inst Mov(r, vop(f.args[0]))
    inst Lesser(r, vop(f.args[1]))
    vopret r

defineVOP vopIf:
  pattern f.kind == fexprIf
  labels FL, EL
  generate:
    let cond = vop(f.ifbranch.args[0])
    inst Cmp(cond, IntLit(0))
    inst Je(FL)
    discard vop(f.ifbranch.args[1])
    inst Jmp(EL)
    label FL
    discard vop(f.elsebody)
    label EL
    vopnone
    
defineVOPSet:
  vopInternalWord
  vopIntLit
  vopAddInt
  vopLesser
  vopIf

