
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
  AVar(varname: VOPAtom, varsize: int)
  Mov(ml: VOPAtom, mr: VOPAtom)
  MovDerefL(mll: VOPAtom, mlr: VOPAtom)
  MovDerefR(mrl: VOPAtom, mrr: VOPAtom)
  Add(al: VOPAtom, ar: VOPAtom)
  Sub(sl: VOPAtom, sr: VOPAtom)
  Cmp(cl: VOPAtom, cr: VOPAtom)
  Je(jelabel: string)
  Jl(jllabel: string)
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
    AVar(n, size):
      "var $#<$#>" % [$n, $size]
    Mov(l, r):
      "mov $#, $#" % [$l, $r]
    MovDerefL(l, r):
      "movderef [$#], $#" % [$l, $r]
    MovDerefR(l, r):
      "movderef $#, [$#]" % [$l, $r]
    Add(l, r):
      "add $#, $#" % [$l, $r]
    Sub(l, r):
      "sub $#, $#" % [$l, $r]
    Cmp(l, r):
      "cmp $#, $#" % [$l, $r]
    Je(l):
      "je " & l
    Jl(l):
      "jl " & l
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

proc isStruct*(t: Symbol): bool =
  t.fexpr.internal.obj.isStruct

proc align*(size: int): int =
  size + (4 - size mod 4)

proc move*(ctx: var VOPFn, dst: VOPAtom, fexpr: FExpr) =
  let t = if fexpr.gettype.kind == symbolLink:
            fexpr.gettype.wrapped
          else:
            fexpr.gettype
  if t.isStruct:
    temps r1, r2
    let value = vop(fexpr)
    let movn = align(t.typesize) div 4
    for i in 0..<movn:
      inst Mov(r1, dst)
      inst Add(r1, IntLit(i*4))
      inst Mov(r2, value)
      inst Add(r2, IntLit(i*4))
      inst MovDerefR(r2, r2)
      inst MovDerefL(r1, r2)
  else:
    inst Mov(dst, vop(fexpr))

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
  labels TL, FL, EL
  temps r
  generate:
    inst Cmp(vop(f.args[0]), vop(f.args[1]))
    inst Jl(TL)
    inst Jmp(FL)
    label TL
    inst Mov(r, IntLit(1))
    inst Jmp(FL)
    label FL
    inst Mov(r, IntLit(0))
    inst Jmp(EL)
    label EL
    vopret r

defineVOP vopIf:
  pattern f.kind == fexprIf
  temps r
  labels FL, EL
  generate:
    inst AVar(r, f.gettype.typesize)
    let cond = vop(f.ifbranch.args[0])
    inst Cmp(cond, IntLit(0))
    inst Je(FL)
    instcall move(r, f.ifbranch.args[1])
    inst Jmp(EL)
    label FL
    instcall move(r, f.elsebody)
    label EL
    vopret r
    
defineVOPSet:
  vopInternalWord
  vopIntLit
  vopAddInt
  vopLesser
  vopIf

