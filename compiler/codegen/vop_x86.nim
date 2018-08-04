
import patty
import asm_x86

variant AtomX86:
  IntLit(intval: int)
  Temp(name: string)
  Reg(reg: Reg32, offset: Option[int])

variant VOPX86:
  Label(label: string)
  Mov(ml: AtomX86, mr: AtomX86)
  Add(al: AtomX86, ar: AtomX86)
  Sub(sl, AtomX86, sr: AtomX86)
  Lesser(ll, AtomX86, lr: AtomX86)

proc `$`*(atom: AtomX86): string =
  match atom:
    IntLit(x):
      $x
    Temp(t):
      t
    Reg(r, o):
      if o.isSome:
        "[$#+$#]" % [$r, $o.get]
      else:
        $r

proc `$`*(vop: VOPX86): string =
  match vop:
    Label(l):
      "$#:" % l
    Mov(l, r):
      "mov $#, $#" % [$l, $r]
    Add(l, r):
      "add $#, $#" % [$l, $r]
    Lesser(l, r):
      "lesser $#, $#" % [$l, $r]

template defineVOP(name, body) = defineVOP(name, VOPX86, AtomX86, body)

defienVOP vopIntAdd:
  pattern f.eqInternal(internalAdd)

  generate:
    inst Add(vop(f.args[0]), vop(f.args[1]))
    vopret 

