
import strutils

type
  Reg32* = enum
    eax = 0
    ecx
    edx
    ebx
    esp
    ebp
    esi
    edi
  Mod* = enum
    modRegReg = 0
    modRegDisp8
    modRegDisp32
    modReg
  Scale* = enum
    scale1 = 0
    scale2
    scale4
    scale8

const DataReg32* = {ecx, edx, ebx}

proc sib*(s: Scale, index: Reg32, base: Reg32): uint8 =
  uint8((int(s) shl 6) + (int(index) shl 3) + int(base))
proc modrm*(m: Mod, reg: Reg32, rm: Reg32): uint8 =
  uint8((int(m) shl 6) + (int(reg) shl 3) + int(rm))

proc asmb*[B](b: var B, x: uint8) =
  b.add(x)
proc asmw[B](b: var B, x: int16) =
  let l1 = x and 255
  let l2 = (x shr 8) and 255
  b.asmb(uint8(l1))
  asmb(b, uint8(l2))
proc asmd[B](b: var B, x: int32) =
  let l1 = x and 255
  let l2 = (x shr 8) and 255
  let l3 = (x shr 16) and 255
  let l4 = (x shr 24) and 255
  asmb(b, uint8(l1))
  asmb(b, uint8(l2))
  asmb(b, uint8(l3))
  asmb(b, uint8(l4))

proc opImm*[B](b: var B, op: uint8, i: int32) =
  b.asmb(op)
  b.asmd(i)
proc opRegImm*[B](b: var B, op: uint8, r: Reg32, i: int32) =
  b.asmb(uint8(int(op)+int(r)))
  b.asmd(i)
proc opRegImmMod*[B](b: var B, op: uint8, m: int, r: Reg32, i: int32) =
  b.asmb(op)
  b.asmb(modrm(modReg, cast[Reg32](m), r))
  b.asmd(i)
proc opRegMod*[B](b: var B, op: uint8, m: int, r: Reg32) =
  b.asmb(op)
  b.asmb(modrm(modReg, r, cast[Reg32](m)))
proc opRegModDisp32*[B](b: var B, op: uint8, m: int, r: Reg32, disp: int32) =
  b.asmb(op)
  b.asmb(modrm(modRegDisp32, r, cast[Reg32](m)))
  b.asmd(disp)
proc opRegReg*[B](b: var B, op: uint8, r1: Reg32, r2: Reg32) =
  b.asmb(op)
  b.asmb(modrm(modReg, r2, r1))
proc opRegRegSibDisp32*[B](b: var B, op: uint8, r1: Reg32, r2: Reg32, disp: int32) =
  b.asmb(op)
  b.asmb(modrm(modRegDisp32, r1, r2))
  b.asmb(sib(scale1, cast[Reg32](0b100), r2))
  b.asmd(disp)
proc opRegDisp32Reg*[B](b: var B, op: uint8, r1: Reg32, disp: int32, r2: Reg32) =
  b.asmb(op)
  b.asmb(modrm(modRegDisp32, r2, r1))
  # b.asmb(sib(scale1, cast[Reg32](0b100), r2))
  b.asmd(disp)
proc opRegRegDisp32*[B](b: var B, op: uint8, r1: Reg32, r2: Reg32, disp: int32) =
  b.asmb(op)
  b.asmb(modrm(modRegDisp32, r1, r2))
  # b.asmb(sib(scale1, cast[Reg32](0b100), r2))
  b.asmd(disp)
proc opReg*[B](b: var B, op: uint8, r: Reg32) =
  b.asmb(uint8(int(op) + int(r)))

#
# Op
#

proc mov*[B](b: var B, r: Reg32, i: int32) =
  b.opRegImm(0xB8, r, i)
proc mov*[B](b: var B, r1: Reg32, r2: Reg32) =
  b.opRegReg(0x89, r1, r2)
proc mov*[B](b: var B, r1: Reg32, disp: int32, r2: Reg32) =
  b.opRegDisp32Reg(0x89, r1, disp, r2)
proc mov*[B](b: var B, r1: Reg32, r2: Reg32, disp: int32) =
  b.opRegRegDisp32(0x8B, r1, r2, disp)
  
proc add*[B](b: var B, r: Reg32, i: int32) =
  if r == eax:
    b.opRegImm(0x05, r, i)
  else:
    b.opRegImmMod(0x81, 0, r, i)
proc add*[B](b: var B, r1: Reg32, r2: Reg32) =
  b.opRegReg(0x01, r1, r2)
proc add*[B](b: var B, r1: Reg32, disp: int32, r2: Reg32) =
  b.opRegDisp32Reg(0x01, r1, disp, r2)
proc add*[B](b: var B, r1: Reg32, r2: Reg32, disp: int32) =
  b.opRegRegDisp32(0x03, r1, r2, disp)

proc sub*[B](b: var B, r: Reg32, i: int32) =
  if r == eax:
    b.opRegImm(0x2D, r, i)
  else:
    b.opRegImmMod(0x81, 5, r, i)
proc sub*[B](b: var B, r1: Reg32, r2: Reg32) =
  b.opRegReg(0x29, r1, r2)
proc sub*[B](b: var B, r1: Reg32, disp: int32, r2: Reg32) =
  b.opRegDisp32Reg(0x29, r1, disp, r2)
proc sub*[B](b: var B, r1: Reg32, r2: Reg32, disp: int32) =
  b.opRegRegDisp32(0x2B, r1, r2, disp)

proc cmp*[B](b: var B, r: Reg32, i: int32) =
  if r == eax:
    b.opRegImm(0x3D, r, i)
  else:
    b.opRegImmMod(0x81, 7, r, i)
proc cmp*[B](b: var B, r1: Reg32, r2: Reg32) =
  b.opRegReg(0x39, r1, r2)
proc cmp*[B](b: var B, r1: Reg32, disp: int32, r2: Reg32) =
  b.opRegDisp32Reg(0x39, r1, disp, r2)
proc cmp*[B](b: var B, r1: Reg32, r2: Reg32, disp: int32) =
  b.opRegRegDisp32(0x3B, r1, r2, disp)

proc jg*[B](b: var B, rel: int32) =
  b.asmb(0x0F)
  b.asmb(0x8F)
  b.asmd(rel)
proc jl*[B](b: var B, rel: int32) =
  b.asmb(0x0F)
  b.asmb(0x8C)
  b.asmd(rel)
proc jz*[B](b: var B, rel: int32) =
  b.asmb(0x0F)
  b.asmb(0x84)
  b.asmd(rel)
proc jmp*[B](b: var B, rel: int32) =
  b.asmb(0xE9)
  b.asmd(rel)

proc push*[B](b: var B, i: int32) =
  b.asmb(0x68)
  b.asmd(i)
proc push*[B](b: var B, r: Reg32) =
  b.opReg(0x50, r)
proc push*[B](b: var B, r: Reg32, disp: int32) =
  b.opRegModDisp32(0xFF, 6, r, disp)
proc pop*[B](b: var B, r: Reg32) =
  b.opReg(0x58, r)
proc pop*[B](b: var B, r: Reg32, disp: int32) =
  b.opRegModDisp32(0x8F, 0, r, disp)
  
proc callRel*[B](b: var B, rel: int32) =
  b.asmb(0xE8)
  b.asmd(rel)
proc ret*[B](b: var B) =
  b.asmb(0xC3)
proc enter*[B](b: var B, i: int16, ib: int8) =
  b.asmb(0xC8)
  b.asmw(i)
  b.asmb(uint8(ib))
proc leave*[B](b: var B) =
  b.asmb(0xC9)
