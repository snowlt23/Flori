
import tacode, asm_x86
import tables
import strutils

type
  AsmAtomKind* = enum
    asmReg
    asmEspRel
    asmEbpRel
  AsmAtom* = object
    case kind*: AsmAtomKind
    of asmReg:
      reg*: Reg32
    of asmEspRel:
      esprel*: int
    of asmEbpRel:
      ebprel*: int
  AsmContext*[B] = object
    buffer*: B
    regs*: set[Reg32]
    stacklen*: int
    labelpos*: Table[string, int]
    address*: Table[string, AsmAtom]

proc newAsmContext*[B](b: B): AsmContext[B] =
  AsmContext[B](buffer: b, stacklen: 0, labelpos: initTable[string, int](), address: initTable[string, AsmAtom]())

proc tmpreg*[B](ctx: var AsmContext[B]): AsmAtom =
  for r in DataReg32:
    if r notin ctx.regs:
      ctx.regs.incl(r)
      return AsmAtom(kind: asmReg, reg: r)
  ctx.stacklen += 4
  result = AsmAtom(kind: asmEspRel, esprel: ctx.stacklen)

proc addLabel*[B](ctx: var AsmContext[B], label: string) =
  ctx.labelpos[label] = ctx.buffer.len
proc getCodePos*[B](ctx: var AsmContext[B]): int =
  ctx.buffer.len
proc getLabelPos*[B](ctx: var AsmContext[B], label: string): int =
  ctx.labelpos[label]

proc resetFn*[B](ctx: var AsmContext[B]) =
  for r in DataReg32:
    ctx.regs.excl(r)
  ctx.stacklen = 0

template generateOp*[B](ctx: var AsmContext[B], code: TACode, asmop: untyped) =
  let tmp = ctx.tmpreg()
  ctx.address[code.dstname] = tmp
  if code.left.kind in {atVar, atArg} and code.right.kind in {atVar, atArg}:
    let left = ctx.address[$code.left]
    let right = ctx.address[$code.right]
    ctx.buffer.mov(tmp.reg, left.reg)
    if right.kind == asmReg:
      ctx.buffer.asmop(tmp.reg, right.reg)
    else:
      ctx.buffer.asmop(tmp.reg, ebp, int32(right.ebprel))
  elif code.left.kind in {atVar, atArg} and code.right.kind == atIntLit:
    let left = ctx.address[$code.left]
    if left.kind == asmReg:
      ctx.buffer.mov(tmp.reg, left.reg)
    else:
      ctx.buffer.mov(tmp.reg, ebp, int32(left.ebprel))
      ctx.buffer.asmop(tmp.reg, int32(code.right.intval))
  else:
    raise newException(Exception, "error in $#: $#" % [astToStr(asmop), $code])
  
proc generateFromTACode*[B](ctx: var AsmContext[B], code: TACode) =
  case code.kind
  of taAdd:
    ctx.generateOp(code, add)
  of taSub:
    ctx.generateOp(code, sub)
  of taMul:
    discard
  of taDiv:
    discard
  of taGreater:
    discard
  of taLess:
    discard
  of taSet:
    discard
  of taFn:
    ctx.addLabel(code.fnname)
    for i in 0..<code.fnargs.len:
      let (argname, argsize) = code.fnargs[i]
      ctx.address["$" & $i] = AsmAtom(kind: asmEbpRel, ebprel: 8 + i*4)
  of taCall:
    let labelpos = ctx.getLabelPos(code.fnlabel)
    let codepos = ctx.getCodePos()
    ctx.buffer.callRel(int32(labelpos - codepos))
  of taVar:
    discard
  of taGoto:
    discard
  of taIf:
    discard
  of taRet:
    case code.ret.kind:
    of atNone:
      discard
    of atVar, atArg:
      let adr = ctx.address[$code.ret]
      case adr.kind
      of asmReg:
        ctx.buffer.mov(eax, adr.reg)
      of asmEspRel:
        ctx.buffer.mov(eax, esp, int32(adr.esprel))
      of asmEbpRel:
        ctx.buffer.mov(eax, ebp, int32(adr.ebprel))
    of atIntLit:
      ctx.buffer.mov(eax, int32(code.ret.intval))
    ctx.buffer.ret()
    ctx.resetFn()
