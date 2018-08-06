
import asm_x86, vop_x86
import tables
import strutils
import options
import patty

type
  AsmContext*[B] = object
    buffer*: B
    labelpos*: Table[string, int]
  DummyBuffer* = object
    len*: int

#
# AsmContext
#

proc newAsmContext*[B](b: B): AsmContext[B] =
  AsmContext[B](buffer: b, labelpos: initTable[string, int]())

proc addLabel*[B](ctx: var AsmContext[B], label: string) =
  ctx.labelpos[label] = ctx.buffer.len
proc getCodePos*[B](ctx: var AsmContext[B]): int =
  ctx.buffer.len
proc getLabelPos*[B](ctx: var AsmContext[B], label: string): int =
  ctx.labelpos[label]
proc getRel*[B](ctx: var AsmContext[B], label: string): int32 =
  int32(ctx.getLabelPos(label) - ctx.getCodePos())

#
# DummyBuffer
#

proc add*(buf: var DummyBuffer, v: uint8) =
  buf.len += 1

#
# generates
#

template genleftright*[B](ctx: var AsmContext[B], asmop, l, r) =
  if l.kind == VOPAtomKind.Reg and r.kind == VOPAtomKind.Reg:
    ctx.buffer.asmop(l.reg, r.reg)
  elif l.kind == VOPAtomKind.EbpRel and r.kind == VOPAtomKind.Reg:
    ctx.buffer.asmop(ebp, int32(l.rel), r.reg)
  elif l.kind == VOPAtomKind.Reg and r.kind == VOPAtomKind.EbpRel:
    ctx.buffer.asmop(l.reg, ebp, int32(r.rel))
  elif l.kind == VOPAtomKind.Reg and r.kind == VOPAtomKind.IntLit:
    ctx.buffer.asmop(l.reg, int32(r.intval))
  else:
    raise newException(Exception, "unsupported $#:$# pattern in x86" % [$l.kind, $r.kind])

template genvalue*[B](ctx: var AsmContext[B], asmop, value) =
  if value.kind == X86AtomKind.Reg:
    ctx.buffer.asmop(value.reg)
  elif code.variant.value.kind == X86AtomKind.EbpRel:
    ctx.buffer.asmop(ebp, int32(value.rel))
  elif code.variant.value.kind == X86AtomKind.EspRel:
    ctx.buffer.asmop(esp, int32(value.rel))
  else:
    raise newException(Exception, "unsupported $# pattern in x86" % [$code.variant.value.kind])

proc generateX86*[B](ctx: var AsmContext[B], vop: VOP) =
  match vop:
    Label(l):
      ctx.addLabel(l)
    AVar(n, size):
      discard
    Mov(l, r):
      ctx.genleftright(mov, l, r)
    MovDerefL(l, r):
      assert(false)
    MovDerefR(l, r):
      assert(false)
    Add(l, r):
      ctx.genleftright(add, l, r)
    Sub(l, r):
      ctx.genleftright(sub, l, r)
    Cmp(l, r):
      ctx.genleftright(cmp, l, r)
    Je(l):
      assert(false)
    Jl(l):
      ctx.buffer.jl(ctx.getRel(vop.jllabel))
    Jmp(l):
      ctx.buffer.jmp(ctx.getRel(vop.jmplabel))

proc generateX86*[B](ctx: var AsmContext[B], fn: VOPFn) =
  # pre label position calculate
  var tmpctx = newAsmContext(DummyBuffer(len: ctx.buffer.len))
  for vop in fn.vops:
    if vop.kind == VOPKind.Label:
      tmpctx.addLabel(vop.label)
  for vop in fn.vops:
    tmpctx.generateX86(vop)

  # generate
  for vop in fn.vops:
    ctx.generateX86(vop)
