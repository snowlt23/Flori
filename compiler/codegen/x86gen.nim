
import x86code, asm_x86
import tables
import strutils
import options

type
  AsmContext*[B] = object
    buffer*: B
    labelpos*: Table[string, int]

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

template generateLeftRight*[B](ctx: var AsmContext[B], code: X86Code, variant: untyped, asmop: untyped) =
  if code.variant.left.kind == X86AtomKind.Reg and code.variant.right.kind == X86AtomKind.Reg:
    ctx.buffer.asmop(code.variant.left.reg.reg, code.variant.right.reg.reg)
  elif code.variant.left.kind == X86AtomKind.EbpRel and code.variant.right.kind == X86AtomKind.Reg:
    ctx.buffer.asmop(ebp, int32(code.variant.left.ebprel.rel), code.variant.right.reg.reg)
  elif code.variant.left.kind == X86AtomKind.EspRel and code.variant.right.kind == X86AtomKind.Reg:
    ctx.buffer.asmop(esp, int32(code.variant.left.esprel.rel), code.variant.right.reg.reg)
  elif code.variant.left.kind == X86AtomKind.Reg and code.variant.right.kind == X86AtomKind.EbpRel:
    ctx.buffer.asmop(code.variant.left.reg.reg, ebp, int32(code.variant.right.ebprel.rel))
  elif code.variant.left.kind == X86AtomKind.Reg and code.variant.right.kind == X86AtomKind.EspRel:
    ctx.buffer.asmop(code.variant.left.reg.reg, esp, int32(code.variant.right.esprel.rel))
  elif code.variant.left.kind == X86AtomKind.Reg and code.variant.right.kind == X86AtomKind.IntLit:
    ctx.buffer.asmop(code.variant.left.reg.reg, int32(code.variant.right.intlit.intval))
  else:
    raise newException(Exception, "unsupported $#:$# pattern in x86" % [$code.variant.left.kind, $code.variant.right.kind])

template generateValue*[B](ctx: var AsmContext[B], code: X86Code, variant: untyped, asmop: untyped) =
  if code.variant.value.kind == X86AtomKind.Reg:
    ctx.buffer.asmop(code.variant.value.reg.reg)
  elif code.variant.value.kind == X86AtomKind.EbpRel:
    ctx.buffer.asmop(ebp, int32(code.variant.value.ebprel.rel))
  elif code.variant.value.kind == X86AtomKind.EspRel:
    ctx.buffer.asmop(esp, int32(code.variant.value.esprel.rel))
  else:
    raise newException(Exception, "unsupported $# kind in x86" % [$code.variant.value.kind])

proc generateX86*[B](ctx: var AsmContext[B], code: X86Code) =
  case code.kind
  of X86CodeKind.Label:
    ctx.addLabel(code.label.name)
  of X86CodeKind.AVar:
    discard
  of X86CodeKind.Add:
    ctx.generateLeftRight(code, add, add)
  of X86CodeKind.Sub:
    ctx.generateLeftRight(code, sub, sub)
  of X86CodeKind.Mul:
    discard
  of X86CodeKind.ADiv:
    discard
  of X86CodeKind.Mov:
    if code.mov.left.kind == X86AtomKind.Reg and code.mov.right.kind == X86AtomKind.StrLit:
      let saddr = ctx.buffer.strlit(code.mov.right.strlit.strval)
      ctx.buffer.mov(code.mov.left.reg.reg, int32(ctx.buffer.baseaddr) + saddr)
      ctx.buffer.addStrLitReloc(saddr, ctx.buffer.len-4)
      return
    ctx.generateLeftRight(code, mov, mov)
  of X86CodeKind.Push:
    if code.push.value.kind == X86AtomKind.Reg:
      ctx.buffer.push(code.push.value.reg.reg)
    elif code.push.value.kind == X86AtomKind.EbpRel:
      ctx.buffer.mov(eax, ebp, int32(code.push.value.ebprel.rel))
      ctx.buffer.push(eax)
    elif code.push.value.kind == X86AtomKind.EspRel:
      ctx.buffer.mov(eax, esp, int32(code.push.value.esprel.rel))
      ctx.buffer.push(eax)
    elif code.push.value.kind == X86AtomKind.IntLit:
      ctx.buffer.push(int32(code.push.value.intlit.intval))
    elif code.push.value.kind == X86AtomKind.StrLit:
      let saddr = ctx.buffer.strlit(code.push.value.strlit.strval)
      ctx.buffer.push(int32(ctx.buffer.baseaddr) + saddr)
      ctx.buffer.addStrLitReloc(saddr, ctx.buffer.len-4)
    else:
      raise newException(Exception, "unsupported $# kind in x86.push" % [$code.push.value.kind])
  of X86CodeKind.Pop:
    if code.pop.value.kind == X86AtomKind.Reg:
      ctx.buffer.pop(code.pop.value.reg.reg)
    elif code.pop.value.kind == X86AtomKind.EbpRel:
      ctx.buffer.pop(ebp, int32(code.pop.value.ebprel.rel))
    elif code.pop.value.kind == X86AtomKind.EspRel:
      ctx.buffer.pop(esp, int32(code.pop.value.esprel.rel))
    else:
      raise newException(Exception, "unsupported $# kind in x86.pop" % [$code.pop.value.kind])
  of X86CodeKind.Cmp:
    if code.cmp.left.kind == X86AtomKind.EbpRel and code.cmp.right.kind == X86AtomKind.IntLit:
      ctx.buffer.cmp(ebp, int32(code.cmp.left.ebprel.rel), int32(code.cmp.right.intlit.intval))
      return
    ctx.generateLeftRight(code, cmp, cmp)
  of X86CodeKind.JmpGreater:
    ctx.buffer.jg(ctx.getRel(code.jmpgreater.label))
  of X86CodeKind.JmpLesser:
    ctx.buffer.jl(ctx.getRel(code.jmplesser.label))
  of X86CodeKind.JmpZero:
    ctx.buffer.jz(ctx.getRel(code.jmpzero.label))
  of X86CodeKind.Jmp:
    ctx.buffer.jmp(ctx.getRel(code.jmp.label))
  of X86CodeKind.Call:
    ctx.buffer.callRel(ctx.getRel(code.call.label))
  of X86CodeKind.FFICall:
    if code.fficall.address.isSome:
      ctx.buffer.mov(eax, int32(code.fficall.address.get))
      if code.fficall.internal:
        ctx.buffer.addInternalReloc(code.fficall.label, ctx.buffer.len-4)
      else:
        ctx.buffer.addDLLReloc(code.fficall.dll.get, code.fficall.ffiname, ctx.buffer.len-4)
      ctx.buffer.call(eax)
    else:
      ctx.buffer.callRel(ctx.getRel(code.fficall.label))
  of X86CodeKind.Ret:
    ctx.buffer.ret()

type
  DummyBuffer* = object
    len*: int
proc add*(buf: var DummyBuffer, v: uint8) =
  buf.len += 1
proc baseaddr*(buf: var DummyBuffer): int = 0
proc addDLLReloc*(buf: var DummyBuffer, dllname: string, fnname: string, address: int) = discard
proc addInternalReloc*(buf: var DummyBuffer, fnname: string, address: int) = discard
proc addStrLitReloc*(buf: var DummyBuffer, strpos: int32, address: int) = discard

proc generateX86*[B](ctx: var AsmContext[B], x86ctx: X86Context, plat: X86Platform): X86Platform =
  var tmpctx = newAsmContext(DummyBuffer(len: ctx.buffer.len))
  tmpctx.labelpos = plat.fnpostbl
  for fn in x86ctx.fns:
    tmpctx.addLabel(fn.name)
    for code in fn.body:
      if code.kind == X86CodeKind.Label:
        tmpctx.addLabel(code.label.name)
  for fn in x86ctx.fns:
    tmpctx.addLabel(fn.name)
    for code in fn.body:
      tmpctx.generateX86(code)
  ctx.labelpos = tmpctx.labelpos
  for fn in x86ctx.fns:
    for code in fn.body:
      ctx.generateX86(code)
  result = plat
  result.fnpostbl = tmpctx.labelpos
