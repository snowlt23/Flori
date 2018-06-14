
import tacode, asm_x86

import strutils, sequtils
import options

type
  X86AtomKind* = enum
    atomReg
    atomEspRel
    atomEbpRel
  X86Atom* = object
    case kind*: X86AtomKind
    of atomReg:
      reg*: Reg
    of atomEspRel, atomEbpRel:
      rel*: int  
  X86CodeKind* = enum
    codeMov
    codeAdd
    codeSub
    codeMul
    codeDiv
    codeSet
    codeCall
    codeRet
    codeJmp
    codeJmpGreater
    codeJmpLesser
  X86Code* = object
    case kind*: CodeX86Kind
    of {codeMov, codeAdd, codeSub, codeMul, codeDiv, codeSet}:
      leftreg*: X86Atom
      rightreg*: X86Atom
    of codeCall:
      calllabel*: int
    of codeRet:
      discard
    of {codeJmp, codeJmpGreater, codeJmpLesser}:
      gotolabel*: int
  X86Context* = object
    codes*: seq[X86Code]
    labels*: Table[string, int]
    revlabels*: Table[int, string]

proc `$`*(atom: X86Atom): string =
  case atom.kind
  of atomReg:
    $atom.reg
  of atomEspRel:
    "[esp-$#]" % $atom.rel
  of atomEbpRel:
    "[ebp-$#]" % $atom.rel
proc `$`*(code: X86Code): string =
  case code.kind
  of codeMov:
    "mov $#, $#" % [$code.leftreg, $code.rightreg]
  of codeAdd:
    "add $#, $#" % [$code.leftreg, $code.rightreg]
  of codeSub:
    "sub $#, $#" % [$code.leftreg, $code.rightreg]
  of codeMul:
    "mul $#, $#" % [$code.leftreg, $code.rightreg]
  of codeDiv:
    "div $#, $#" % [$code.leftreg, $code.rightreg]
  of codeCall:
    "call " & code.calllabel
  of codeRet:
    "ret"
  of codeJmp:
    "jmp " & code.gotolabel
  of codeJmpGreater:
    "jg " & code.gotolabel
  of codeJmpLesser:
    "jl " code.gotolabel

proc newX86Context*(): X86Context =
  X86Context(codes: @[], labels: initTable[string, int](), revlabels: initTable[int, string]())

proc addLabel*(ctx: var X86Context, labelname: string) =
  ctx.labels[labelname] = ctx.codes.len
  ctx.revlabels[ctx.codes.len] = labelname
  
proc `$`*(ctx: var X86Context): string =
  result = ""
  for i, code in ctx.codes:
    if ctx.revlabels.hasKey(i):
      result &= ctx.revlabels[i] & ":\n"
    result &= "  " & $code & "\n"

proc convertFromTACode*(ctx: var X86Context, code: TACode) =
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
      ctx.address[argname] = AsmAtom(kind: asmEbpRel, ebprel: 8 + i*4)
    ctx.buffer.push(ebp)
    ctx.buffer.mov(ebp, esp)
  of taCall:
    let labelpos = ctx.getLabelPos(code.fnlabel)
    let codepos = ctx.getCodePos() + 5
    ctx.buffer.callRel(int32(labelpos - codepos))
  of taVar:
    ctx.stacklen += code.size
    ctx.buffer.sub(esp, int32(code.size))
    ctx.address[code.varname] = AsmAtom(kind: asmEbpRel, ebprel: -ctx.stacklen)
    let valueaddr = ctx.address[$code.value]
    if valueaddr.kind == asmReg:
      ctx.buffer.mov(ebp, int32(-ctx.stacklen), valueaddr.reg)
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
    ctx.buffer.mov(esp, ebp)
    ctx.buffer.pop(ebp)
    ctx.buffer.ret()
    ctx.resetFn()
