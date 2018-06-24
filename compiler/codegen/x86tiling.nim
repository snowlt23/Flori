
import opselect
import tacode, x86code, asm_x86
import sequtils

defTile tileX86Label:
  PATTERN:
    TACodeKind.Label
  CODE:
    initX86CodeLabel(code1.label.name)

defTile tileX86Add:
  PATTERN:
    TACodeKind.Add
  CODE:
    initX86CodeAVar(code1.add.name, 4)
    initX86CodeMov(initX86AtomTemp(code1.add.name), toX86Atom(code1.add.left))
    initX86CodeAdd(initX86AtomTemp(code1.add.name), toX86Atom(code1.add.right))

defTile tileX86Sub:
  PATTERN:
    TACodeKind.Sub
  CODE:
    initX86CodeAVar(code1.sub.name, 4)
    initX86CodeMov(initX86AtomTemp(code1.sub.name), toX86Atom(code1.sub.left))
    initX86CodeSub(initX86AtomTemp(code1.sub.name), toX86Atom(code1.sub.right))

defTile tileX86Mul:
  PATTERN:
    TACodeKind.Mul
  CODE:
    initX86CodeAVar(code1.mul.name, 4) # FIXME:
    initX86CodeMov(initX86AtomReg(eax), toX86Atom(code1.mul.left))
    initX86CodeMul(toX86Atom(code1.mul.right))
    initX86CodeMov(initX86AtomTemp(code1.mul.name), initX86AtomReg(eax))

defTile tileX86ADiv:
  PATTERN:
    TACodeKind.ADiv
  CODE:
    initX86CodeAVar(code1.adiv.name, 4) # FIXME:
    initX86CodeMov(initX86AtomReg(eax), toX86Atom(code1.adiv.left))
    initX86CodeADiv(toX86Atom(code1.adiv.right))
    initX86CodeMov(initX86AtomTemp(code1.adiv.name), initX86AtomReg(eax))

defTile tileX86Greater:
  PATTERN:
    TACodeKind.Greater
  CODE:
    CODEBLOCK:
      let tlabel = buf.tmplabel
      let nlabel = buf.tmplabel
    initx86CodeAVar(code1.greater.name, 4)
    initx86CodeCmp(tox86atom(code1.greater.left), tox86atom(code1.greater.right))
    initx86CodeJmpLesser(tlabel)
    initx86CodeMov(initx86atomtemp(code1.greater.name), initx86atomintlit(0))
    initx86CodeJmp(nlabel)
    initx86CodeLabel(tlabel)
    initx86CodeMov(initx86atomtemp(code1.greater.name), initx86atomintlit(1))
    initx86CodeLabel(nlabel)

defTile tileX86Lesser:
  PATTERN:
    TACodeKind.Lesser
  CODE:
    CODEBLOCK:
      let tlabel = buf.tmplabel
      let nlabel = buf.tmplabel
    initX86CodeAVar(code1.lesser.name, 4)
    initX86CodeCmp(tox86atom(code1.lesser.left), tox86atom(code1.lesser.right))
    initX86CodeJmpLesser(tlabel)
    initX86CodeMov(initx86atomtemp(code1.lesser.name), initx86atomintlit(0))
    initX86CodeJmp(nlabel)
    initX86CodeLabel(tlabel)
    initX86CodeMov(initx86atomtemp(code1.lesser.name), initx86atomintlit(1))
    initX86CodeLabel(nlabel)

defTile tileX86Set:
  PATTERN:
    TACodeKind.Set
  CODE:
    initX86CodeMov(initX86AtomTemp(code1.set.name), toX86Atom(code1.set.value))

defTile tileX86Call:
  PATTERN:
    TACodeKind.Call
  CODE:
    initX86CodeAVar(code1.call.name, 4, true) # FIXME:
    initX86CodeCall(code1.call.calllabel, code1.call.args.mapIt(toX86Atom(it)))
    initX86CodeMov(initX86AtomTemp(code1.call.name), initX86AtomReg(eax))

defTile tileX86AVar:
  PATTERN:
    TACodeKind.AVar
  CODE:
    initX86CodeAVar(code1.avar.name, code1.avar.size)
    CODEBLOCK:
      if code1.avar.value.kind != TAAtomKind.None:
        addCode(initX86CodeMov(initX86AtomTemp(code1.avar.name), toX86Atom(code1.avar.value)))

defTile tileX86Goto:
  PATTERN:
    TACodekind.Goto
  CODE:
    initX86CodeJmp(code1.goto.gotolabel)

defTile tileX86AIf:
  PATTERN:
    TACodeKind.AIf
  CODE:
    initX86CodeCmp(toX86Atom(code1.aif.cond), initX86AtomIntLit(1))
    initX86CodeJmpZero(code1.aif.gotolabel)

defTile tileX86Ret:
  PATTERN:
    TACodeKind.Ret
  CODE:
    initX86CodeMov(initX86AtomReg(eax), toX86Atom(code1.ret.value))
    initX86CodeRet()

defTile tileX86LesserIfToGreaterIf: # FIXME:
  PATTERN:
    TACodeKind.Lesser
    TACodeKind.AIf
    TACodeKind.Goto
    TACodeKind.Label
  MATCH:
    code1.lesser.right.kind == TAAtomKind.IntLit
    code2.aif.cond.kind == TAAtomKind.AVar
    code1.lesser.name == code2.aif.cond.avar.name
    code2.aif.gotolabel == code4.label.name
  CODE:
    initX86CodeCmp(toX86Atom(code1.lesser.left), initX86AtomIntLit(code1.lesser.right.intlit.intval-1))
    initX86CodeJmpGreater(code3.goto.gotolabel)

defTile tileX86GreaterIf: # FIXME:
  PATTERN:
    TACodeKind.Greater
    TACodeKind.AIf
  MATCH:
    code2.aif.cond.kind == TAAtomKind.AVar
    code1.greater.name == code2.aif.cond.avar.name
  CODE:
    initX86CodeCmp(tox86atom(code1.greater.left), tox86atom(code1.greater.right))
    initX86CodeJmpLesser(code2.aif.gotolabel)

defTile tileX86LesserIf: # FIXME:
  PATTERN:
    TACodeKind.Lesser
    TACodeKind.AIf
  MATCH:
    code2.aif.cond.kind == TAAtomKind.AVar
    code1.lesser.name == code2.aif.cond.avar.name
  CODE:
    initX86CodeCmp(tox86atom(code1.lesser.left), tox86atom(code1.lesser.right))
    initX86CodeJmpLesser(code2.aif.gotolabel)

defTile tileX86CallAddRet:
  PATTERN:
    TACodeKind.Call
    TACodeKind.Add
    TACodeKind.Ret
  MATCH:
    code2.add.right.kind == TAAtomKind.AVar and code1.call.name == code2.add.right.avar.name
  CODE:
    initX86CodeCall(code1.call.calllabel, code1.call.args.mapit(toX86Atom(it)))

    initX86CodeAdd(initX86AtomReg(eax), toX86Atom(code2.add.left))
    initX86CodeRet()

defTile tileX86AddRet:
  PATTERN:
    TACodeKind.Add
    TACodeKind.Ret
  CODE:
    initX86CodeMov(initX86AtomReg(eax), toX86Atom(code1.add.left))
    initX86CodeAdd(initX86AtomReg(eax), toX86Atom(code1.add.right))
    initX86CodeRet()

defTile tileX86SubRet:
  PATTERN:
    TACodeKind.Sub
    TACodeKind.Ret
  CODE:
    initX86CodeMov(initX86AtomReg(eax), toX86Atom(code1.sub.left))
    initX86CodeSub(initX86AtomReg(eax), toX86Atom(code1.sub.right))
    initX86CodeRet()

defTileset x86Tilingset:
  # 4
  tileX86LesserIfToGreaterIf

  # 3
  tileX86CallAddRet

  # 2
  tileX86GreaterIf
  tileX86LesserIf
  tileX86AddRet
  tileX86SubRet

  # 1
  tileX86Label
  tileX86Add
  tileX86Sub
  tileX86Mul
  tileX86ADiv
  tileX86Greater
  tileX86Lesser
  tileX86Set
  tileX86Call
  tileX86AVar
  tileX86Goto
  tileX86AIf
  tileX86Ret

type TileTAFn* = object
  codes*: seq[TACode]
  pos*: int
proc `[]`*(fn: TileTAFn, i: int): TACode = fn.codes[i]
proc len*(fn: TileTAFn): int = fn.codes.len

proc x86Tiling*(ctx: TAContext): X86Context =
  result = newX86Context()
  for fn in ctx.fns:
    var fnctx = newX86Context()
    var tilefn = TileTAFn(codes: fn.body, pos: 0)
    while tilefn.pos < tilefn.len:
      x86Tilingset(fnctx, tilefn)
    result.fns.add(X86Fn(name: fn.fnname, args: fn.args, body: fnctx.codes))
