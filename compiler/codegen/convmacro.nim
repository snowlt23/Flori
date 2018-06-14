
import strutils, sequtils
import macros
import tables
import options

import asm_x86

var convtable {.compileTime.} = initTable[string, NimNode]()

proc addConv*(name: NimNode, body: NimNode) {.compileTime.} =
  convtable[$name] = body
proc getConv*(name: NimNode): NimNode {.compileTime.} =
  if not convtable.hasKey($name):
    error("undeclared $# convtype." % $name, name)
  convtable[$name]

proc bodycomb*(a, b: NimNode): NimNode {.compileTime.} =
  result = newStmtList()
  result.add(a)
  for e in b:
    result.add(e)
  
macro defConvType*(nameof: untyped, body: untyped): untyped =
  let name = if nameof.kind == nnkInfix:
               if $nameof[0] != "of":
                 error("conv type inherit should be `of`", nameof)
               nameof[1]
             else:
               nameof
  let inherit = if nameof.kind == nnkInfix:
                  some(nameof[2])
                else:
                  none(NimNode)
  let body = if inherit.isSome:
               bodycomb(getConv(inherit.get), body)
             else:
               body
               
  let enumid = ident($name & "Kind")
  var enumbody = nnkEnumTy.newTree(newEmptyNode())
  var kindtypes = newStmtList()
  var casesrc = "  case kind*: $#\n" % $enumid
  var prcsrc = ""
  var addkinds = newStmtList()

  var addstmt = newStmtList()
  var substmt = newStmtList()
  var convstmt = newStmtList()
  # collect arms
  for b in body:
    b.expectKind(nnkCall)
    b[0].expectKind(nnkIdent)
    b[1].expectKind(nnkStmtList)
    if $b[0] == "ADD":
      for t in b[1]:
        addstmt.add(t)
    elif $b[0] == "SUB":
      for t in b[1]:
        substmt.add(t)
    elif $b[0] == "CONV":
      for t in b[1]:
        convstmt.add(t)
    else:
      error("unknown convtype arms: $#" % $b[0], b)

  var subnames = initTable[string, bool]()
  for subkind in substmt:
    subkind.expectKind(nnkIdent)
    subnames[$subkind] = true
  var convtypes = initTable[string, tuple[to: NimNode, prc: NimNode]]()
  for convkind in convstmt:
    convkind.expectKind(nnkAsgn)
    convkind[0].expectKind(nnkInfix)
    convtypes[convkind[0][1].repr] = (convkind[0][2], convkind[1])

  for addkind in addstmt:
    let kindname = addkind[0]
    if subnames.hasKey($kindname):
      continue
    enumbody.add(kindname)
    
    if addkind.kind == nnkCall:
      casesrc.add("  of $#.$#: \n    discard\n" % [$enumid, $kindname])
    elif addkind.kind == nnkObjConstr:
      let kindtypeid = ident($name & $kindname)
      let fields = toSeq(addkind)[1..^1]
      let kindtypebody = fields.mapIt(
        if convtypes.hasKey(it[1].repr):
          "  $#*: $#\n" % [it[0].repr, convtypes[it[1].repr].to.repr]
        else:
          "  $#*: $#\n" % [it[0].repr, it[1].repr]
      ).join()
      let kindprcbody = fields.mapIt(
        if convtypes.hasKey(it[1].repr):
          "$1: $3(frm.$2.$1)" % [it[0].repr, toLowerAscii($kindname), $convtypes[it[1].repr].prc]
        else:
          "$1: frm.$2.$1" % [it[0].repr, toLowerAscii($kindname)]
      ).join(", ")
      kindtypes.add(parseExpr("type $#* = object\n$#" % [$kindtypeid, kindtypebody]))
      casesrc.add("  of $#.$#: \n    $#*: $#\n" % [$enumid, $kindname, toLowerAscii($kindname), $kindtypeid])
      if inherit.isSome:
        prcsrc.add("""
when compiles(if frm.kind == $2.$3: return $4(kind: $1.$3, $6: $4$3($5))):
  if frm.kind == $2.$3: return $4(kind: $1.$3, $6: $4$3($5))
""" % [$enumid, $inherit.get & "Kind", $kindname, $name, kindprcbody, toLowerAscii($kindname)])
    else:
      error("unsupported add kind: $#" % $addkind.kind, addkind)
      
  addConv(name, nnkCall.newTree(ident"ADD", addstmt))
  let objectnode = parseExpr("type $#* = object\n$#" % [$name, casesrc])
  result = quote do:
    type `enumid`* {.pure.} = `enumbody`
    `kindtypes`
    `objectnode`
  if inherit.isSome:
    var convproc = parseExpr("proc convertto$#*(frm: $#): $# = discard" % [$name, $inherit.get, $name])
    convproc[6] = parseStmt(prcsrc)
    result.add(convproc)

  echo result.repr

defConvType TAAtom:
  ADD:
    None()
    AVar(name: string)
    IntLit(intval: int64)
  
defConvType X86Atom of TAAtom:
  ADD:
    Temp(name: string)
    Reg(reg: Reg32)
    EspRel(rel: int)
    EbpRel(rel: int)
  SUB:
    None
    AVar

proc converttoX86AtomImpl*(atom: TAAtom): X86Atom =
  if atom.kind == TAAtomKind.None:
    raise newException(Exception, "cannot convert TAAtomKind.None to X86Atom.")
  elif atom.kind == TAAtomKind.AVar:
    return X86Atom(kind: X86AtomKind.Temp, temp: X86AtomTemp(name: atom.avar.name))
  else:
    return converttoX86Atom(atom)
proc converttoX86AtomSeq*(atom: seq[TAAtom]): seq[X86Atom] =
  atom.mapIt(converttoX86AtomImpl(it))
    
defConvType TACode:
  ADD:
    Add(name: string, left: TAAtom, right: TAAtom)
    Sub(name: string, left: TAAtom, right: TAAtom)
    Mul(name: string, left: TAAtom, right: TAAtom)
    ADiv(name: string, left: TAAtom, right: TAAtom)
    Greater(name: string, left: TAAtom, right: TAAtom)
    Lesser(name: string, left: TAAtom, right: TAAtom)
    Set(name: string, value: TAAtom)
    Fn(fnname: string, args: seq[(string, int)], retsize: int)
    Call(name: string, calllabel: string, args: seq[TAAtom], isPure: bool)
    AVar(name: string, size: int, value: TAAtom)
    Goto(gotolabel: string)
    AIf(cond: TAAtom, gotolabel: string)
    Ret(value: TAAtom)

defConvType X86Code of TACode:
  CONV:
    TAAtom -> X86Atom = converttoX86AtomImpl
    seq[TAAtom] -> seq[X86Atom] = converttoX86AtomSeq
  ADD:
    JmpGreater(label: string)
    JmpLess(label: string)
    Jmp(label: string)
  SUB:
    Goto
    Fn
    Ret

echo converttoX86Atom(TAAtom(kind: TAAtomKind.None))
echo converttoX86Atom(TAAtom(kind: TAAtomKind.IntLit, intlit: TAAtomIntLit(intval: 9)))

let left = TAAtom(kind: TAAtomKind.IntLit, intlit: TAAtomIntLit(intval: 4))
let right = TAAtom(kind: TAAtomKind.IntLit, intlit: TAAtomIntLit(intval: 5))
let avar = TAAtom(kind: TAAtomKind.AVar, avar: TAAtomAVar(name: "t0"))
echo converttoX86Code(TACode(kind: TACodeKind.Add, add: TACodeAdd(name: "t0", left: left, right: right)))
echo converttoX86Code(TACode(kind: TACodeKind.Sub, sub: TACodeSub(name: "t1", left: avar, right: right)))
