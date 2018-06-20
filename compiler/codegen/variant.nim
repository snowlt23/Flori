
import strutils, sequtils
import macros
import tables
import options

proc generateEmptyVariant*(name: NimNode, kindname: NimNode): string =
  "  of $#.$#:\n    discard\n" % [$name & "Kind", $kindname]
proc generateVariant*(name: NimNode, kindname: NimNode): string =
  "  of $1Kind.$2:\n    $3*: $1$2\n" % [$name, $kindname, toLowerAscii($kindname)]
proc generateEmptyInitProc*(name: NimNode, addkind: NimNode): NimNode =
  let kindname = addkind[0]
  parseExpr("proc init$1$2*(): $1 = $1(kind: $1Kind.$2)" % [$name, $kindname])
proc generateInitProc*(name: NimNode, addkind: NimNode): NimNode =
  let kindname = addkind[0]
  let fields = toSeq(addkind)[1..^1]
  let initfields = fields.mapIt(
    "$1: $1" % [it[0].repr]
  ).join(", ")
  let initargdecls = fields.mapIt(
    "$#: $#" % [it[0].repr, it[1].repr]
  ).join(", ")
  parseExpr("proc init$1$2*($5): $1 = $1(kind: $1Kind.$2, $3: $1$2($4))" % [$name, $kindname, toLowerAscii($kindname), initfields, initargdecls])

proc generateVariantType*(name: NimNode, addkind: NimNode): NimNode =
  let kindname = addkind[0]
  let fields = toSeq(addkind)[1..^1]
  let kindtypebody = fields.mapIt(
    "  $#*: $#\n" % [it[0].repr, it[1].repr]
  ).join()
  parseExpr("type $1$2* = object\n$3" % [$name, $kindname, kindtypebody])

macro defVariant*(name: untyped, body: untyped): untyped =
  var enumbody = nnkEnumTy.newTree(newEmptyNode())
  var casesrc = "  case kind*: $1Kind\n" % $name
  var initprocs = newStmtList()
  var kindtypes = newStmtList()

  for addkind in body:
    let kindname = addkind[0]
    enumbody.add(kindname)
    if addkind.kind == nnkCall:
      casesrc.add(generateEmptyVariant(name, kindname))
      initprocs.add(generateEmptyInitProc(name, addkind))
    elif addkind.kind == nnkObjConstr:
      casesrc.add(generateVariant(name, kindname))
      initprocs.add(generateInitProc(name, addkind))
      kindtypes.add(generateVariantType(name, addkind))
    else:
      error("unsupported variant kind: $#" % $addkind.kind, addkind)

  let objectnode = parseExpr("type $#* = object\n$#" % [$name, casesrc])
  let enumid = ident($name & "Kind")
  result = quote do:
    type `enumid`* {.pure.} = `enumbody`
    `kindtypes`
    `objectnode`
    `initprocs`
  # echo result.repr
