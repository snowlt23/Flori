
import strutils, sequtils
import options
import tables
import terminal
import deques
import algorithm

import image, symbol

const fexprAtoms* = {fexprIdent..fexprStrLit}
const fexprCalls* = {fexprInfix..fexprField}

proc `$`*(fexpr: FExpr): string

proc hint*(span: Span, msg: string) =
  let h = " $#($#:$#): " % [$span.filename, $span.line, $span.linepos] & msg
  styledEcho(fgGreen, "[Hint] ", resetStyle, h)
proc error*(span: Span, msg: string) =
  for expand in gCtx.expands:
    let e = "$#($#:$#): expand by" % [$expand.filename, $expand.line, $expand.linepos]
    styledEcho(fgGreen, "[Expand] ", resetStyle, e)
  var e = ""
  if span.isinternal:
    e = "internal.$#($#): " % [$span.filename, $span.line] & msg
  else:
    e = "$#($#:$#): " % [$span.filename, $span.line, $span.linepos] & msg
  styledEcho(fgRed, "[Error] ", resetStyle, e)

  when defined(replError):
    raise newException(FExprError, e)
  elif defined(release) or defined(noExceptionError):
    quit()
  else:
    raise newException(FExprError, e)

template assert*(fexpr: FExpr, b: typed) =
  if not b:
    fexpr.error("internal error.")

proc hint*(fexpr: FExprObj, msg: string) = fexpr.span.hint(msg)
proc hint*(fexpr: FExpr, msg: string) = fexpr.obj.span.hint(msg)
proc error*(fexpr: FExprObj, msg: string) = fexpr.span.error(msg)
proc error*(fexpr: FExpr, msg: string) = fexpr.obj.span.error(msg)

template internalSpan*(): Span =
  Span(filename: istring(instantiationInfo().filename), line: instantiationInfo().line, isinternal: true)

proc fident*(span: Span, name: string): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprIdent, idname: istring(name)))
proc fsymbol*(span: Span, sym: Symbol): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprSymbol, symbol: sym))
proc fquote*(span: Span, q: string): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprQuote, quoted: istring(q)))

proc fintlit*(span: Span, x: int64): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprIntLit, intval: x))
proc ffloatlit*(span: Span, x: float): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprFloatLit, floatval: x))
proc fstrlit*(span: Span, s: string): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprStrLit, strval: istring(s)))

proc finfix*(span: Span, call, left, right: FExpr): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprInfix, callname: call, args: iarray([left, right])))
proc fcall*(span: Span, call: FExpr, args: openArray[FExpr]): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprCall, callname: call, args: iarray(args)))
proc fmethod*(span: Span, first: FExpr, call: FExpr, args: openArray[FExpr]): FExpr =
  var s = @[first] & @args
  genFExpr(FExprObj(span: span, kind: fexprMethod, callname: call, args: iarray(s)))
proc ffield*(span: Span, first: FExpr, call: FExpr): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprField, callname: call, args: iarray([first])))
proc fblock*(span: Span, sons: openArray[FExpr]): FExpr =
  genFExpr(FExprObj(span: span, kind: fexprBlock, sons: iarray(sons)))

proc kind*(fexpr: FExpr): FExprKind = fexpr.obj.kind
proc idname*(fexpr: FExpr): IString = fexpr.obj.idname
proc symbol*(fexpr: FExpr): Symbol = fexpr.obj.symbol
proc quoted*(fexpr: FExpr): IString = fexpr.obj.quoted
proc intval*(fexpr: FExpr): int64 = fexpr.obj.intval
proc strval*(fexpr: FExpr): IString = fexpr.obj.strval
proc span*(fexpr: FExpr): Span = fexpr.obj.span
proc call*(fexpr: FExpr): FExpr = fexpr.obj.callname
proc `call=`*(fexpr: FExpr, c: FExpr) = fexpr.obj.callname = c
proc args*(fexpr: FExpr): IArray[FExpr] = fexpr.obj.args
proc sons*(fexpr: FExpr): IArray[FExpr] = fexpr.obj.sons
proc src*(fexpr: FExpr): Option[IString] = fexpr.obj.src
proc `src=`*(fexpr: FExpr, opt: Option[IString]) = fexpr.obj.src = opt
proc typ*(fexpr: FExpr): Option[Symbol] = fexpr.obj.typ
proc `typ=`*(fexpr: FExpr, opt: Option[Symbol]) = fexpr.obj.typ = opt
proc len*(fexpr: FExpr): int = fexpr.obj.sons.len
proc scope*(fexpr: FExpr): Option[FScope] = fexpr.obj.scope
proc `scope=`*(fexpr: FExpr, opt: Option[FScope]) = fexpr.obj.scope = opt
proc internal*(fexpr: FExpr): InternalMarker = fexpr.obj.internal.get

proc genIndent*(indent: int): string =
  repeat(' ', indent)

proc toString*(fexpr: FExpr, indent: int, desc: bool, typ: bool): string

proc toString*(fexpr: var FExprObj, indent: int, desc: bool, typ: bool): string =
  case fexpr.kind
  of fexprIdent:
    $fexpr.idname
  of fexprSymbol:
    toString(fexpr.symbol, desc)
  of fexprQuote:
    "`" & $fexpr.quoted
  of fexprIntLit:
    $fexpr.intval
  of fexprFloatLit:
    $fexpr.floatval
  of fexprStrLit:
    "\"" & $fexpr.strval & "\""
  of fexprInfix:
    toString(fexpr.args[0], indent, desc, typ) & " " & toString(fexpr.callname, indent, desc, typ) & " " & toString(fexpr.args[1], indent, desc, typ)
  of fexprCall:
    toString(fexpr.callname, indent, desc, typ) & "(" & fexpr.args.mapIt(toString(it, indent, desc, typ)).join(", ") & ")"
  of fexprMethod:
    var s = newSeq[string]()
    for i in 1..<fexpr.args.len:
      s.add(toString(fexpr.args[i], indent, desc, typ))
    "$#.$#($#)" % [toString(fexpr.args[0], indent, desc, typ), toString(fexpr.callname, indent, desc, typ), s.join(", ")]
  of fexprField:
    toString(fexpr.args[0], indent, desc, typ) & "." & toString(fexpr.callname, indent, desc, typ)
  of fexprBlock:
    if fexpr.sons.len == 0:
      ""
    else:
      "\n" & genIndent(indent+2) & fexpr.sons.mapIt(it.toString(indent+2, desc, typ)).join("\n" & genIndent(indent+2))
proc toString*(fexpr: FExpr, indent: int, desc: bool, typ: bool): string = fexpr.obj.toString(indent, desc, typ)

proc `$`*(fexpr: var FExprObj): string = fexpr.toString(0, false, false)
proc `$`*(fexpr: FExpr): string = $fexpr.obj
proc desc*(fexpr: var FExprObj): string = fexpr.toString(0, true, false)
proc desc*(fexpr: FExpr): string = fexpr.obj.desc()
proc debug*(fexpr: var FExprObj): string = fexpr.toString(0, true, true)
proc debug*(fexpr: FExpr): string = fexpr.obj.debug()

proc getSrcExpr*(fexpr: FExpr): string =
  if fexpr.obj.src.isSome:
    $fexpr.obj.src.get
  else:
    $fexpr
