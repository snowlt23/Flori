
import options
import tables
import streams

import linmem
import codegen.jit

type
  CallConvention* = enum
    convNone
    convCdecl
    convStdcall

defineInternal(InternalMarker)
defineInternal(Symbol)
defineInternal(FScope)
defineInternal(FExpr)

type
  FExprError* = object of Exception
  Span* = object
    filename*: IString
    line*: int
    linepos*: int
    pos*: int
    isinternal*: bool
  InternalOp* = enum
    internalNone
    internalAdd
    internalSub
    internalMul
    internalDiv
    internalGreater
    internalLess
    internalSet
    internalAddr
    internalDeref
  InternalMarkerObj* = object
    undecided*: bool
    internalop*: InternalOp
    internalsize*: int
    cffi*: Option[IString]
    dll*: Option[IString]
    internalffi*: bool
    callconv*: CallConvention
    isStruct*: bool
    isTemplate*: bool
    argnames*: Option[IArray[Symbol]]
    inferargnames*: IList[Symbol]
    argtypes*: Option[IArray[Symbol]]
    inferargtypes*: IList[Symbol]
    returntype*: Symbol
  SymbolKind* = enum
    symbolLink
    symbolType
    symbolTypeGenerics
    symbolGenerics
    symbolUnion
    symbolWord
    symbolDef
    symbolVar
    symbolRef
  SymbolObj* = object
    scope*: FScope
    name*: IString
    fexpr*: FExpr
    instance*: Option[Symbol]
    case kind*: SymbolKind
    of symbolTypeGenerics:
      types*: IArray[Symbol]
    of symbolUnion:
      uniontypes*: IArray[Symbol]
    of symbolLink, symbolVar, symbolRef:
      wrapped*: Symbol
    else:
      discard
  FExprKind* = enum
    fexprIdent = 0
    fexprSymbol
    fexprQuote
    fexprIntLit
    fexprFloatLit
    fexprStrLit

    fexprPrefix
    fexprInfix
    fexprCall
    fexprMethod
    fexprField
    fexprBlock

    fexprIf
    fexprWhile
  FExprObj* = object
    scope*: Option[FScope]
    span*: Span
    src*: Option[IString]
    typ*: Option[Symbol]
    internal*: Option[InternalMarker]
    case kind*: FExprKind
    of fexprIdent:
      idname*: IString
    of fexprSymbol:
      symbol*: Symbol
    of fexprQuote:
      quoted*: IString
    of fexprIntLit:
      intval*: int64
    of fexprFloatLit:
      floatval*: float64
    of fexprStrLit:
      strval*: IString
    of fexprPrefix, fexprInfix, fexprCall, fexprMethod, fexprField:
      callname*: FExpr
      args*: IArray[FExpr]
    of fexprBlock:
      sons*: IArray[FExpr]
    of fexprIf:
      ifbranch*: FExpr
      elifbranches*: IArray[FExpr]
      elsebody*: FExpr
    of fexprWhile:
      whilebranch*: FExpr
  ProcName* = object
    name*: string
    argtypes*: seq[Symbol]
    generics*: seq[Symbol]
  InternalProcType* = proc (scope: FScope, fexpr: var FExpr)
  ProcDecl* = object
    internalproc*: Option[InternalProcType]
    name*: IString
    sym*: Symbol
  ProcDeclGroup* = object
    decls*: IList[ProcDecl]
  TupleTable*[T] = object
    name*: IString
    value*: T
  FScopeObj* = object
    name*: IString
    top*: FScope
    word*: Option[FExpr]
    level*: int
    imports*: IList[TupleTable[FScope]]
    decls*: IList[TupleTable[Symbol]]
    procdecls*: IList[TupleTable[ProcDeclGroup]]
  FImage*[B] = object
    buffer*: B
    importpaths*: IList[IString]
  SemContext* = object
    expands*: seq[Span]
    tmpcount*: int
    notevals*: seq[FExpr]
    rootScope*: FScope

implInternal(InternalMarker, InternalMarkerObj)
implInternal(Symbol, SymbolObj)
implInternal(FScope, FScopeObj)
implInternal(FExpr, FExprObj)

proc initFImage*[B](jitbuf: B): FImage[B] =
  FImage[B](buffer: jitbuf, importpaths: ilistNil[IString]())
proc initSemContext*(): SemContext =
  SemContext(expands: @[], tmpcount: 0, notevals: @[])

var gImage*: FImage[JitBuffer]
var gCtx*: SemContext

proc fscopeNil*(): FScope =
  FScope(index: -1)
proc fscopeRoot*(): FScope =
  FScope(index: 0)

template rootScope*(): FScope = gCtx.rootScope

proc newInternalMarker*(): InternalMarker =
  genInternalMarker(InternalMarkerObj(
    inferargnames: ilistNil[Symbol](),
    inferargtypes: ilistNil[Symbol]()
  ))

#
# Image
#

proc writeimage*(s: Stream, image: var FImage) =
  let linbin = linmemBinary()
  let jitbin = image.buffer.toBin()

  s.write(linbin.len.int64)
  s.write(jitbin.len.int64)
  s.write(linbin)
  s.write(jitbin)

proc readimage*(s: Stream): FImage[JitBuffer] =
  let linsize = s.readInt64()
  let jitsize = s.readInt64()

  let linbin = newString(linsize)
  let jitbin = newString(jitsize)

  result = FImage[JitBuffer]()
  # linmem
  initLinmem(linsize.int)
  discard s.readData(cast[pointer](addr(linmem[0])), linsize.int)
  # jitbuf
  result.buffer = initJitBuffer(jitsize.int)
  discard s.readData(result.buffer.baseptr, jitsize.int)

proc saveimage*[P](filename: string, platform: P) =
  let ss = newStringStream()
  ss.writeimage(gImage)
  ss.write(platform)
  writeFile(filename, ss.data)

proc loadimage*[P](filename: string): P =
  let ss = newStringStream(readFile(filename))
  gImage = ss.readimage()
  if gImage.scopes.len > 0:
    gCtx.rootScope = FScope(index: 0)
  result.read(ss)
