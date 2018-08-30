
import options
import tables
import streams

import linmem

defineInternal(MetadataStore)
defineInternal(Symbol)
defineInternal(Scope)
defineInternal(FExpr)

type
  FExprError* = object of Exception
  Span* = object
    filename*: IString
    line*: int
    linepos*: int
    pos*: int
    isinternal*: bool
  InternalKind* = enum
    internalNone
    internalDefn
    internalMacro
    internalDeftype
    internalTypedef
    internalIf
    internalWhile
    internalVar
    internalConst
    internalDef
    internalSet
    internalFieldAccess
    internalImport
    internalExport
    internalReload
    internalCodegenDecl
    internalCodegenHead
    internalBlock
  MetadataStoreObj* = object
    internal*: InternalKind
    header*: Option[IString]
    patternc*: Option[IString]
    importc*: Option[IString]
    exportc*: Option[IString]
    declc*: Option[IString]
    infixc*: bool
    typ*: Symbol
    constvalue*: FExpr
    converters*: IList[FExpr]
    isCStruct*: bool
    isEvaluated*: bool
    isToplevel*: bool
    isEliminated*: bool
  SymbolKind* = enum
    symbolDef
    symbolArg
    symbolType
    symbolGenerics
    symbolTypeGenerics
    symbolVar
    symbolRef
    symbolFunc
    symbolFuncGenerics
    symbolFuncType
    symbolInfix
    symbolMacro
    symbolSyntax
    symbolInternal
    symbolIntLit
  SymbolObj* = object
    scope*: Scope
    name*: IString
    fexpr*: FExpr
    instance*: Option[Symbol]
    case kind*: SymbolKind
    of symbolArg:
      argpos*: int
    of symbolTypeGenerics:
      types*: IArray[Symbol]
    of symbolVar, symbolRef:
      wrapped*: Symbol
    of symbolSyntax, symbolMacro:
      macroproc*: MacroProc
    of symbolFuncType:
      argtypes*: IArray[Symbol]
      rettype*: Symbol
    of symbolIntLit:
      intval*: int64
    else:
      discard
  FExprKind* = enum
    fexprIdent = 0
    fexprPrefix
    fexprInfix

    fexprQuote
    fexprSymbol
    
    fexprIntLit
    fexprFloatLit
    fexprStrLit

    fexprSeq
    fexprArray
    fexprList
    fexprBlock

  MatchedKind* = enum
    matchType
    matchConvert
    matchNone
  Matched* = object
    case kind*: MatchedKind
    of matchConvert:
      convsym*: Symbol
    else:
      discard

  FExprObj* = object
    span*: Span
    metadata*: MetadataStore
    src*: Option[IString]
    case kind*: FExprKind
    of fexprIdent, fexprPrefix, fexprInfix:
      idname*: IString
      priority*: int
      isleft*: bool
    of fexprQuote:
      quoted*: FExpr
    of fexprSymbol:
      symbol*: Symbol
    of fexprIntLit:
      intval*: int64
    of fexprFloatLit:
      floatval*: float64
    of fexprStrLit:
      strval*: string
    of fexprSeq, fexprArray, fexprList, fexprBlock:
      sons*: IArray[FExpr]

  MacroProc* = object
    importname*: IString
    call*: proc (fexpr: FExpr): FExpr {.cdecl.}

  ProcName* = object
    name*: string
    argtypes*: seq[Symbol]
    generics*: seq[Symbol]
  InternalProcType* = proc (scope: Scope, fexpr: var FExpr)
  ProcDecl* = object
    name*: IString
    sym*: Symbol
    argtypes*: IArray[Symbol]
    generics*: IArray[Symbol]
    internalproc*: Option[InternalProcType]
    isSyntax*: bool
    isMacro*: bool
    macroproc*: MacroProc
  ProcDeclGroup* = object
    decls*: IList[ProcDecl]
  TupleTable*[T] = object
    name*: IString
    value*: T
  ScopeObj* = object
    name*: IString
    top*: Scope
    level*: int
    imports*: IList[TupleTable[Scope]]
    exports*: IList[TupleTable[Scope]]
    decls*: IList[TupleTable[Symbol]]
    procdecls*: IList[TupleTable[ProcDeclGroup]]
  FImage* = object
  SemContext* = object
    expands*: seq[Span]
    tmpcount*: int
    notevals*: seq[FExpr]
    rootScope*: Scope
    defines*: seq[string]
    globaltoplevels*: seq[FExpr]

implInternal(MetadataStore, MetadataStoreObj)
implInternal(Symbol, SymbolObj)
implInternal(Scope, ScopeObj)
implInternal(FExpr, FExprObj)

proc initFImage*(): FImage =
  FImage()
proc initSemContext*(): SemContext =
  SemContext(expands: @[], tmpcount: 0, notevals: @[], defines: @[], globaltoplevels: @[])

var gImage*: FImage
var gCtx*: SemContext

proc isMatch*(m: Matched): bool =
  m.kind != matchNone

proc fexprNil*(): FExpr =
  FExpr(index: -1)
proc isNil*(fexpr: FExpr): bool =
  fexpr.index == -1

proc symbolNil*(): Symbol =
  Symbol(index: -1)
proc isNil*(sym: Symbol): bool =
  sym.index == -1
proc hasTyp*(fexpr: FExpr): bool =
  not fexpr.metadata.typ.isNil

proc scopeNil*(): Scope =
  Scope(index: -1)
proc scopeRoot*(): Scope =
  Scope(index: 0)

template rootScope*(): Scope = gCtx.rootScope

proc newMetadataStore*(): MetadataStore =
  genMetadataStore(MetadataStoreObj(internal: internalNone, typ: symbolNil(), converters: ilistNil[FExpr]()))

#
# Image
#

proc writeimage*(s: Stream, image: var FImage) =
  let linbin = linmemBinary()
  s.write(linbin.len.int64)
  s.write(linbin)

proc readimage*(s: Stream): FImage =
  let linsize = s.readInt64()
  let linbin = newString(linsize)
  # linmem
  initLinmem(linsize.int)
  discard s.readData(cast[pointer](addr(linmem[0])), linsize.int)

proc saveimage*[P](filename: string) =
  let ss = newStringStream()
  ss.writeimage(gImage)
  writeFile(filename, ss.data)

proc loadimage*[P](filename: string) =
  let ss = newStringStream(readFile(filename))
  gImage = ss.readimage()
  if gImage.scopes.len > 0:
    gCtx.rootScope = Scope(index: 0)
