
import options
import tables
import streams
import dynlib
import marshal

import linmem

defineInternal(MetadataStore)
defineInternal(MacroProc)
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
    scope*: Scope
    internal*: InternalKind
    decl*: bool
    header*: Option[IString]
    patternc*: Option[IString]
    importc*: Option[IString]
    exportc*: Option[IString]
    declc*: Option[IString]
    infixc*: bool
    patternjs*: Option[IString]
    importjs*: Option[IString]
    exportjs*: Option[IString]
    infixjs*: bool
    typ*: Symbol
    compiletime*: bool
    constvalue*: FExpr
    converters*: IList[FExpr]
    isCStruct*: bool
    isSyntax*: bool
    isEvaluated*: bool
    isExpanded*: bool
    isToplevel*: bool
    isElimEvaluated*: bool
    isEliminated*: bool
    isConverted*: bool
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
      strval*: IString
    of fexprSeq, fexprArray, fexprList, fexprBlock:
      sons*: IList[FExpr]

  MacroProcObj* = object
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
    returntype*: Symbol
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
    parent*: Scope
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
    modules*: IList[TupleTable[Scope]]
    importpaths*: IArray[IString]
    defines*: seq[string]
    globaltoplevels*: seq[FExpr]
    moptions*: string
    macrolib*: LibHandle
    macroprocs*: seq[MacroProc]

implInternal(MetadataStore, MetadataStoreObj)
implInternal(MacroProc, MacroProcObj)
implInternal(Symbol, SymbolObj)
implInternal(Scope, ScopeObj)
implInternal(FExpr, FExprObj)

proc initFImage*(): FImage =
  FImage()
proc initSemContext*(): SemContext =
  SemContext(expands: @[], tmpcount: 0, notevals: @[], defines: @[], globaltoplevels: @[], modules: ilistNil[TupleTable[Scope]]())

var gImage*: FImage
var gCtx*: SemContext

proc hasDefn*(f: FExpr): bool =
  f.metadata.internal in {internalDefn, internalMacro}

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
proc `==`*(a, b: Scope): bool =
  a.obj.name == b.obj.name and a.obj.level == b.obj.level
proc isTop*(s: Scope): bool =
  s == s.parent
proc top*(s: Scope): Scope =
  var cur = s
  while true:
    if cur.isTop:
      return cur
    cur = cur.parent
  assert(false)

template rootScope*(): Scope = gCtx.rootScope

proc newMetadataStore*(): MetadataStore =
  genMetadataStore(MetadataStoreObj(internal: internalNone, typ: symbolNil(), converters: ilistNil[FExpr]()))

proc hasConvert*(matches: openArray[Matched]): bool =
  for m in matches:
    if m.kind == matchConvert:
      return true
  return false

#
# Image
#

proc writeimage*(s: Stream, image: var FImage) =
  gCtx.macrolib = nil
  let linbin = linmemBinary()
  s.write(linbin.len.int64)
  s.write(linbin)
  s.store(gCtx)

proc readimage*(s: Stream): FImage =
  let linsize = s.readInt64()
  # linmem
  initLinmem(linsize.int)
  linmemPos = linsize.int
  linmemExtend(defaultLinmemSpace)
  discard s.readData(linmemPtr, linsize.int)
  s.load(gCtx)

proc saveimage*(filename: string) =
  let ss = newStringStream()
  ss.writeimage(gImage)
  writeFile(filename, ss.data)

proc loadimage*(filename: string) =
  let ss = newStringStream(readFile(filename))
  gImage = ss.readimage()
