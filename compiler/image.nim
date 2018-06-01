
import options
import tables
import streams

type
  FExprError* = object of Exception
  Span* = ref object
    filename*: string
    line*: int
    linepos*: int
    pos*: int
    isinternal*: bool
  FString* = object
    index*: int
    len*: int
  FArray*[T] = object
    index*: int
    len*: int
  SymbolKind* = enum
    symbolType
    symbolGenerics
    symbolTypeGenerics
    symbolFunc
    symbolFuncGenerics
    symbolFuncType
    symbolInfix
    symbolMacro
    symbolSyntax
    symbolInternal
    symbolDef
    symbolArg
    symbolVar
    symbolRef
    symbolConstant
  Symbol* = ref object
    scope*: FScope
    name*: FString
    fexpr*: FExpr
    instance*: Option[Symbol]
    case kind*: SymbolKind
    of symbolTypeGenerics:
      types*: seq[Symbol]
    of symbolVar, symbolRef:
      wrapped*: Symbol
    of symbolSyntax, symbolMacro:
      discard
      # macroproc*: MacroProc
    of symbolFuncType:
      argtypes*: seq[Symbol]
      rettype*: Symbol
    of symbolConstant:
      constvalue*: FExpr
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
  FExprObj* = object
    span*: Span
    src*: Option[FString]
    case kind*: FExprKind
    of fexprIdent, fexprPrefix, fexprInfix:
      idname*: FString
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
      strval*: FString
    of fexprSeq, fexprArray, fexprList, fexprBlock:
      sons*: FArray[FExpr]
  FExpr* = object
    index: int
  FScopeObj* = object
    name*: string
    top*: FScope
    level*: int
    decls*: Table[string, Symbol]
  FScope* = object
    image: FImage
    index: int
  FImage* = object
    fexprs*: seq[FExprObj]
    scopes*: seq[FScopeObj]
    mem*: seq[uint8]
  SemContext* = object
    expands*: seq[Span]

proc newFImage*(): FImage =
  FImage(fexprs: @[], scopes: @[], mem: @[])

var gImage* = newFImage()
var gCtx* = SemContext(expands: @[])
    
converter obj*(fexpr: FExpr): var FExprObj =
  gImage.fexprs[fexpr.index]
converter obj*(scope: FScope): FScopeObj =
  gImage.scopes[scope.index]

proc addFExpr*(image: var FImage, f: FExprObj): FExpr =
  result = FExpr(index: image.fexprs.len)
  image.fexprs.add(f)
proc addFScope*(image: var FImage, s: FScopeObj): FScope =
  result = FScope(index: image.scopes.len)
  image.scopes.add(s)

proc fstring*(s: string): FString =
  result = FString(index: gImage.mem.len, len: s.len)
  for c in s:
    gImage.mem.add(uint8(c))
proc `$`*(fs: FString): string =
  result = ""
  for i in 0..<fs.len:
    result.add(char(gImage.mem[fs.index + i]))
    
proc farray*[T](len: int): FArray[T] =
  result = FArray(index: gImage.mem.len, len: len * sizeof(T))
proc `[]`*[T](arr: FArray[T], i: int): T =
  cast[ptr T](addr(gImage.mem[arr.index + i*sizeof(T)]))[]
proc mget*[T](arr: var FArray[T], i: int): var T =
  cast[ptr T](addr(gImage.mem[arr.index + i*sizeof(T)]))[]
proc `[]=`*[T](arr: FArray[T], i: int, val: T) =
  cast[ptr T](addr(gImage.mem[arr.index + i*sizeof(T)]))[] = val
iterator items*[T](arr: FArray[T]): T =
  for i in 0..<arr.len:
    yield(arr[i])
iterator mitems*[T](arr: var FArray[T]): var T =
  for i in 0..<arr.len:
    yield(arr.mget(i))
iterator pairs*[T](arr: FArray[T]): (int, T) =
  for i in 0..<arr.len:
    yield(i, arr[i])
iterator mpairs*[T](arr: var FArray[T]): (int, var T) =
  for i in 0..<arr.len:
    yield(i, arr.mget(i))

proc genFExpr*(f: FExprObj): FExpr =
  gImage.addFExpr(f)
proc genFScope*(s: FScopeObj): FScope =
  gImage.addFScope(s)
  
proc writeimage*(s: Stream, image: var FImage) =
  let fsize = image.fexprs.len * sizeof(FExprObj)
  let ssize = image.scopes.len * sizeof(FScopeObj)
  let memsize = image.mem.len
  var fs = newString(fsize)
  var ss = newString(ssize)
  var mem = newString(memsize)
  if image.fexprs.len != 0:
    copyMem(cast[pointer](addr(fs[0])), cast[pointer](addr(image.fexprs[0])), fsize)
  if image.scopes.len != 0:
    copyMem(cast[pointer](addr(ss[0])), cast[pointer](addr(image.scopes[0])), ssize)
  if image.mem.len != 0:
    copyMem(cast[pointer](addr(mem[0])), cast[pointer](addr(image.mem[0])), memsize)
  s.write(fsize.int64)
  s.write(ssize.int64)
  s.write(memsize.int64)
  s.write(fs)
  s.write(ss)
  s.write(mem)
proc readimage*(s: Stream): FImage =
  let fsize = s.readInt64()
  let ssize = s.readInt64()
  let memsize = s.readInt64()
  result = FImage(fexprs: newSeq[FExprObj](fsize div sizeof(FExprObj)), scopes: newSeq[FScopeObj](ssize div sizeof(FScopeObj)), mem: newSeq[uint8](memsize))
  if result.fexprs.len != 0:
    discard s.readData(cast[pointer](addr(result.fexprs[0])), fsize.int)
  if result.scopes.len != 0:
    discard s.readData(cast[pointer](addr(result.scopes[0])), ssize.int)
  if result.mem.len != 0:
    discard s.readData(cast[pointer](addr(result.mem[0])), memsize.int)
proc saveimage*(filename: string) =
  let ss = newStringStream()
  ss.writeimage(gImage)
  writeFile(filename, ss.data)
proc loadimage*(filename: string) =
  let ss = newStringStream(readFile(filename))
  gImage = ss.readimage()
