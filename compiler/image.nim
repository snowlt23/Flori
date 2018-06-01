
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
  IString* = object
    index*: int
    len*: int
  IArray*[T] = object
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
    name*: IString
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
      sons*: IArray[FExpr]
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

proc istring*(s: string): IString =
  result = IString(index: gImage.mem.len, len: s.len)
  for c in s:
    gImage.mem.add(uint8(c))
proc `$`*(fs: IString): string =
  result = ""
  for i in 0..<fs.len:
    result.add(char(gImage.mem[fs.index + i]))
    
proc iarray*[T](len: int): IArray[T] =
  result = IArray[T](index: gImage.mem.len, len: len)
  for i in 0..<len * sizeof(T):
    gImage.mem.add(0)
proc checkBounds*[T](arr: IArray[T], i: int) =
  when not defined(release):
    if i < 0 or i >= arr.len:
      raise newException(Exception, "index out of bounds: $#" % $i)
proc `[]`*[T](arr: IArray[T], i: int): T =
  arr.checkBounds(i)
  cast[ptr T](addr(gImage.mem[arr.index + i*sizeof(T)]))[]
proc mget*[T](arr: var IArray[T], i: int): var T =
  arr.checkBounds(i)
  cast[ptr T](addr(gImage.mem[arr.index + i*sizeof(T)]))[]
proc `[]=`*[T](arr: IArray[T], i: int, val: T) =
  arr.checkBounds(i)
  cast[ptr T](addr(gImage.mem[arr.index + i*sizeof(T)]))[] = val
proc iarray*[T](arr: openArray[T]): IArray[T] =
  result = iarray[T](arr.len)
  for i, e in arr:
    result[i] = e
proc iarray*[T](): IArray[T] =
  iarray[T]([])
iterator items*[T](arr: IArray[T]): T =
  for i in 0..<arr.len:
    yield(arr[i])
iterator mitems*[T](arr: var IArray[T]): var T =
  for i in 0..<arr.len:
    yield(arr.mget(i))
iterator pairs*[T](arr: IArray[T]): (int, T) =
  for i in 0..<arr.len:
    yield(i, arr[i])
iterator mpairs*[T](arr: var IArray[T]): (int, var T) =
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
