
import options
import tables
import streams

type
  FExprError* = object of Exception
  Span* = object
    filename*: IString
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
  IListObj*[T] = object
    value*: T
    next*: IList[T]
  IList*[T] = object
    index*: int
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
  SymbolObj* = object
    scope*: FScope
    name*: IString
    fexpr*: FExpr
    instance*: Option[Symbol]
    case kind*: SymbolKind
    of symbolTypeGenerics:
      types*: IArray[Symbol]
    of symbolVar, symbolRef:
      wrapped*: Symbol
    of symbolSyntax, symbolMacro:
      discard
      # macroproc*: MacroProc
    of symbolFuncType:
      argtypes*: IArray[Symbol]
      rettype*: Symbol
    of symbolConstant:
      constvalue*: FExpr
    else:
      discard
  Symbol* = object
    index*: int
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
    scope*: Option[FScope]
    span*: Span
    src*: Option[IString]
    typ*: Option[Symbol]
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
    index*: int
  ProcName* = object
    name*: string
    argtypes*: seq[Symbol]
    generics*: seq[Symbol]
  InternalProcType* = proc (scope: FScope, fexpr: var FExpr)
  ProcDecl* = object
    internalproc*: Option[InternalProcType]
    name*: IString
    argtypes*: IArray[Symbol]
    generics*: IArray[Symbol]
    returntype*: Symbol
    sym*: Symbol
  ProcDeclGroup* = object
    decls*: IList[ProcDecl]
  TupleTable*[T] = tuple[name: IString, value: T]
  FScopeObj* = object
    name*: IString
    top*: FScope
    level*: int
    imports*: IList[TupleTable[FScope]]
    decls*: IList[TupleTable[Symbol]]
    procdecls*: IList[TupleTable[ProcDeclGroup]]
  FScope* = object
    index*: int
  FImage* = object
    fexprs*: seq[FExprObj]
    symbols*: seq[SymbolObj]
    scopes*: seq[FScopeObj]
    mem*: seq[uint8]
    importpaths*: IList[IString]
  SemContext* = object
    expands*: seq[Span]
    tmpcount*: int
    rootScope*: FScope

proc newFImage*(): FImage =
  FImage(fexprs: @[], symbols: @[], scopes: @[], mem: @[], importpaths: IList[IString](index: -1))

var gImage* = newFImage()
var gCtx* = SemContext(expands: @[], tmpcount: 0)

template rootScope*(): FScope = gCtx.rootScope
    
proc obj*(fexpr: FExpr): var FExprObj =
  gImage.fexprs[fexpr.index]
proc obj*(sym: Symbol): var SymbolObj =
  gImage.symbols[sym.index]
proc obj*(scope: FScope): var FScopeObj =
  gImage.scopes[scope.index]

proc addFExpr*(image: var FImage, f: FExprObj): FExpr =
  result = FExpr(index: image.fexprs.len)
  image.fexprs.add(f)
proc addSymbol*(image: var FImage, s: SymbolObj): Symbol =
  result = Symbol(index: image.symbols.len)
  image.symbols.add(s)
proc addFScope*(image: var FImage, s: FScopeObj): FScope =
  result = FScope(index: image.scopes.len)
  image.scopes.add(s)
  
proc genFExpr*(f: FExprObj): FExpr =
  gImage.addFExpr(f)
proc genSymbol*(s: SymbolObj): Symbol =
  gImage.addSymbol(s)
proc genFScope*(s: FScopeObj): FScope =
  gImage.addFScope(s)

#
# Internal Types
#

proc istring*(s: string): IString =
  result = IString(index: gImage.mem.len, len: s.len)
  for c in s:
    gImage.mem.add(uint8(c))
proc `$`*(fs: IString): string =
  result = ""
  for i in 0..<fs.len:
    result.add(char(gImage.mem[fs.index + i]))
proc `==`*(a: IString, b: string): bool =
  $a == b
    
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

proc ilist*[T](value: T, next: IList[T]): IList[T] =
  result = IList[T](index: gImage.mem.len)
  
  for i in 0..<sizeof(T):
    gImage.mem.add(0)
  for i in 0..<sizeof(int32):
    gImage.mem.add(0)

  let p = cast[ptr IListObj[T]](addr(gImage.mem[result.index]))
  p[].value = value
  p[].next = next
converter obj*[T](lst: IList[T]): IListObj[T] =
  let p = cast[ptr IListObj[T]](addr(gImage.mem[lst.index]))
  p[]
proc value*[T](lst: IList[T]): var T =
  let p = cast[ptr IListObj[T]](addr(gImage.mem[lst.index]))
  p[].value
proc next*[T](lst: IList[T]): var IList[T] =
  let p = cast[ptr IListObj[T]](addr(gImage.mem[lst.index]))
  p[].next
proc ilistNil*[T](): IList[T] =
  IList[T](index: -1)
proc isNil*[T](lst: IList[T]): bool =
  lst.index == -1
proc last*[T](lst: IList[T]): IList[T] =
  var cur = lst
  while true:
    if cur.isNil:
      return cur
    cur = cur.next
proc add*[T](lst: var IList[T], value: T) =
  lst = ilist(value, lst)
iterator items*[T](lst: IList[T]): T =
  var cur = lst
  while true:
    if cur.isNil:
      break
    yield(cur.value)
    cur = cur.next
iterator mitems*[T](lst: IList[T]): var T =
  var cur = lst
  while true:
    if cur.isNil:
      break
    yield(cur.mvalue)
    cur = cur.next
iterator pairs*[T](lst: IList[T]): (int, T) =
  var cur = lst
  var i = 0
  while true:
    if cur.isNil:
      break
    yield(i, cur.value)
    cur = cur.next
    i.inc

#
# Image
#
  
proc writeimage*(s: Stream, image: var FImage) =
  let fsize = image.fexprs.len * sizeof(FExprObj)
  let symsize = image.symbols.len * sizeof(SymbolObj)
  let ssize = image.scopes.len * sizeof(FScopeObj)
  let memsize = image.mem.len
  var fs = newString(fsize)
  var syms = newString(symsize)
  var ss = newString(ssize)
  var mem = newString(memsize)
  if image.fexprs.len != 0:
    copyMem(cast[pointer](addr(fs[0])), cast[pointer](addr(image.fexprs[0])), fsize)
  if image.symbols.len != 0:
    copyMem(cast[pointer](addr(syms[0])), cast[pointer](addr(image.symbols[0])), symsize)
  if image.scopes.len != 0:
    copyMem(cast[pointer](addr(ss[0])), cast[pointer](addr(image.scopes[0])), ssize)
  if image.mem.len != 0:
    copyMem(cast[pointer](addr(mem[0])), cast[pointer](addr(image.mem[0])), memsize)
  s.write(fsize.int64)
  s.write(symsize.int64)
  s.write(ssize.int64)
  s.write(memsize.int64)
  s.write(fs)
  s.write(ss)
  s.write(mem)
  s.write(image.importpaths)
proc readimage*(s: Stream): FImage =
  let fsize = s.readInt64()
  let symsize = s.readInt64()
  let ssize = s.readInt64()
  let memsize = s.readInt64()
  result = FImage(fexprs: newSeq[FExprObj](fsize div sizeof(FExprObj)), symbols: newSeq[SymbolObj](symsize div sizeof(SymbolObj)), scopes: newSeq[FScopeObj](ssize div sizeof(FScopeObj)), mem: newSeq[uint8](memsize))
  if result.fexprs.len != 0:
    discard s.readData(cast[pointer](addr(result.fexprs[0])), fsize.int)
  if result.symbols.len != 0:
    discard s.readData(cast[pointer](addr(result.symbols[0])), symsize.int)
  if result.scopes.len != 0:
    discard s.readData(cast[pointer](addr(result.scopes[0])), ssize.int)
  if result.mem.len != 0:
    discard s.readData(cast[pointer](addr(result.mem[0])), memsize.int)
  discard s.readData(cast[pointer](addr(result.importpaths)), sizeof(IList[IString]))
proc saveimage*(filename: string) =
  let ss = newStringStream()
  ss.writeimage(gImage)
  writeFile(filename, ss.data)
proc loadimage*(filename: string) =
  let ss = newStringStream(readFile(filename))
  gImage = ss.readimage()
  if gImage.scopes.len > 0:
    gCtx.rootScope = FScope(index: 0)
