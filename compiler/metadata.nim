
import types, fexpr

import macros
import types
import tables
import options
import strutils

var parsedstmt {.compileTime.} = newStmtList()

macro defMetadata*(key: untyped, T: typed, metacopy = true): untyped =
  let tsym = ident("Metadata" & $T)
  let keyhas = ident("has" & $key)
  let keyget = key
  let keyset = nnkAccQuoted.newTree((ident(($key) & "=")))
  let keystr = newLit($key)
  result = quote do:
    when not declared(`tsym`):
      type `tsym` = ref object of Metadata
        data: `T`
    proc `keyhas`*(fexpr: FExpr): bool =
      fexpr.metadata.hasKey(`keystr`)
    proc `keyget`*(fexpr: FExpr): var `T` =
      `tsym`(fexpr.metadata[`keystr`]).data
    proc `keyset`*(fexpr: FExpr, value: `T`) =
      fexpr.metadata[`keystr`] = `tsym`(data: value)
  if metacopy.boolVal:
    parsedstmt.add(quote do:
      if fexpr.`keyhas`:
        copied.`key` = fexpr.`key`
    )
  # echo result.repr

macro defBoolMetadata*(key: untyped): untyped =
  let metaid = ident("internal" & $key)
  let hasid = ident("hasinternal" & $key)
  let setkey = ident($key & "=")
  result = quote do:
    defMetadata(`metaid`, bool)
    proc `key`*(fexpr: FExpr): bool = fexpr.`hasid` and fexpr.`metaid`
    proc `setkey`*(fexpr: FExpr, b: bool) = fexpr.`metaid` = b

macro defParsedType*(name: untyped, body: untyped): untyped =
  var objbody = nnkRecList.newTree()
  var fieldprocs = newStmtList()
  let metaname = ident(toLowerAscii($name))
  var metacopy = newStmtList()
  objbody.add(nnkIdentDefs.newTree(nnkPostfix.newTree(ident"*", ident"fexpr"), ident("FExpr"), newEmptyNode()))
  for b in body:
    if b.kind == nnkIdent:
      let fieldname = b
      let fieldposname = ident($b & "pos=")
      let setfieldname = ident($b & "=")
      let hasname = ident("has" & $b)
      objbody.add(nnkIdentDefs.newTree(fieldname, ident"int", newEmptyNode()))
      fieldprocs.add(quote do:
        proc `fieldname`*(this: `name`): var FExpr =
          this.fexpr[this.`fieldname`]
        proc `setfieldname`*(this: `name`, value: FExpr) =
          this.fexpr[this.`fieldname`] = value
        proc `fieldposname`*(this: var `name`, value: int) =
          this.`fieldname` = value
        proc `hasname`*(this: `name`): bool = this.`fieldname` != -1
      )
      metacopy.add(quote do:
        copied.`metaname`.`fieldname` = fexpr.`metaname`.`fieldname`
      )
    elif b.kind == nnkAsgn:
      let f = b[0]
      let fieldname = nnkPostfix.newTree(ident"*", b[0])
      let fieldtype = b[1]
      objbody.add(nnkIdentDefs.newTree(fieldname, fieldtype, newEmptyNode()))
      metacopy.add(quote do:
        copied.`metaname`.`f` = fexpr.`metaname`.`f`
      )
    else:
      error("unexpected kind: $#" % $b.kind, b)
    
  let obj = nnkObjectTy.newTree(newEmptyNode(), newEmptyNode(), objbody)
  result = quote do:
    type `name`* = `obj`
    `fieldprocs`
    defMetadata(`metaname`, `name`, false)

  let hasname = ident("has" & $name)
  parsedstmt.add(quote do:
    if fexpr.`hasname`:
      copied.`metaname` = `name`()
      `metacopy`
      copied.`metaname`.fexpr = copied
  )

defParsedType Defn:
  name
  generics
  args
  retprefix
  ret
  retgenerics
  pragma
  body
defParsedType Deftype:
  name
  generics
  pragma
  body
defParsedType InitExpr:
  typ
  body
defParsedType WhileExpr:
  cond
  body
defParsedType DefExpr:
  isPrefix = bool
  name
  value
defParsedType SetExpr:
  dst
  value
defParsedType FieldAccessExpr:
  value
  fieldname
defParsedType ImportExpr:
  importname
      
type
  InternalMarkKind* = enum
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
    internalInit
    internalImport
    internalExport
    internalReload
    internalCodegenDecl
    internalCodegenHead
    internalBlock
  InternalPragma* = object
    # c codegen
    importc*: Option[string]
    header*: Option[string]
    exportc*: Option[string]
    patternc*: Option[string]
    declc*: Option[string]
    infixc*: bool
    # js codegen
    importjs*: Option[string]
    exportjs*: Option[string]
    patternjs*: Option[string]
    infixjs*: bool
    # general
    internal*: bool
    isSyntax*: bool
    inline*: bool
    nodestruct*: bool
    compiletime*: bool
    nocompiletime*: bool
    resource*: bool

type
  IfExpr* = object
    elifbranch*: seq[tuple[cond: FExpr, body: FExpr]]
    elsebranch*: FExpr

type Converters* = object
  converters*: seq[FExpr]

defMetadata(typ, Symbol)
defMetadata(constvalue, FExpr)
defMetadata(converters, Converters)
defMetadata(runtime, FExpr)

defBoolMetadata(isEvaluated)
defBoolMetadata(isToplevel)
defBoolMetadata(isParsed)
defBoolMetadata(isGenerated)
defBoolMetadata(isCStruct)
defBoolMetadata(isExpanded)
defBoolMetadata(isElimEvaluated)
defBoolMetadata(isEliminated)

defMetadata(internalExpand, FExpr)

defMetadata(internalScope, Scope)
defMetadata(internalMark, InternalMarkKind)
defMetadata(internalPragma, InternalPragma)

defMetadata(internalIfExpr, IfExpr)
defMetadata(parent, FExpr)
defMetadata(globalindex, int)

proc copy*(fexpr: FExpr): FExpr

proc copyKind*(fexpr: FExpr): FExpr =
  new result
  result.kind = fexpr.kind
  result.span = fexpr.span
  result.metadata = initTable[string, Metadata]()
  
  case fexpr.kind
  of fexprIdent, fexprPrefix,  fexprInfix:
    result.idname = fexpr.idname
    result.priority = fexpr.priority
    result.isleft = fexpr.isleft
  of fexprQuote:
    result.quoted = fexpr.quoted.copy
  of fexprSymbol:
    result.symbol = fexpr.symbol
  of fexprIntLit:
    result.intval = fexpr.intval
  of fexprFloatLit:
    result.floatval = fexpr.floatval
  of fexprStrLit:
    result.strval = fexpr.strval
  of fexprContainer:
    result = fcontainer(fexpr.span, fexpr.kind)
    result.metadata = initTable[string, Metadata]()
    for son in fexpr:
      result.addSon(son.copy)
      
macro expandFExprCopy*(): untyped =
  let id = ident"copied"
  let fexpr = ident"fexpr"
  # echo parsedstmt.repr
  result = quote do:
    proc copy*(`fexpr`: FExpr): FExpr =
      let `id` = `fexpr`.copyKind
      `parsedstmt`
      return `id`

expandFExprCopy()
