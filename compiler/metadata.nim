
import macros
import types
import tables
import options

macro defMetadata*(key: untyped, T: typed): untyped =
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
  # echo result.repr
      
type
  InternalMarkKind* = enum
    internalDefn
    internalMacro
    internalDeftype
    internalIf
    internalWhile
    internalVar
    internalDef
    internalTrack
    internalSet
    internalFieldAccess
    internalInit
    internalImport
  InternalPragma* = object
    importc*: Option[string]
    header*: Option[string]
    infixc*: bool
    pattern*: Option[string]
    declc*: Option[string]
    internal*: bool

type
  DefnExpr* = object
    name*: FExpr
    generics*: FExpr
    args*: FExpr
    ret*: FExpr
    retgenerics*: FExpr
    isretref*: bool
    pragma*: FExpr
    body*: FExpr
  DeftypeExpr* = object
    name*: FExpr
    generics*: FExpr
    pragma*: FExpr
    body*: FExpr
  ProtocolExpr* = object
    name*: FExpr
    generics*: Option[FExpr]
    body*: FExpr
  InitExpr* = object
    typ*: FExpr
    body*: FExpr

type
  IfExpr* = object
    elifbranch*: seq[tuple[cond: FExpr, body: FExpr]]
    elsebranch*: FExpr
  WhileExpr* = object
    cond*: FExpr
    body*: FExpr
  DefExpr* = object
    name*: FExpr
    value*: FExpr
  SetExpr* = object
    dst*: FExpr
    value*: FExpr
  FieldAccessExpr* = object
    value*: FExpr
    fieldname*: FExpr
  ImportExpr* = object
    modname*: Name
    importname*: Name

defMetadata(typ, Symbol)
defMetadata(ctrc, CTRC)
defMetadata(effect, Effect)
defMetadata(evaluated, bool)

defMetadata(internalScope, Scope)
defMetadata(internalCtx, SemanticContext)
defMetadata(internalToplevel, bool)
defMetadata(internalMark, InternalMarkKind)
defMetadata(internalPragma, InternalPragma)

defMetadata(initexpr, InitExpr)
defMetadata(defn, DefnExpr)
defMetadata(deftype, DeftypeExpr)
defMetadata(cstruct, bool)

defMetadata(internalIfExpr, IfExpr)
defMetadata(internalWhileExpr, WhileExpr)
defMetadata(internalDefExpr, DefExpr)
defMetadata(internalSetExpr, SetExpr)
defMetadata(internalFieldAccessExpr, FieldAccessExpr)
defMetadata(internalImportExpr, ImportExpr)
defMetadata(parent, FExpr)

proc isToplevel*(fexpr: FExpr): bool = fexpr.hasInternalToplevel and fexpr.internalToplevel
proc `isToplevel=`*(fexpr: FExpr, b: bool) = fexpr.internalToplevel = b

proc isGenerics*(defn: DefnExpr): bool = not defn.generics.isSpecTypes
proc isGenerics*(deftype: DeftypeExpr): bool = not deftype.generics.isSpecTypes
