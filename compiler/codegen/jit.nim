
import streams
import marshal
import dynlib
import ../internalffi

type
  AllocationType = enum
    allocMemCommit = 0x1000
  Protect = enum
    protectExecuteRW = 0x40
  FreeType = enum
    freeMemDecommit = 16384

when defined(windows):
  proc VirtualAlloc(lpAddress: pointer, size: int, alloctype: AllocationType, protect: Protect): pointer {.importc, header: "windows.h", stdcall.}
  proc VirtualFree(lpAddress: pointer, size: int, freetype: FreeType): bool {.importc, header: "windows.h", stdcall.}

type
  RelocKind* = enum
    relocDLL
    relocInternal
    relocStrLit
  Reloc* = object
    address*: int
    case kind*: RelocKind
    of relocDLL:
      dllname*: string
      dllprocname*: string
    of relocInternal:
      internalprocname*: string
    of relocStrLit:
      strpos*: int32
  JitBuffer* = ref object
    mem: pointer
    cap: int
    len*: int
    relocs*: seq[Reloc]
  JitProc* = object
    buf: JitBuffer
    pos*: int

proc initJitBuffer*(size: int): JitBuffer =
  when defined(windows):
    let mem = cast[ptr uint8](VirtualAlloc(nil, size, allocMemCommit, protectExecuteRW))
  JitBuffer(mem: mem, cap: size, len: 0, relocs: @[])
proc extend*(buf: var JitBuffer) =
  if buf.len+1 >= buf.cap:
    let newmem = VirtualAlloc(nil, buf.cap*2, allocMemCommit, protectExecuteRW)
    copyMem(newmem, buf.mem, buf.cap)
    discard VirtualFree(buf.mem, buf.cap, freeMemDecommit)
    buf.mem = newmem
    buf.cap *= 2
proc add*(buf: var JitBuffer, x: uint8) =
  buf.extend()
  cast[ptr uint8](cast[int](buf.mem) + buf.len)[] = x
  buf.len += 1
proc baseaddr*(buf: JitBuffer): int =
  cast[int](buf.mem)
proc baseptr*(buf: JitBuffer): pointer =
  buf.mem
proc getproc*(buf: JitBuffer): JitProc =
  JitProc(buf: buf, pos: buf.len)
proc addDLLReloc*(buf: var JitBuffer, dllname: string, fnname: string, address: int) =
  buf.relocs.add(Reloc(kind: relocDLL, dllname: dllname, dllprocname: fnname, address: address))
proc addInternalReloc*(buf: var JitBuffer, procname: string, address: int) =
  buf.relocs.add(Reloc(kind: relocInternal, internalprocname: procname, address: address))
proc addStrLitReloc*(buf: var JitBuffer, strpos: int32, address: int) =
  buf.relocs.add(Reloc(kind: relocStrLit, strpos: strpos, address: address))

proc baseaddr*(buf: seq[uint8]): int = 0
proc addDLLReloc*(buf: var seq[uint8], dllname: string, fnname: string, address: int) = discard
proc addInternalReloc*(buf: var seq[uint8], procname: string, address: int) = discard
proc addStrLitReloc*(buf: var seq[uint8], strpos: int, address: int) = discard

proc toProc*[F](prc: JitProc): F =
  cast[F](cast[int](prc.buf.mem) + prc.pos)

proc toBin*(buf: JitBuffer, s, e: int): string =
  result = newString(e-s+1)
  for i in s..e:
    result[i-s] = cast[ptr char](cast[int](buf.mem) + i)[]
proc toBin*(buf: JitBuffer): string =
  result = newString(buf.len)
  for i in 0..<buf.len:
    result[i] = cast[ptr char](cast[int](buf.mem) + i)[]

proc write*(s: Stream, buf: JitBuffer) =
  s.write(buf.cap.int64)
  s.write(buf.len.int64)
  for i in 0..<buf.cap:
    s.write(cast[ptr char]((cast[int](buf.mem) + i))[])
  let relocstr = $$buf.relocs
  s.write(relocstr.len.int64)
  s.write(relocstr)
proc readJitBuffer*(s: Stream): JitBuffer =
  let cap = s.readInt64()
  let len = s.readInt64()
  result = initJitBuffer(cap.int)
  for i in 0..<cap:
    result.add(s.readUint8())
  let reloclen = s.readInt64()
  let relocstr = s.readStr(reloclen.int)
  result.cap = cap.int
  result.len = len.int
  result.relocs = to[seq[Reloc]](relocstr)

iterator items*(buf: JitBuffer): uint8 =
  for i in 0..<buf.len:
    yield(cast[ptr uint8](cast[int](buf.mem) + i)[])

proc littleEmb(buf: JitBuffer, pos: int, x: int32) =
  let l1 = x and 255
  let l2 = (x shr 8) and 255
  let l3 = (x shr 16) and 255
  let l4 = (x shr 24) and 255
  cast[ptr uint8](cast[int](buf.mem) + pos + 0)[] = uint8(l1)
  cast[ptr uint8](cast[int](buf.mem) + pos + 1)[] = uint8(l2)
  cast[ptr uint8](cast[int](buf.mem) + pos + 2)[] = uint8(l3)
  cast[ptr uint8](cast[int](buf.mem) + pos + 3)[] = uint8(l4)

proc relocation*(buf: JitBuffer) =
  for reloc in buf.relocs:
    case reloc.kind
    of relocDLL:
      let lib = loadLib(reloc.dllname)
      buf.littleEmb(reloc.address, cast[int32](lib.checkedSymAddr(reloc.dllprocname)))
    of relocInternal:
      buf.littleEmb(reloc.address, cast[int32](getInternalFFI(reloc.internalprocname)))
    of relocStrLit:
      buf.littleEmb(reloc.address, int32(buf.baseaddr) + reloc.strpos)
