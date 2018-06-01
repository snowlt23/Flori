
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
  JitBuffer* = ref object
    mem: pointer
    cap: int
    len: int
  JitProc* = object
    buf: JitBuffer
    pos: int

proc initJitBuffer*(size: int): JitBuffer =
  when defined(windows):
    let mem = cast[ptr uint8](VirtualAlloc(nil, size, allocMemCommit, protectExecuteRW))
  JitBuffer(mem: mem, cap: size, len: 0)
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
proc getproc*(buf: JitBuffer): JitProc =
  JitProc(buf: buf, pos: buf.len)

proc toProc*[F](prc: JitProc): F =
  cast[F](cast[int](prc.buf.mem) + prc.pos)
