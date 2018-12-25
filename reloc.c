#include "flori.h"

RelocList relocs;

void fixup_lendian32(uint8_t* addr, int x) {
  int b1 = x & 0xFF;
  int b2 = (x >> 8) & 0xFF;
  int b3 = (x >> 16) & 0xFF;
  int b4 = (x >> 24) & 0xFF;
  addr[0] = b1;
  addr[1] = b2;
  addr[2] = b3;
  addr[3] = b4;
}

void fixup_lendian64(uint8_t* addr, size_t x) {
  int b1 = x & 0xFF;
  int b2 = (x >> 8) & 0xFF;
  int b3 = (x >> 16) & 0xFF;
  int b4 = (x >> 24) & 0xFF;
  int b5 = (x >> 32) & 0xFF;
  int b6 = (x >> 40) & 0xFF;
  int b7 = (x >> 48) & 0xFF;
  int b8 = (x >> 56) & 0xFF;
  addr[0] = b1;
  addr[1] = b2;
  addr[2] = b3;
  addr[3] = b4;
  addr[4] = b5;
  addr[5] = b6;
  addr[6] = b7;
  addr[7] = b8;
}

void reloc_init() {
  relocs = nil_RelocList();
}

void reloc_add_info(size_t jitidx, size_t dataidx) {
  relocs = new_RelocList((RelocInfo){jitidx, dataidx}, relocs);
}

void reloc_execute() {
  forlist (RelocList, RelocInfo, rinfo, relocs) {
    fixup_lendian64(jit_toptr(rinfo.jitidx), (size_t)data_toptr(rinfo.dataidx));
  }
}

void reloc_execute_addr(uint8_t* addr) {
  data_set_memptr(addr);
  reloc_execute();
}