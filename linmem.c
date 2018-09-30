#include "flori.h"

#define LINMEM_TOPLEVEL_CAP (1024*1024)

uint8_t* linmemptr;
int linmempos;
int linmemcap;

void linmem_init(int size) {
  linmemptr = malloc(size);
  linmempos = 0;
  linmemcap = size;
}

bool linmem_is_shortage(int size) {
  return linmempos + size >= linmemcap;
}

bool linmem_need_extend() {
  return linmem_is_shortage(LINMEM_TOPLEVEL_CAP);
}

void linmem_extend() {
  while (linmem_need_extend()) {
    linmemcap *= 2;
  }
  linmemptr = realloc(linmemptr, linmemcap);
}

int linmem_alloc(int size) {
  assert(!linmem_is_shortage(size));
  int idx = linmempos;
  linmempos += size;
  return idx;
}

void* linmem_toptr(int index) {
  assert(index >= 0);
  return (void*)(linmemptr + index);
}
