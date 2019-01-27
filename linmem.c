#include "flori.h"

uint8_t* linmemptr;
int linmempos;
int linmemcap;
int linmem_needcap;

void linmem_init(int size) {
  linmemptr = malloc(size);
  linmempos = 0;
  linmemcap = size;
  linmem_needcap = size;
}

bool linmem_need_extend() {
  return linmempos + linmem_needcap >= linmemcap;
}

void linmem_extend() {
  while (linmem_need_extend()) {
    linmemcap *= 2;
  }
  linmemptr = realloc(linmemptr, linmemcap);
}

int linmem_alloc(int size) {
  assert(linmempos < linmemcap);
  int idx = linmempos;
  linmempos += size;
  return idx;
}

void* linmem_toptr(int index) {
  assert(index >= 0);
  return (void*)(linmemptr + index);
}

int linmem_getidx() {
  return linmempos;
}
