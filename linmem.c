#include "flori.h"

uint8_t* linmemptr;
int linmempos;
int linmemcap;

void linmem_init(int size) {
  linmemptr = malloc(size);
  linmempos = 0;
  linmemcap = size;
}

bool linmem_need_extend(int size) {
  return linmempos + size >= linmemcap;
}

void linmem_extend(int size) {
  while (linmem_need_extend(size)) {
    linmemcap *= 2;
  }
}

int linmem_alloc(int size) {
  linmem_extend(size);
  int idx = linmempos;
  linmempos += size;
  return idx;
}

void* linmem_toptr(int index) {
  return (void*)(linmemptr + index);
}
