#include "flori.h"
#include <string.h>

#ifdef WIN32
  #include <windows.h>
  #define jit_memalloc(size) VirtualAlloc(0, size, MEM_COMMIT, PAGE_EXECUTE_READWRITE)
  #define jit_memfree(p, size) VirtualFree(p, size, MEM_DECOMMIT)
#else
  #include <sys/mman.h>
  #include <dlfcn.h>
  #define jit_memalloc(size) mmap(NULL, size, PROT_EXEC|PROT_READ|PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, 0, 0)
  #define jit_memfree(p, size) munmap(p, size)
#endif

uint8_t* jitptr;
int jitpos;
int jitcap;
int jit_needcap;

void jit_init(int size) {
  jitptr = jit_memalloc(size);
  jitpos = 0;
  jitcap = size;
  jit_needcap = size;
}

bool jit_need_extend() {
  return jitpos + jit_needcap >= jitcap;
}

void jit_extend() {
  int prevcap = jitcap;
  while (jit_need_extend()) {
    jitcap *= 2;
  }
  uint8_t* newptr = jit_memalloc(jitcap);
  memcpy(newptr, jitptr, jitpos);
  jit_memfree(jitptr, prevcap);
  jitptr = newptr;
}

int jit_getidx() {
  return jitpos;
}

int jit_alloc_write(uint8_t* buf, int n) {
  int idx = jitpos;
  memcpy(jitptr+jitpos, buf, n);
  jitpos += n;
  return idx;
}

void* jit_toptr(int index) {
  return (void*)(jitptr + index);
}

void jit_write_to_file(char* filename) {
  FILE* f = fopen(filename, "wb");
  for (uint8_t* p = jitptr; p < jitptr+jitcap; p++) {
    putc(*p, f);
  }
  fclose(f);
}

uint8_t* jit_codeptr() {
  return jitptr;
}

size_t jit_codesize() {
  return jitpos;
}
