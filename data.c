#include "flori.h"
#include <string.h>

uint8_t* dataptr;
size_t datapos;
size_t datacap;

void data_init(size_t size) {
  dataptr = malloc(size);
  datapos = 0;
  datacap = size;
}

bool data_need_extend(size_t size) {
  return datapos + size >= datacap;
}

void data_extend(size_t size) {
  while (data_need_extend(size)) {
    datacap *= 2;
  }
  dataptr = realloc(dataptr, datacap);
}

size_t data_alloc(size_t size) {
  size_t idx = datapos;
  datapos += size;
  return idx;
}

void* data_toptr(size_t idx) {
  return (void*)(dataptr + idx);
}

size_t data_cstring(char* s) {
  size_t idx = data_alloc(strlen(s)+1);
  strcpy((char*)data_toptr(idx), s);
  return idx;
}

uint8_t* data_memptr() {
  return dataptr;
}

void data_set_memptr(uint8_t* p) {
  dataptr = p;
}

size_t data_memsize() {
  return datapos;
}