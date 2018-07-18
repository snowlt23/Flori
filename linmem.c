#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include "flori.h"

uint8_t* linmem = NULL;
int linmempos = 0;
int linmemsize = 0;

void init_linmem_cap(int size) {
  linmem = malloc(size);
  linmempos = 0;
  linmemsize = size;
}

void init_linmem() {
  init_linmem_cap(1024*1024);
}

void linmem_extend(int size) {
  if (linmempos+size >= linmemsize) {
    while (linmempos+size < linmemsize) {
      linmemsize *= 2;
    }
    linmem = realloc(linmem, linmemsize);
  }
}

int linmem_alloc(int size) {
  linmem_extend(size);
  int p = linmempos;
  linmempos += size;
  return p;
}

void* linmem_toptr(int pos) {
  return (void*)(linmem+pos);
}

//
// istring
//

istring new_istring(char* s) {
  istring is;
  is.index = linmem_alloc((strlen(s)+1) * sizeof(char));
  is.len = strlen(s);
  for (int i=0; i<strlen(s); i++) {
    *(char*)linmem_toptr(is.index + sizeof(char)*i) = s[i];
  }
  *(char*)linmem_toptr(is.index + sizeof(char)*strlen(s)) = '\0';
  return is;
}

char* istring_cstr(istring is) {
  return (char*)linmem_toptr(is.index);
}

//
// iarray_fexpr
//

iarray_fexpr new_iarray_fexpr(int len) {
  iarray_fexpr ia;
  ia.index = linmem_alloc(len * sizeof(fexpr));
  ia.len = len;
  return ia;
}

void iarray_fexpr_set(iarray_fexpr ia, int index, fexpr f) {
  assert(index < ia.len);
  *(fexpr*)linmem_toptr(ia.index + sizeof(fexpr)*index) = f;
}

fexpr iarray_fexpr_get(iarray_fexpr ia, int index) {
  assert(index < ia.len);
  return *(fexpr*)linmem_toptr(ia.index + sizeof(fexpr)*index);
}
