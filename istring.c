#include "flori.h"
#include <string.h>

IString new_istring(char* s) {
  int slen = strlen(s);
  int idx = linmem_alloc(slen+1);
  strcpy(linmem_toptr(idx), s);
  return (IString){idx};
}

char* istring_cstr(IString s) {
  return (char*)linmem_toptr(s.index);
}

bool istring_eq(IString a, IString b) {
  return strcmp(istring_cstr(a), istring_cstr(b)) == 0;
}