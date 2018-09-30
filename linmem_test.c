#include "flori.h"

int main() {
  linmem_init(1);
  if (linmem_need_extend()) linmem_extend();
  int idx1 = linmem_alloc(8);
  assert(idx1 == 0);
  int idx2 = linmem_alloc(12);
  assert(idx2 == 8);
  int idx3 = linmem_alloc(1);
  assert(idx3 == 20);
  assert(linmem_toptr(idx2) - linmem_toptr(idx1) == 8);
  assert(linmem_toptr(idx3) - linmem_toptr(idx2) == 12);
  assert(linmem_toptr(idx3) - linmem_toptr(idx1) == 20);
  return 0;
}
