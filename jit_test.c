#include "flori.h"

int main() {
  jit_init(1);
  uint8_t buf[] = {
    0x00, 0x01, 0x02, 0x03
  };
  int idx = jit_getidx();
  jit_alloc_write(buf, 4);
  assert((*(uint8_t*)jit_toptr(idx+0)) == 0);
  assert((*(uint8_t*)jit_toptr(idx+1)) == 1);
  assert((*(uint8_t*)jit_toptr(idx+2)) == 2);
  assert((*(uint8_t*)jit_toptr(idx+3)) == 3);
}
