#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "flori.h"

int main() {
  assert(strcmp(tokenkind_tostring(TOKEN_ADD), "TOKEN_ADD") == 0);
  assert(strcmp(tokenkind_tostring(TOKEN_SUB), "TOKEN_SUB") == 0);
}
