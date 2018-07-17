#include <stdio.h>
#include "flori.h"

lexer* new_lexer(FILE* handle, char* filename) {
  lexer* lx = malloc(sizeof(lexer));
  lx->handle = handle;
  lx->filename = filename;
  lx->line = 1;
  lx->column = 1;
  return lx;
}

