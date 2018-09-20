#include "flori.h"

char* read_stdin() {
  char* s = malloc(1024);
  int spos = 0;
  int scap = 1024;
  for (;;) {
    char c = getc(stdin);
    if (c == EOF) break;
    if (spos+1 >= scap) {
      scap *= 2;
      s = realloc(s, scap);
    }
    s[spos++] = c;
  }
  s[spos] = '\0';
  return s;
}

int main() {
  linmem_init(1024);
  Stream* s = new_stream(read_stdin());
  FExpr f = parse(s);
  printf("  .intel_syntax noprefix\n");
  printf("  .global main\n");
  printf("main:\n");
  codegen(f);
  printf("  ret\n");
  return 0;
}
