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
  linmem_init(1024*1024);
  jit_init(1024);
  semantic_init();
  codegen_init();
  Stream* s = new_stream(read_stdin());
  while (!stream_isend(s)) {
    if (linmem_need_extend()) linmem_extend();
    FExpr f = parse(s);
    if (f.index == -1) continue;
    semantic_analysis(f);
    codegen(f);
  }
  jit_write_to_file("buffer.jit");
  printf("%d", call_main());
}
