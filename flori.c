#include "flori.h"
#include <string.h>

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

void generate_executable(char* filename) {
  FILE* fp = fopen(filename, "wb");
  write_elf_executable(fp, jit_codeptr(), jit_codesize(), get_main_offset());
  fclose(fp);
}

int main(int argc, char** argv) {
  linmem_init(1024*1024);
  jit_init(1024);
  semantic_init();
  Stream* s = new_stream(read_stdin());
  while (!stream_isend(s)) {
    if (linmem_need_extend()) linmem_extend();
    FExpr f = parse(s);
    if (f.index == -1) continue;
    semantic_analysis(f);
    codegen(f);
  }

  if (argc == 3) {
    if (strcmp(argv[1], "-o") == 0) {
      char* outname = argv[2];
      generate_executable(outname);
    } else {
      error("unknown %s command-line option.", argv[1]);
    }
  } else {
    jit_write_to_file("buffer.jit");
    printf("%d", call_main());
  }
}
