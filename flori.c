#include "flori.h"
#include <string.h>
#include <sys/stat.h>

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

void define_entrypoint() {
  Stream* s = new_stream("fn __start() {\nmain; exit 0\n}\n");
  if (linmem_need_extend()) linmem_extend();
  FExpr f = parse(s);
  if (f.index == -1) assert(false);
  semantic_analysis(f);
  codegen(f);
}

size_t get_entrypoint_offset() {
  FnDecl fndecl;
  if (!search_fndecl(new_istring("__start"), new_FTypeVec(), &fndecl)) {
    error("undefined reference to `__start");
  }
  return fp(FSymbol, fndecl.sym)->fnidx;
}

void generate_executable(char* filename) {
  FILE* fp = fopen(filename, "wb");
  write_elf_executable(fp, jit_codeptr(), jit_codesize(), get_entrypoint_offset());
  fclose(fp);
  chmod(filename, S_IRUSR | S_IWUSR|S_IXUSR);
}

int main(int argc, char** argv) {
  linmem_init(1024*1024);
  jit_init(1024);
  data_init(1024);
  reloc_init();
  semantic_init();
  Stream* s = new_stream(read_stdin());
  while (!stream_isend(s)) {
    if (linmem_need_extend()) linmem_extend();
    if (jit_need_extend(1024)) jit_extend(1024);
    if (data_need_extend(1024)) {
      data_extend(1024);
      reloc_execute();
    }
    FExpr f = parse(s);
    if (f.index == -1) continue;
    semantic_analysis(f);
    codegen(f);
  }

  if (argc == 3) {
    if (strcmp(argv[1], "-o") == 0) {
      char* outname = argv[2];
      define_entrypoint();
      generate_executable(outname);
    } else {
      error("unknown %s command-line option.", argv[1]);
    }
  } else {
    jit_write_to_file("buffer.jit");
    printf("%d", call_main());
  }
}
