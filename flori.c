#include <stdio.h>
#include "flori.h"

int main() {
  lexer* lx = new_lexer(stdin, "stdin.flori");
  tokenstream* ts = lex(lx);
  fexpr* f = parse_fexpr(ts);
  init_codegen(stdout);
  codegen_main(f);
}
