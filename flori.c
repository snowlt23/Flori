#include <stdio.h>
#include "flori.h"

int main() {
  init_linmem();
  lexer* lx = new_lexer(stdin, "stdin.flori");
  tokenstream* ts = offside_rule(lex(lx));
  fexpr f = parse_fexpr(ts);
  init_codegen(stdout);
  codegen_fn(f);
  codegen_startup();
}
