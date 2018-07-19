#include <stdio.h>
#include "flori.h"

int main() {
  init_linmem();
  init_codegen(stdout);
  lexer* lx = new_lexer(stdin, "stdin.flori");
  tokenstream* ts = offside_rule(lex(lx));
  for (;;) {
    if (get_token(ts) == NULL) break;
    fexpr f = parse_fexpr(ts);
    codegen_fn(f);
  }
  codegen_startup();
}
