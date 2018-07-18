#include <assert.h>
#include <string.h>
#include "flori.h"

int main() {
  init_linmem();
  lexer* lx = new_lexer(stdin, "stdin.flori");
  tokenstream* ts = offside_rule(lex(lx));
  fexpr f = parse_fexpr(ts);
  printf("%s", fexprkind_tostring(fexpr_ptr(f)->kind));
}
