#include <assert.h>
#include <string.h>
#include "flori.h"

int main() {
  lexer* lx = new_lexer(stdin, "stdin.flori");
  tokenstream* ts = lex(lx);
  fexpr* f = parse_fexpr(ts);
  printf("%s", fexprkind_tostring(f->kind));
}
