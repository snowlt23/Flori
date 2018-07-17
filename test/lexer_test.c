#include <assert.h>
#include <string.h>
#include "flori.h"

int main() {
  lexer* lx = new_lexer(stdin, "stdin.flori");
  tokenstream* ts = lex(lx);
  for (;;) {
    token* t = next_token(ts);
    if (t == NULL) break;
    printf("%s ", tokenkind_tostring(t->kind));
  }
}
