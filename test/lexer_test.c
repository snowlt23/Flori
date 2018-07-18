#include <assert.h>
#include <string.h>
#include "flori.h"

int main() {
  lexer* lx = new_lexer(stdin, "stdin.flori");
  tokenstream* ts = lex(lx);
  for (;;) {
    token* t = next_token(ts);
    if (t == NULL) break;
    if (t->kind == TOKEN_INTLIT) {
      printf("%s:%d ", tokenkind_tostring(t->kind), t->intval);
    } else if (t->kind == TOKEN_IDENT) {
      printf("%s:%s ", tokenkind_tostring(t->kind), t->ident);
    } else if (t->kind == TOKEN_OP) {
      printf("%s:%s ", tokenkind_tostring(t->kind), t->ident);
    } else {
      printf("%s ", tokenkind_tostring(t->kind));
    }
  }
}
