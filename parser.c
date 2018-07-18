#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "flori.h"

fexpr new_fexpr(fexprkind kind) {
  fexpr f = alloc_fexpr();
  fexpr_ptr(f)->kind = kind;
  return f;
}
fexpr new_fident(char* id) {
  fexpr f = new_fexpr(FEXPR_IDENT);
  fexpr_ptr(f)->ident = new_istring(id);
  return f;
}

fexpr new_fintlit(int x) {
  fexpr f = new_fexpr(FEXPR_INTLIT);
  fexpr_ptr(f)->intval = x;
  return f;
}

fexpr new_finfix(fexpr call, fexpr left, fexpr right) {
  fexpr f = new_fexpr(FEXPR_INFIX);
  fexpr_ptr(f)->call = call;
  fexpr_ptr(f)->arguments = new_iarray_fexpr(2);
  iarray_fexpr_set(fexpr_ptr(f)->arguments, 0, left);
  iarray_fexpr_set(fexpr_ptr(f)->arguments, 1, right);
  return f;
}

fexpr parse_factor(tokenstream* ts) {
  token* t = get_token(ts);
  if (t->kind == TOKEN_INTLIT) {
    next_token(ts);
    return new_fintlit(t->intval);
  } else {
    return parse_fexpr(ts);
  }
}

#define DEF_PARSE_INFIX(pri, call) \
  fexpr parse_infix ## pri(tokenstream* ts) { \
    fexpr left = call(ts); \
    for (;;) { \
      token* t = get_token(ts); \
      if (t != NULL && t->kind == TOKEN_OP && op_priority(t) == pri) { \
        next_token(ts); \
        left = new_finfix(new_fident(t->ident), left, call(ts)); \
      } else { \
        break; \
      } \
    } \
    return left; \
  }

DEF_PARSE_INFIX(4, parse_factor);
DEF_PARSE_INFIX(5, parse_infix4);
DEF_PARSE_INFIX(7, parse_infix5);
DEF_PARSE_INFIX(15, parse_infix7);

fexpr parse_fexpr(tokenstream* ts) {
  return parse_infix15(ts);
}
