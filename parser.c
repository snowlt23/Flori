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
  if (t->kind == TOKEN_LPAREN) {
    next_token(ts);
    fexpr f = parse_fexpr(ts);
    if (next_token(ts)->kind != TOKEN_RPAREN) {fprintf(stderr, "unmatched lparen`(` to rparen`)`."); exit(1);} // FIXME: change to parse_error(...)
    return f;
  } else if (t->kind == TOKEN_IDENT) {
    next_token(ts);
    return new_fident(t->ident);
  } else if (t->kind == TOKEN_INTLIT) {
    next_token(ts);
    return new_fintlit(t->intval);
  } else {
    return parse_fexpr(ts);
  }
}

fexpr parse_block(tokenstream* ts) {
  if (get_token(ts)->kind == TOKEN_LBLOCK) {
    fexpr f[1024] = {};
    next_token(ts);
    for (int i=0; ; i++) {
      if (get_token(ts)->kind == TOKEN_RBLOCK) {
        next_token(ts);
        fexpr fblk = new_fexpr(FEXPR_BLOCK);
        fexpr_ptr(fblk)->sons = new_iarray_fexpr(i);
        for (int j=0; j<i; j++) {
          iarray_fexpr_set(fexpr_ptr(fblk)->sons, j, f[j]);
        }
        return fblk;
      }
      f[i] = parse_fexpr(ts);
    }
  } else {
    return parse_factor(ts);
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

DEF_PARSE_INFIX(4, parse_block);
DEF_PARSE_INFIX(5, parse_infix4);
DEF_PARSE_INFIX(7, parse_infix5);
DEF_PARSE_INFIX(15, parse_infix7);

fexpr parse_fexpr(tokenstream* ts) {
  return parse_infix15(ts);
}
