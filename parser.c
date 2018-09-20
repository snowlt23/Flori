#include "flori.h"
#include <string.h>
#include <ctype.h>

//
// stream
//

#define streamrep(i, s) for (int i=0; !stream_isend(s); i++)

Stream* new_stream(char* buf) {
  Stream* s = malloc(sizeof(Stream));
  s->buf = buf;
  s->pos = 0;
  s->len = strlen(buf);
  return s;
}

char stream_get(Stream* s) {
  return s->buf[s->pos];
}
char stream_next(Stream* s) {
  return s->buf[s->pos++];
}
void stream_prev(Stream* s) {
  s->pos--;
}
bool stream_isend(Stream* s) {
  return s->pos >= s->len;
}

//
// ast
//

FExpr new_fexpr(FExprKind kind) {
  FExpr f = alloc_FExpr();
  %%fwith FExpr fobj = f;
  fobj->kind = kind;
  return f;
}

//
// parse
//

FExpr parse_intlit(Stream* s) {
  char litbuf[1024] = {};
  streamrep(i, s) {
    assert(i < 1024);
    char c = stream_get(s);
    if (!isdigit(c)) break;
    stream_next(s);
    litbuf[i] = c;
  }
  FExpr f = new_fexpr(FEXPR_INTLIT);
  %%fwith FExpr fobj = f;
  fobj->intval = atoi(litbuf);
  return f;
}

FExpr parse(Stream* s) {
  return parse_intlit(s);
}
