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
  fe(f)->kind = kind;
  return f;
}

FExpr fident(char* id) {
  FExpr f = new_fexpr(FEXPR_IDENT);
  fe(f)->ident = new_istring(id);
  return f;
}

FExpr fop(char* id) {
  FExpr f = new_fexpr(FEXPR_OP);
  fe(f)->ident = new_istring(id);
  return f;
}

bool cmp_ident(FExpr f, char* id) {
  return strcmp(istring_cstr(fe(f)->ident), id) == 0;
}

FExpr new_fcontainer(FExprKind kind) {
  FExpr f = alloc_FExpr();
  fe(f)->kind = kind;
  fe(f)->sons = nil_IListFExpr();
  return f;
}

void push_son(FExpr f, FExpr son) {
  fe(f)->sons = new_IListFExpr(son, fe(f)->sons);
}

void reverse_sons(FExpr f) {
  fe(f)->sons = IListFExpr_reverse(fe(f)->sons);
}

FExpr copy_fexpr(FExpr f) {
  FExpr newf = alloc_FExpr();
  *fe(newf) = *fe(f);
  return newf;
}

//
// parse
//

bool isident(char c) {
  return isalpha(c) || isdigit(c) || c == '_';
}

bool isspaces(char c) {
  return c == ' ' || c == '\t' || c == '\r';
}

bool isoperator(char c) {
  static char ifx[] = {
    '+', '-', '*', '/', '%',
    '<', '>', '.', '=', ':',
    '!', '&', '|', '~'
  };
  for (int i=0; i<sizeof(ifx); i++) {
    if (c == ifx[i]) {
      return true;
    }
  }
  return false;
}

void skip_spaces(Stream* s) {
  for (;;) {
    if (isspaces(stream_get(s))) {
      stream_next(s);
    } else {
      break;
    }
  }
}

FExpr parse_ident(Stream* s) {
  char litbuf[1024] = {};
  streamrep(i, s) {
    assert(i < 1024);
    char c = stream_get(s);
    if (!isident(c)) break;
    stream_next(s);
    litbuf[i] = c;
  }
  FExpr f = new_fexpr(FEXPR_IDENT);
  fe(f)->ident = new_istring(strdup(litbuf));
  return f;
}

FExpr parse_operator(Stream* s) {
  char litbuf[1024] = {};
  streamrep(i, s) {
    assert(i < 1024);
    char c = stream_get(s);
    if (!isoperator(c)) break;
    stream_next(s);
    litbuf[i] = c;
  }
  FExpr f = new_fexpr(FEXPR_OP);
  fe(f)->ident = new_istring(strdup(litbuf));
  return f;
}

bool ishex(char c) {
  return isalpha(c) || c == 'x';
}

FExpr parse_intlit(Stream* s) {
  char litbuf[1024] = {};
  streamrep(i, s) {
    assert(i < 1024);
    char c = stream_get(s);
    if (!(isdigit(c) || ishex(c))) break;
    stream_next(s);
    litbuf[i] = c;
  }
  FExpr f = new_fexpr(FEXPR_INTLIT);
  fe(f)->intval = strtol(litbuf, NULL, 0);
  return f;
}

FExpr parse_flist(Stream* s) {
  if (stream_next(s) != '(') error("expect `( token");
  IListFExpr sons = nil_IListFExpr();
  streamrep(i, s) {
    skip_spaces(s);
    if (stream_get(s) == ')') break;
    FExpr son = parse(s);
    if (son.index == -1) continue;
    sons = new_IListFExpr(son, sons);
  }
  if (stream_next(s) != ')') error("expect `) token");
  FExpr f = new_fexpr(FEXPR_LIST);
  fe(f)->sons = IListFExpr_reverse(sons);
  return f;
}

FExpr parse_fblock(Stream* s) {
  if (stream_next(s) != '{') error("expect `{ token");
  IListFExpr sons = nil_IListFExpr();
  streamrep(i, s) {
    skip_spaces(s);
    if (stream_get(s) == '}') break;
    FExpr son = parse(s);
    if (son.index == -1) continue;
    sons = new_IListFExpr(son, sons);
  }
  if (stream_next(s) != '}') error("expect `} token");
  FExpr f = new_fexpr(FEXPR_BLOCK);
  fe(f)->sons = IListFExpr_reverse(sons);
  return f;
}

FExpr parse_reader_type(Stream* s) {
  if (stream_next(s) != '^') error("expect `^ reader token.");
  FExpr typeid = parse_ident(s);
  if (cmp_ident(typeid, "ptr")) {
    skip_spaces(s);
    FExpr f = new_fcontainer(FEXPR_SEQ);
    push_son(f, parse_ident(s));
    push_son(f, fident("type"));
    fseq(retf, fident("type_ptr"), f);
    return retf;
  } else {
    fseq(f, fident("type"), typeid);
    return f;
  }
}

FExpr parse_element(Stream* s) {
  if (isdigit(stream_get(s))) {
    return parse_intlit(s);
  } else if (isident(stream_get(s))) {
    return parse_ident(s);
  } else if (isoperator(stream_get(s))) {
    return parse_operator(s);
  } else if (stream_get(s) == '(') {
    return parse_flist(s);
  } else if (stream_get(s) == '{') {
    return parse_fblock(s);
  } else if (stream_get(s) == '^') {
    return parse_reader_type(s);
  } else {
    error("unexpected %c char", stream_get(s));
  }
}

FExpr parse(Stream* s) {
  IListFExpr sons = nil_IListFExpr();
  for (;;) {
    skip_spaces(s);
    if (stream_get(s) == '\n' || stream_get(s) == ';' || stream_get(s) == ',') {
      stream_next(s);
      break;
    } else if (stream_get(s) == '}' || stream_get(s) == ')') {
      break;
    }
    sons = new_IListFExpr(parse_element(s), sons);
  }
  if (IListFExpr_len(sons) == 0) {
    return (FExpr){-1};
  }
  if (IListFExpr_len(sons) == 1) {
    return IListFExpr_value(sons);
  }
  FExpr f = new_fexpr(FEXPR_SEQ);
  fe(f)->sons = IListFExpr_reverse(sons);
  return f;
}
