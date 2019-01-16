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

void stream_back(Stream* s, IString id) {
  s->pos -= strlen(istring_cstr(id));
}

//
// parse
//

bool isident(char c) {
  return isalpha(c) || isdigit(c) || c == '_';
}

bool isspaces(char c) {
  return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

bool is_end_delim(char c) {
  return c == ' ' || c == ',' || c == '\r' || c == '\n' || c == '\0';
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

bool ishex(char c) {
  return isalpha(c) || c == 'x';
}

void skip_spaces(Stream* s) {
  streamrep(i, s) {
    if (isspaces(stream_get(s))) {
      stream_next(s);
    } else if (stream_get(s) == '#') {
      streamrep(i, s) {
        if (stream_get(s) == '\n') {
          break;
        }
        stream_next(s);
      }
    } else {
      break;
    }
  }
}

bool is_opid(IString id) {
  char* s = istring_cstr(id);
  while (*s) {
    if (!isoperator(*s)) return false;
    s++;
  }
  return true;
}

int calc_op_priority(char* opident) {
  int l = strlen(opident);
  if (opident[0] == '<' || opident[0] == '>') {
    return 7;
  } else if (l >= 2 && (opident[0] == '!' || opident[0] == '=') && opident[1] == '=') {
    return 7;
  } else if (l >= 2 && opident[1] == '=') {
    return 15;
  } else if (opident[0] == '&' || opident[0] == '|') {
    return 12;
  } else if (opident[0] == '+' || opident[0] == '-' || opident[0] == '!') {
    return 5;
  } else if (opident[0] == '*' || opident[0] == '/' || opident[0] == '%') {
    return 4;
  } else if (opident[0] == '.') {
    return 1; 
  } else if (opident[0] == '=') {
    return 15;
  } else {
    assert(false);
  }
}

//
// primitive parser
//

IString lex_ident(Stream* s) {
  char litbuf[1024] = {};
  streamrep(i, s) {
    assert(i < 1024);
    char c = stream_get(s);
    if (is_end_delim(c)) break;
    stream_next(s);
    litbuf[i] = c;
    if (c == '(' || c == '[' || c == '{') break;
  }
  return new_istring(strdup(litbuf));
}

FMap parse_fident(Stream* s) {
  char litbuf[1024] = {};
  streamrep(i, s) {
    assert(i < 1024);
    char c = stream_get(s);
    if (!isident(c)) break;
    stream_next(s);
    litbuf[i] = c;
  }
  return fident(new_istring(strdup(litbuf)));
}

FMap parse_fintlit(Stream* s) {
  char litbuf[1024] = {};
  streamrep(i, s) {
    assert(i < 1024);
    char c = stream_get(s);
    if (!(isdigit(c) || ishex(c))) break;
    stream_next(s);
    litbuf[i] = c;
  }
  return fintlit(strtol(litbuf, NULL, 0));
}

FMap parse_fstrlit(Stream* s) {
  char litbuf[1024*1024] = {};
  streamrep(i, s) {
    assert(i < 1024*1024);
    char c = stream_get(s);
    stream_next(s);
    if (c == '"') break;
    litbuf[i] = c;
  }
  return fstrlit(new_istring(strdup(litbuf)));
}

FMap void_typef() {
  def_fmap(f, type, {
      def_field(t, fstrlit(new_istring("void")));
    });
  return f;
}

//
// internal parser
//

FMap parse_list(Stream* s) {
  FMap f = flist();
  streamrep(i, s) {
    skip_spaces(s);
    if (stream_get(s) == ',') stream_next(s);
    if (stream_get(s) == ')') {
      stream_next(s);
      break;
    }
    flist_push(f, parse(s));
  }
  *fm(f) = *fm(flist_reverse(f));
  return f;
}

FMap parse_block(Stream* s) {
  FMap f = flist();
  streamrep(i, s) {
    skip_spaces(s);
    if (stream_get(s) == '}') {
      stream_next(s);
      break;
    }
    flist_push(f, parse(s));
  }
  *fm(f) = *fm(flist_reverse(f));
  fm(f)->kind = new_istring("block");
  return f;
}

FMap parse_fn(Stream* s) {
  def_fmap(f, fn, {
      skip_spaces(s);
      def_field(name, parse_fident(s));
      def_field(args, parse(s));
      FMap rettype = parse(s);
      FMap body;
      if (eq_kind(rettype, new_istring("block"))) {
        body = rettype;
        rettype = void_typef();
      } else {
        body = parse(s);
      }
      def_field(returntype, rettype);
      def_field(body, body);
    });
  return f;
}

void def_parser(char* name, FMap (*internalfn)(Stream* s)) {
  add_parser_decl(new_internal_parserdecl(new_istring(name), internalfn));
}

void parser_init_internal() {
  def_parser("(", parse_list);
  def_parser("{", parse_block);
  def_parser("fn", parse_fn);
}

//
//
//

FMap parse_prim(Stream* s) {
  skip_spaces(s);
  IString id = lex_ident(s);
  if (isdigit(*istring_cstr(id))) {
    stream_back(s, id);
    return parse_fintlit(s);
  } else if (*istring_cstr(id) == '"') {
    stream_back(s, id);
    s->pos++;
    return parse_fstrlit(s);
  } else {
    return fident(id);
  }
}

FMap parse_infix5(Stream* s) {
  FMap left = parse_prim(s);
  streamrep(i, s) {
    skip_spaces(s);
    IString id = lex_ident(s);
    if (!is_opid(id) || calc_op_priority(istring_cstr(id)) != 5) {
      stream_back(s, id);
      break;
    }
    FMap args = flist();
    flist_push(args, parse_prim(s));
    flist_push(args, left);
    left = fcall(fident(id), args);
  }
  return left;
}

FMap parse(Stream* s) {
  skip_spaces(s);
  IString id = lex_ident(s);
  ParserDecl pdecl;
  if (search_parser_decl(id, &pdecl)) {
    if (pdecl.internalfn != NULL) {
      return (pdecl.internalfn)(s);
    } else {
      assert(false); // TODO:
    }
  }

  stream_back(s, id);
  return parse_infix5(s);
}
