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
  return c == ' ' || c == '\t' || c == '\r';
}

bool is_end_delim(char c) {
  return c == ' ' || c == ',' || c == '\r' || c == '\n' || c == '\0' || c == ';';
}

bool isoperator(char c) {
  static char ifx[] = {
    '+', '-', '*', '/', '%',
    '<', '>', '.', '=', ':',
    '!', '&', '|', '~', '^'
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
  if (strlen(s) == 0) return false;
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
    debug("%s", opident);
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

IString lex_operator(Stream* s) {
  char litbuf[1024] = {};
  streamrep(i, s) {
    assert(i < 1024);
    char c = stream_get(s);
    if (!isoperator(c)) break;
    stream_next(s);
    litbuf[i] = c;
    if (c == '(' || c == '[' || c == '{') break;
  }
  return new_istring(strdup(litbuf));
}

FMap parse_declident(Stream* s) {
  char litbuf[1024] = {};
  streamrep(i, s) {
    assert(i < 1024);
    char c = stream_get(s);
    if (!isident(c) && !isoperator(c)) break;
    stream_next(s);
    litbuf[i] = c;
  }
  return fident(new_istring(strdup(litbuf)));
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
      def_field(t, fident(new_istring("void")));
    });
  return f;
}

FMap parse_type(Stream* s) {
  skip_spaces(s);
  IString id = lex_ident(s);
  if (istring_ceq(id, "ptr")) {
    def_fmap(f, type, {
        def_field(ptr, fintlit(1));
        skip_spaces(s);
        def_field(t, parse_fident(s));
      });
    return f;
  } else {
    stream_back(s, id);
    def_fmap(f, type, {
        def_field(t, parse_fident(s));
      });
    return f;
  }
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
    if (stream_get(s) == '\n' || stream_get(s) == ';') {
      stream_next(s);
      continue;
    }
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
      def_field(name, parse_declident(s));

      if (stream_next(s) != '(') error("expect function argdecls");
      FMap argdecls = flist();
      streamrep(i, s) {
        skip_spaces(s);
        if (stream_get(s) == ',') stream_next(s);
        if (stream_get(s) == ')') {
          stream_next(s);
          break;
        }
        
        def_fmap(ad, argdecl, {
            skip_spaces(s);
            def_field(name, parse_fident(s));
            def_field(type, parse(s));
          });
        flist_push(argdecls, ad);
      }
      *fm(argdecls) = *fm(flist_reverse(argdecls));
      def_field(argdecls, argdecls);
      
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

FMap parse_inline(Stream* s) {
  FMap f = parse(s);
  fmap_cpush(f, "inline", fintlit(1));
  return f;
}

FMap parse_defprimitive(Stream* s) {
  def_fmap(f, defprimitive, {
      skip_spaces(s);
      def_field(name, parse_fident(s));
      def_field(size, parse(s));
    });
  return f;
}

FMap parse_return(Stream* s) {
  FMap args = flist();
  flist_push(args, parse(s));
  return fcall(fident(new_istring("return")), args);
}

FMap parse_var(Stream* s) {
  def_fmap(f, var, {
      skip_spaces(s);
      def_field(name, parse_fident(s));
      skip_spaces(s);
      def_field(vartype, parse(s));
    });
  return f;
}

FMap parse_if(Stream* s) {
  FMap elifs = flist();
  fblockseq(els);
  def_fmap(elf, elif, {
      def_field(cond, parse(s));
      def_field(body, parse(s));
    });
  flist_push(elifs, elf);
  
  streamrep (i, s) {
    skip_spaces(s);
    IString id = lex_ident(s);
    if (istring_ceq(id, "elif")) {
      def_fmap(elf, elif, {
          def_field(cond, parse(s));
          def_field(body, parse(s));
        });
      flist_push(elifs, elf);
    } else if (istring_ceq(id, "else")) {
      els = parse(s);
    } else {
      stream_back(s, id);
      break;
    }
  }
  def_fmap(f, if, {
      def_field(elifs, elifs);
      def_field(else, els);
    });
  return f;
}

FMap parse_struct(Stream* s) {
  def_fmap(f, struct, {
      skip_spaces(s);
      def_field(name, parse_declident(s));

      skip_spaces(s);
      if (stream_next(s) != '{') error("expect struct body");
      FMap fields = flist();
      streamrep(i, s) {
        skip_spaces(s);
        if (stream_get(s) == '\n' || stream_get(s) == ';') {
          stream_next(s);
          continue;
        }
        if (stream_get(s) == '}') {
          stream_next(s);
          break;
        }
        
        def_fmap(field, field, {
            skip_spaces(s);
            def_field(name, parse_fident(s));
            def_field(type, parse(s));
          });
        flist_push(fields, field);
      }
      *fm(fields) = *fm(flist_reverse(fields));
      def_field(fields, fields);
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
  def_parser("inline", parse_inline);
  def_parser("defprimitive", parse_defprimitive);
  def_parser("return", parse_return);
  def_parser("var", parse_var);
  def_parser("if", parse_if);
  def_parser("^", parse_type);
  def_parser("struct", parse_struct);
}

//
//
//

FMap parse_prim(Stream* s) {
  skip_spaces(s);
  IString id = lex_ident(s);
  if (strlen(istring_cstr(id)) == 0) {
    return nil_FMap();
  } else if (isdigit(*istring_cstr(id))) {
    stream_back(s, id);
    return parse_fintlit(s);
  } else if (*istring_cstr(id) == '"') {
    stream_back(s, id);
    s->pos++;
    return parse_fstrlit(s);
  } else if (isident(*istring_cstr(id))) {
    stream_back(s, id);
    return parse_fident(s);
  } else {
    stream_back(s, id);
    return parse(s);
  }
}

FMap parse_call(Stream* s) {
  FMap f = parse_prim(s);
  skip_spaces(s);
  IString id = lex_ident(s);
  if (istring_ceq(id, "(")) {
    f = fcall(f, parse_list(s));
  } else {
    stream_back(s, id);
  }
  return f;
}

#define def_infix_parser(name, next, pri)                               \
  FMap name(Stream* s) {                                                \
    skip_spaces(s);                                                       \
    IString id = lex_operator(s);                                            \
    if (is_opid(id) && calc_op_priority(istring_cstr(id)) == pri) {      \
      flistseq(args, name(s));                                          \
      return fcall(fident(id), args);                                  \
    }                                                                     \
    stream_back(s, id);                                                   \
    FMap left = next(s);                                                \
    streamrep(i, s) {                                                   \
      skip_spaces(s);                                                   \
      if (stream_get(s) == '\n' || stream_get(s) == ';') break; \
      IString id = lex_operator(s);                                        \
      if (!is_opid(id) || calc_op_priority(istring_cstr(id)) != pri) {  \
        stream_back(s, id);                                             \
        break;                                                          \
      }                                                                 \
      FMap args = flist();                                              \
      flist_push(args, next(s));                                        \
      flist_push(args, left);                                           \
      left = fcall(fident(id), args);                                   \
    }                                                                   \
    return left;                                                        \
  }

def_infix_parser(parse_infix1, parse_call, 1);
def_infix_parser(parse_infix4, parse_infix1, 4);
def_infix_parser(parse_infix5, parse_infix4, 5);
def_infix_parser(parse_infix7, parse_infix5, 7);
def_infix_parser(parse_infix12, parse_infix7, 12);
def_infix_parser(parse_infix15, parse_infix12, 15);

FMap parse(Stream* s) {
  skip_spaces(s);
  if (stream_get(s) == '\n' || stream_get(s) == ';') {
    stream_next(s);
    return nil_FMap();
  }
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

  id = lex_operator(s);
  if (search_parser_decl(id, &pdecl)) {
    if (pdecl.internalfn != NULL) {
      return (pdecl.internalfn)(s);
    } else {
      assert(false); // TODO:
    }
  }
  stream_back(s, id);
  
  return parse_infix15(s);
}
