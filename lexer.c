#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include "flori.h"

//
// token
//

token* new_token(tokenkind kind) {
  token* t = malloc(sizeof(token));
  t->kind = kind;
  return t;
}

token* new_token_intlit(int x) {
  token* t = new_token(TOKEN_INTLIT);
  t->intval = x;
  return t;
}

token* new_token_ident(char* id) {
  token* t = new_token(TOKEN_IDENT);
  t->ident = id;
  return t;
}

token* new_token_op(char* id) {
  token* t = new_token(TOKEN_OP);
  t->ident = id;
  return t;
}

int op_priority(token* t) {
  assert(t->kind == TOKEN_OP);
  int slen = strlen(t->ident);
  assert(slen >= 1);
  if (t->ident[0] == '+' || t->ident[0] == '-') {
    return 5;
  } else {
    assert(false);
    return 16;
  }
}

//
// tokenstream
//

tokenstream* new_tokenstream(vector* tokens) {
  tokenstream* ts = malloc(sizeof(tokenstream));
  ts->tokens = tokens;
  ts->position = 0;
  return ts;
}

token* get_token(tokenstream* ts) {
  if (ts->tokens->len <= ts->position) return NULL;
  return (token*)vector_get(ts->tokens, ts->position);
}

token* next_token(tokenstream* ts) {
  token* t = get_token(ts);
  ts->position++;
  return t;
}

//
// lexer context
//

#define lexer_error(lx, ...) {fprintf(stderr, "%s(%d:%d) ", lx->filename, lx->line, lx->column); fprintf(stderr, __VA_ARGS__); exit(1);}

lexer* new_lexer(FILE* handle, char* filename) {
  lexer* lx = malloc(sizeof(lexer));
  lx->handle = handle;
  lx->filename = filename;
  lx->line = 1;
  lx->column = 1;
  return lx;
}

char lexer_getc(lexer* lx) {
  lx->column++;;
  return getc(lx->handle);
}

void lexer_ungetc(lexer* lx, char c) {
  lx->column--;
  ungetc(c, lx->handle);
}

//
// lexer
//

bool isoperator(char c) {
  return c == '!' || c == '|' || c == '%' || c == '&' || c == '+' || c == '-' || c == '*' || c == '/' || c == '.' || c == ':' || c == '=' || c == '<' || c == '>';
}

bool isidentfirst(char c) {
  return isalpha(c) || c == '_';
}

bool isidentrest(char c) {
  return isidentfirst(c) || isdigit(c);
}

tokenstream* lex(lexer* lx) {
  vector* tokens = new_vector();
  for (;;) {
    char c = lexer_getc(lx);
    if (c == EOF) break;
    if (isdigit(c)) {
      char digitbuf[256] = {};
      digitbuf[0] = c;
      for (int i=1; ; i++) {
        assert(i < 256);
        c = lexer_getc(lx);
        if (c == EOF || !isdigit(c)) {
          lexer_ungetc(lx, c);
          break;
        }
        digitbuf[i] = c;
      }
      vector_push(tokens, (void*)new_token_intlit(atoi(digitbuf)));
    } else if (isoperator(c)) {
      char opbuf[256] = {};
      opbuf[0] = c;
      for (int i=1; ; i++) {
        assert(i < 256);
        c = lexer_getc(lx);
        if (c == EOF || !isoperator(c)) {
          lexer_ungetc(lx, c);
          break;
        }
        opbuf[i] = c;
      }
      vector_push(tokens, (void*)new_token_op(string_copy(opbuf)));
    } else if (isidentfirst(c)) {
      char idbuf[256] = {};
      idbuf[0] = c;
      for (int i=1; ; i++) {
        assert(i < 256);
        c = lexer_getc(lx);
        if (c == EOF || !isidentrest(c)) {
          lexer_ungetc(lx, c);
          break;
        }
        idbuf[i] = c;
      }
      vector_push(tokens, (void*)new_token_ident(string_copy(idbuf)));
    } else if (c == ' ') {
      c = lexer_getc(lx);
      if (c == ' ') {
        vector_push(tokens, (void*)new_token(TOKEN_INDENT));
      } else {
        lexer_ungetc(lx, c);
      }
    } else if (c == '\n') {
      vector_push(tokens, (void*)new_token(TOKEN_NEWLINE));
      lx->line++;
      lx->column = 1;
    } else {
      lexer_error(lx, "unexpected token %c", c);
    }
  }
  return new_tokenstream(tokens);
}

tokenstream* offside_rule(tokenstream* ts) {
  vector* v = new_vector();

  int currindent = 0;
  bool isnewline = false;
  for (;;) {
    token* t = next_token(ts);
    if (t == NULL) break;
    if (t->kind == TOKEN_NEWLINE) {
      isnewline = true;
    } else if (t->kind == TOKEN_INDENT && isnewline) {
      int indent = 1;
      for (;;) {
        token* nt = get_token(ts);
        if (nt->kind != TOKEN_INDENT) break;
        next_token(ts);
        indent++;
      }
      if (indent > currindent) {
        vector_push(v, new_token(TOKEN_LBLOCK));
      } else if (indent < currindent) {
        for (int i=0; i<currindent - indent; i++) {
          vector_push(v, new_token(TOKEN_RBLOCK));
        }
      }
      isnewline = false;
      currindent = indent;
    } else if (t->kind == TOKEN_INDENT) {
    } else {
      vector_push(v, (void*)t);
    }
  }
  for (int i=0; i<currindent; i++) {
    vector_push(v, new_token(TOKEN_RBLOCK));
  }

  return new_tokenstream(v);
}

