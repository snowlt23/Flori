#ifndef _FLORI_H_
#define _FLORI_H_

#include <stdio.h>
#include <stdbool.h>

typedef struct {
  void** data;
  int cap;
  int len;
} vector;

typedef struct {
  char* filename;
  int line;
  int column;
} span;

typedef struct {
  FILE* handle;
  char* filename;
  int line;
  int column;
} lexer;

%%enum tokenkind {
  TOKEN_OP,
  TOKEN_IDENT,
  TOKEN_INTLIT,
};

typedef struct {
  tokenkind kind;
  union {
    char* ident;
    int intval;
  };
} token;

typedef struct {
  vector* tokens;
  int position;
} tokenstream;

%%enum fexprkind {
  FEXPR_CALL,
  FEXPR_INFIX,
  FEXPR_IDENT,
  FEXPR_INTLIT,
};

typedef struct _fexpr {
  fexprkind kind;
  union {
    struct {
      struct _fexpr* call;
      vector* arguments;
    };
    char* ident;
    int intval;
  };
} fexpr;

// string.c
char* string_copy(char* s);

// vector.c
vector* new_vector_cap(int cap);
vector* new_vector();
void vector_extend(vector* v);
void* vector_get(vector* v, int index);
void vector_set(vector* v, int index, void* elem);
void vector_push(vector* v, void* elem);

// token
char* tokenkind_tostring(tokenkind kind);
int op_priority(token* t);

// lexer.c
lexer* new_lexer(FILE* handle, char* filename);
tokenstream* lex(lexer* lx);
token* get_token(tokenstream* ts);
token* next_token(tokenstream* ts);

// fexpr
char* fexprkind_tostring(fexprkind kind);

// parser.c
fexpr* parse_fexpr(tokenstream* ts);

#endif
