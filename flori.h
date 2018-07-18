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
  int index;
  int len;
} istring;

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
  TOKEN_LPAREN,
  TOKEN_RPAREN,
  TOKEN_INDENT,
  TOKEN_NEWLINE,
  TOKEN_LBLOCK,
  TOKEN_RBLOCK,
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

typedef struct {
  int index;
  int len;
} iarray_fexpr;

%%fstruct fexpr fexpr_obj {
  fexprkind kind;
  union {
    struct {
      fexpr call;
      iarray_fexpr arguments;
    };
    istring ident;
    int intval;
  };
};

// string.c
char* string_copy(char* s);

// vector.c
vector* new_vector_cap(int cap);
vector* new_vector();
void vector_extend(vector* v);
void* vector_get(vector* v, int index);
void vector_set(vector* v, int index, void* elem);
void vector_push(vector* v, void* elem);

// linmem.c
void init_linmem_cap(int size);
void init_linmem();
int linmem_alloc(int size);
void* linmem_toptr(int pos);

// istring
istring new_istring(char* s);
char* istring_cstr(istring is);

// iarray_fexpr
iarray_fexpr new_iarray_fexpr(int len);
void iarray_fexpr_set(iarray_fexpr ia, int index, fexpr f);
fexpr iarray_fexpr_get(iarray_fexpr ia, int index);

// token
char* tokenkind_tostring(tokenkind kind);
int op_priority(token* t);

// lexer.c
lexer* new_lexer(FILE* handle, char* filename);
tokenstream* lex(lexer* lx);
tokenstream* offside_rule(tokenstream* ts);
token* get_token(tokenstream* ts);
token* next_token(tokenstream* ts);

// fexpr
char* fexprkind_tostring(fexprkind kind);

// parser.c
fexpr parse_fexpr(tokenstream* ts);

// codegen.c
void init_codegen(FILE* handle);
void codegen(fexpr f);
void codegen_main(fexpr f);

#endif
