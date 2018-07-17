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
  TOKEN_ADD,
  TOKEN_SUB,
};

typedef struct {
  tokenkind kind;
  union {
  };
} token;

// vector.c
vector* new_vector_cap(int cap);

vector* new_vector();
void vector_extend(vector* v);
void* vector_get(vector* v, int index);
void vector_set(vector* v, int index, void* elem);
void vector_push(vector* v, void* elem);

// lexer.c
lexer* new_lexer(FILE* handle, char* filename);

#endif
