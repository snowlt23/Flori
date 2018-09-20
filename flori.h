#ifndef _FLORI_H_
#define _FLORI_H_
%%adinclude "flori.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define error(...) { fprintf(stderr, __VA_ARGS__); exit(1); }

#define fwith(f, t) t ## Obj* f = (t ## Obj*)t ## _ptr(f)

typedef struct {
  char* buf;
  int pos;
  int len;
} Stream;

%%enum FExprKind {
  FEXPR_IDENT,
  FEXPR_INFIX,

  FEXPR_QUOTE,
  FEXPR_PREFIX,
  FEXPR_SYMBOL,

  FEXPR_INTLIT,
  FEXPR_FLOATLIT,
  FEXPR_STRLIT,

  FEXPR_SEQ,
  FEXPR_ARRAY,
  FEXPR_LIST,
  FEXPR_BLOCK
};

typedef struct {
  // IString filename;
  int line;
  int linepos;
  int pos;
  bool isinternal;
} Span;

%%template fstruct {
  typedef struct {
    int index;
  } %%1;
  %%1 alloc_%%1();
  %%2* %%1_ptr(%%1 t);
} {
  %%1 alloc_%%1() {
    return (%%1){linmem_alloc(sizeof(%%2))};
  }
  %%2* %%1_ptr(%%1 t) {
    return (%%2*)linmem_toptr(t.index);
  }
}

%%expand fstruct(FExpr, struct _FExprObj);
typedef struct _FExprObj {
  FExprKind kind;
  union {
    int intval;
  };
} FExprObj;

// linmem.c
void linmem_init(int size);
int linmem_alloc(int size);
void* linmem_toptr(int index);

// parser.c
Stream* new_stream(char* buf);
FExpr parse(Stream* s);

// codegen.c
void codegen(FExpr f);

#endif
