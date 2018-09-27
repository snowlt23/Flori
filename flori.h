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

%%template ilist {
  typedef struct _IList%%1 {
    int index;
  } IList%%1;
  typedef struct {
    %%1 value;
    IList%%1 next;
  } IListObj%%1;
  IList%%1 nil_IList%%1();
  IListObj%%1* IList%%1_ptr(IList%%1 l);
  IList%%1 new_IList%%1(%%1 value, IList%%1 next);
  %%1 IList%%1_value(IList%%1 l);
  IList%%1 IList%%1_next(IList%%1 l);
  bool IList%%1_isnil(IList%%1 l);
  int IList%%1_len(IList%%1 l);
} {
  IList%%1 nil_IList%%1() {
    return (IList%%1){-1};
  }
  IListObj%%1* IList%%1_ptr(IList%%1 l) {
    return (IListObj%%1*)linmem_toptr(l.index);
  }
  IList%%1 new_IList%%1(%%1 value, IList%%1 next) {
    IList%%1 l = (IList%%1){linmem_alloc(sizeof(IListObj%%1))};
    IList%%1_ptr(l)->value= value;
    IList%%1_ptr(l)->next = next;
    return l;
  }
  %%1 IList%%1_value(IList%%1 l) {
    return IList%%1_ptr(l)->value;
  }
  IList%%1 IList%%1_next(IList%%1 l) {
    return IList%%1_ptr(l)->next;
  }
  bool IList%%1_isnil(IList%%1 l) {
    return l.index == -1;
  }
  int IList%%1_len(IList%%1 l) {
    int len = 0;
    IList%%1 curr = l;
    for (;;) {
      if (IList%%1_isnil(curr)) break;
      curr = IList%%1_next(curr);
      len++;
    }
    return len;
  }
}

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
%%expand ilist(FExpr);
typedef struct _FExprObj {
  FExprKind kind;
  union {
    char* ident;
    int intval;
    IListFExpr sons;
  };
} FExprObj;

// linmem.c
void linmem_init(int size);
int linmem_alloc(int size);
void* linmem_toptr(int index);

// jit.c
void jit_init(int size);
int jit_getidx();
int jit_alloc_write(uint8_t* buf, int n);
void* jit_toptr(int index);

// parser.c
Stream* new_stream(char* buf);
FExpr parse(Stream* s);

// codegen.c
void codegen(FExpr f);

#endif
