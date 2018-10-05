#ifndef _FLORI_H_
#define _FLORI_H_
%%adinclude "flori.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define error(...) { fprintf(stderr, __VA_ARGS__); exit(1); }
#define check_next(l, ...) if (IListFExpr_isnil(l)) { error(__VA_ARGS__); }

#define fwith(f, t) t ## Obj* f = (t ## Obj*)t ## _ptr(f)

typedef struct {
  char* buf;
  int pos;
  int len;
} Stream;

typedef struct {
  int index;
} IString;

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

#define CONCAT(a, b) a ## b
#define flist_get_nextvalue(T, l) (IList ## T ## _isnil(l) ? (T){} : IList ## T ## _value(l))
#define forlist2(T, lv, e, l) IList ## T lv; T e; for (lv = l, e = flist_get_nextvalue(T, lv); !IList ## T ## _isnil(lv); lv = IList ## T ## _next(lv), e = flist_get_nextvalue(T, lv))
#define forlist1(T, ln, e, l) forlist2(T, CONCAT(_tmplst, ln), e, l)
#define forlist(T, e, l) forlist1(T, __LINE__, e, l)

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
  IList%%1 IList%%1_reverse(IList%%1 l);
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
  IList%%1 IList%%1_reverse(IList%%1 l) {
    IList%%1 ret = nil_IList%%1();
    forlist(%%1, e, l) {
      ret = new_IList%%1(e, ret);
    }
    return ret;
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
    IString ident;
    int intval;
    IListFExpr sons;
  };
} FExprObj;

typedef struct {
  IString key;
  int index;
} FnInfo;

typedef struct {
  IString key;
  FExpr body;
} JitInfo;

typedef struct {
  IString key;
  int offset;
} VarInfo;

%%expand ilist(FnInfo);
%%expand ilist(JitInfo);
%%expand ilist(VarInfo);

// linmem.c
bool linmem_need_extend();
void linmem_extend();
void linmem_init(int size);
int linmem_alloc(int size);
void* linmem_toptr(int index);

// jit.c
void jit_init(int size);
int jit_getidx();
int jit_alloc_write(uint8_t* buf, int n);
void* jit_toptr(int index);

// istring.c
IString new_istring(char* s);
char* istring_cstr(IString s);

// parser.c
bool cmp_ident(FExpr f, char* id);
bool stream_isend(Stream* s);
Stream* new_stream(char* buf);
FExpr parse(Stream* s);

// codegen.c
void codegen_init();
void codegen(FExpr f);
int call_main();

#endif
