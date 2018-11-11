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

#define fp(t, f) t ## _ptr(f)
#define fe(f) fp(FExpr, f)

#define fiter(itr, f) IListFExpr itr = f
#define fcurr(itr) IListFExpr_value(itr)
#define fnext(itr) fnext_impl(&itr)

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
  FEXPR_OP,

  FEXPR_QUOTE,
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
#define flist_get_nextvalue(L, T, l) (L ## _isnil(l) ? (T){} : L ## _value(l))
#define forlist2(L, T, lv, e, l) L lv; T e; for (lv = l, e = flist_get_nextvalue(L, T, lv); !L ## _isnil(lv); lv = L ## _next(lv), e = flist_get_nextvalue(L, T, lv))
#define forlist1(L, T, ln, e, l) forlist2(L, T, CONCAT(_tmplst, ln), e, l)
#define forlist(L, T, e, l) forlist1(L, T, __LINE__, e, l)

%%template ilist {
  typedef struct _%%1 {
    int index;
  } %%1;
  typedef struct {
    %%2 value;
    %%1 next;
  } %%1Obj;
  %%1 nil_%%1();
  %%1Obj* %%1_ptr(%%1 l);
  %%1 new_%%1(%%2 value, %%1 next);
  %%2 %%1_value(%%1 l);
  %%1 %%1_next(%%1 l);
  bool %%1_isnil(%%1 l);
  int %%1_len(%%1 l);
  %%1 %%1_reverse(%%1 l);
} {
  %%1 nil_%%1() {
    return (%%1){-1};
  }
  %%1Obj* %%1_ptr(%%1 l) {
    return (%%1Obj*)linmem_toptr(l.index);
  }
  %%1 new_%%1(%%2 value, %%1 next) {
    %%1 l = (%%1){linmem_alloc(sizeof(%%1Obj))};
    %%1_ptr(l)->value= value;
    %%1_ptr(l)->next = next;
    return l;
  }
  %%2 %%1_value(%%1 l) {
    return %%1_ptr(l)->value;
  }
  %%1 %%1_next(%%1 l) {
    return %%1_ptr(l)->next;
  }
  bool %%1_isnil(%%1 l) {
    return l.index == -1;
  }
  int %%1_len(%%1 l) {
    int len = 0;
    %%1 curr = l;
    for (;;) {
      if (%%1_isnil(curr)) break;
      curr = %%1_next(curr);
      len++;
    }
    return len;
  }
  %%1 %%1_reverse(%%1 l) {
    %%1 ret = nil_%%1();
    forlist(%%1, %%2, e, l) {
      ret = new_%%1(e, ret);
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

typedef enum {
  FTYPE_VOID,
  FTYPE_INT,
} FTypeKind;

%%expand fstruct(FType, struct _FTypeObj);
typedef struct _FTypeObj {
  FTypeKind kind;
} FTypeObj;
%%expand ilist(IListFType, FType);

%%expand fstruct(FExpr, struct _FExprObj);
%%expand ilist(IListFExpr, FExpr);

%%expand fstruct(FSymbol, struct _FSymbolObj);
typedef struct _FSymbolObj {
  FExpr f;
  int varoffset;
} FSymbolObj;

typedef struct _FExprObj {
  FExprKind kind;
  FType typ;
  union {
    IString ident;
    FSymbol sym;
    FType typsym;
    int intval;
    IListFExpr sons;
  };
} FExprObj;

typedef struct {
  IString name;
  FSymbol sym;
} Decl;

typedef struct {
  IString name;
  IListFType argtypes;
  FSymbol sym;
} FnDecl;

%%expand ilist(DeclMap, Decl);
%%expand ilist(FnDeclGroup, FnDecl);
%%expand ilist(FnDeclMap, FnDeclGroup);

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

%%expand ilist(IListFnInfo, FnInfo);
%%expand ilist(IListJitInfo, JitInfo);
%%expand ilist(IListVarInfo, VarInfo);

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
void jit_write_to_file(char* filename);

// istring.c
IString new_istring(char* s);
char* istring_cstr(IString s);
bool istring_eq(IString a, IString b);

// parser.c
FExpr new_fexpr(FExprKind kind);
bool cmp_ident(FExpr f, char* id);
bool stream_isend(Stream* s);
Stream* new_stream(char* buf);
FExpr parse(Stream* s);

// semantic.c
void semantic_init();
FExpr fnext_impl(IListFExpr* il);
void semantic_analysis(FExpr f);

// codegen.c
void codegen_init();
void codegen(FExpr f);
int call_main();

#endif
