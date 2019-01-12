#ifndef _FLORI_H_
#define _FLORI_H_
%%adinclude "flori.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define debug(...) {fprintf(stderr, __VA_ARGS__); fprintf(stderr, "\n");}
#define error(...) { fprintf(stderr, __VA_ARGS__); exit(1); }
#define check_next(l, ...) if (IListFExpr_isnil(l)) { error(__VA_ARGS__); }
#define isfnil(l) IListFExpr_isnil(l)

#define fp(t, f) (assert(0 <= f.index && f.index < linmem_getidx()), t ## _ptr(f))
#define fe(f) fp(FExpr, f)

#define fiter(itr, f) IListFExpr itr = f
#define fcurr(itr) IListFExpr_value(itr)
#define fnext(itr) fnext_impl(&itr)

#define ppcat1(a, b) a ## b
#define ppcat(a, b) ppcat1(a, b)
#define fcont1(sons, sonstmp, v, kind, ...)      \
  FExpr sons[] = {__VA_ARGS__}; \
  FExpr v = new_fcontainer(kind); \
  for (int sonstmp=0; sonstmp<sizeof(sons)/sizeof(FExpr); sonstmp++) { \
    push_son(v, sons[sonstmp]);                             \
  } \
  reverse_sons(v);
#define fcont(v, kind, ...) fcont1(ppcat(_sons, __LINE__), ppcat(_sonstmp, __LINE__), v, kind, __VA_ARGS__)
#define fseq(v, ...) fcont(v, FEXPR_SEQ, __VA_ARGS__)
#define flist(v, ...) fcont(v, FEXPR_LIST, __VA_ARGS__)
#define fblock(v, ...) fcont(v, FEXPR_BLOCK, __VA_ARGS__)

#define with_reloc(addr, body) \
  { \
    uint8_t* __tmpaddr = data_memptr(); \
    reloc_execute_addr(addr); \
    body \
    reloc_execute_addr(__tmpaddr); \
  }

typedef struct {
  void** data;
  int cap;
  int len;
} Vector;

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
  %%2 %%1_last(%%1 l);
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
  %%2 %%1_last(%%1 l) {
    %%1 curr = l;
    for (;;) {
      if (%%1_isnil(%%1_next(curr))) break;
      curr = %%1_next(curr);
    }
    return %%1_value(curr);
  }
}

%%template fstruct {
  typedef struct {
    size_t index;
  } %%1;
  %%1 alloc_%%1();
  %%2* %%1_ptr(%%1 t);
  %%1 nil_%%1();
  bool %%1_isnil(%%1 t);
} {
  %%1 alloc_%%1() {
    return (%%1){linmem_alloc(sizeof(%%2))};
  }
  %%1 nil_%%1() {
    return (%%1){-1};
  }
  %%2* %%1_ptr(%%1 t) {
    assert(!%%1_isnil(t));
    return (%%2*)linmem_toptr(t.index);
  }
  bool %%1_isnil(%%1 t) {
    return t.index == -1;
  }
}

// %%1=V %%2=T
%%template vector {
  typedef struct {
    %%2* data;
    int cap;
    int len;
  } %%1;
  %%1* newcap_%%1(int cap);
  %%1* new_%%1();
  void %%1_extend(%%1* v);
  %%2 %%1_get(%%1* v, int index);
  void %%1_set(%%1* v, int index, %%2 elem);
  void %%1_push(%%1* v, %%2 elem);
} {
  %%1* newcap_%%1(int cap) {
    %%1* v = malloc(sizeof(%%1));
    v->data = malloc(cap*sizeof(%%2));
    v->cap = cap;
    v->len = 0;
    return v;
  }
  %%1* new_%%1() {
    return newcap_%%1(256);
  }
  void %%1_extend(%%1* v) {
    if (v->cap < v->len+1) {
      v->data = realloc(v->data, v->cap*2*sizeof(%%2));
      v->cap *= 2;
    }
  }
  %%2 %%1_get(%%1* v, int index) {
    return v->data[index];
  }
  void %%1_set(%%1* v, int index, %%2 elem) {
    v->data[index] = elem;
  }
  void %%1_push(%%1* v, %%2 elem) {
    %%1_extend(v);
    %%1_set(v, v->len++, elem);
  }
}

%%expand fstruct(FExpr, struct _FExprObj);
%%expand ilist(IListFExpr, FExpr);

%%expand fstruct(FSymbol, struct _FSymbolObj);
typedef struct _FSymbolObj {
  FExpr f;
  bool isjit;
  bool ismacro;
  bool isprim;
  IString name;
  bool istoplevel;
  bool isinternal;
  union {
    int vardataidx;
    int varoffset;
    struct {
      int fnidx;
      int stacksize;
      int rewrited;
    };
    int size;
    void* internalptr;
  };
} FSymbolObj;

typedef enum {
  FTYPE_PRIM,
  FTYPE_PTR,
  FTYPE_SYM,
} FTypeKind;

%%expand fstruct(FType, struct _FTypeObj);
typedef struct _FTypeObj {
  FTypeKind kind;
  FSymbol sym;
  FType ptrof;
} FTypeObj;
%%expand ilist(IListFType, FType);
%%expand vector(FTypeVec, FType);

typedef struct _FExprObj {
  FExprKind kind;
  FType typ;
  bool istyp;
  FExpr srcf;
  bool evaluated;
  bool codegened;
  bool istoplevel;
  int priority;
  union {
    IString ident;
    FSymbol sym;
    FType typsym;
    int intval;
    float floatval;
    IString strval;
    IListFExpr sons;
  };
} FExprObj;

typedef struct {
  IString name;
  FSymbol sym;
  FType typ;
} Decl;

typedef struct {
  IString name;
  IListFType argtypes;
  FType returntype;
  bool isjit;
  FSymbol sym;
  bool isinternal;
  void (*internalfn)(FExpr);
} FnDecl;

%%expand ilist(DeclMap, Decl);
%%expand ilist(IListFnDecl, FnDecl);
typedef struct {
  IString name;
  IListFnDecl decls;
} FnDeclGroupObj;
%%expand fstruct(FnDeclGroup, FnDeclGroupObj);
%%expand ilist(FnDeclMap, FnDeclGroup);

typedef struct {
  IString name;
  bool issyntax;
  void (*semanticfn)(FExpr);
  void (*codegenfn)(FExpr);
  void (*lvaluegenfn)(FExpr);
} InternalDecl;
%%expand ilist(InternalDeclMap, InternalDecl);

typedef struct {
  size_t jitidx;
  size_t dataidx;
} RelocInfo;
%%expand ilist(RelocList, RelocInfo);

// linmem.c
bool linmem_need_extend();
void linmem_extend();
void linmem_init(int size);
int linmem_alloc(int size);
void* linmem_toptr(int index);
int linmem_getidx();

// jit.c
void jit_init(int size);
bool jit_need_extend(int size);
void jit_extend(int size);
int jit_getidx();
int jit_alloc_write(uint8_t* buf, int n);
void* jit_toptr(int index);
void jit_write_to_file(char* filename);
uint8_t* jit_codeptr();
size_t jit_codesize();

// data.c
void data_init(size_t size);
bool data_need_extend(size_t size);
void data_extend(size_t size);
size_t data_alloc(size_t size);
void* data_toptr(size_t idx);
size_t data_cstring(char* s);
size_t data_int(size_t x);
uint8_t* data_memptr();
void data_set_memptr(uint8_t* addr);
size_t data_memsize();

// reloc.c
void fixup_lendian32(uint8_t* addr, int x);
void fixup_lendian64(uint8_t* addr, size_t x);
void reloc_init();
void reloc_add_info(size_t jitidx, size_t dataidx);
void reloc_execute();
void reloc_execute_addr(uint8_t* addr);

// istring.c
IString new_istring(char* s);
char* istring_cstr(IString s);
bool istring_eq(IString a, IString b);

// fexpr.c
FExpr new_fexpr(FExprKind kind);
FExpr fident(char* id);
FExpr fop(char* id);
bool cmp_ident(FExpr f, char* id);
FExpr new_fcontainer(FExprKind kind);
void push_son(FExpr f, FExpr son);
void reverse_sons(FExpr f);
FExpr copy_fexpr(FExpr f);
FExpr deepcopy_fexpr(FExpr f);
char* fexpr_tostring(FExpr f);

// parser.c
bool stream_isend(Stream* s);
Stream* new_stream(char* buf);
FExpr parse_element(Stream* s);
FExpr parse(Stream* s);

// boot.c
bool search_fndecl(IString name, FTypeVec* argtypes, FnDecl* fndecl);
void boot_semantic(FExpr f);
void boot_codegen(FExpr f);
void boot_eval_toplevel(FExpr f);
void boot_init();
void boot_def_internals();
int boot_call_main();

// elfgen.c
void write_elf_executable(FILE* fp, uint8_t* codeptr, size_t codesize, uint8_t* dataptr, size_t datasize, size_t entryoffset);

#endif
