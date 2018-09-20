#ifndef _FLORI_H_
#define _FLORI_H_
%%adinclude "flori.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

typedef enum {
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
} FExprKind;

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
  // union {
  //   struct {
  //     IString idname;
  //     int priority;
  //     bool isleft;
  //   };
  //   struct {
  //     FExpr quoted;
  //   };
  //   struct {
  //     Symbol symbol;
  //   };
  //   int intval;
  //   float floatval;
  //   IString strval;
  //   IList_FExpr sons;
  // };
} FExprObj;

// linmem.c
int linmem_alloc(int size);
void* linmem_toptr(int index);

#endif
