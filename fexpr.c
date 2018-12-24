#include "flori.h"
#include <string.h>

//
// ast
//

FExpr new_fexpr(FExprKind kind) {
  FExpr f = alloc_FExpr();
  fe(f)->kind = kind;
  return f;
}

FExpr fident(char* id) {
  FExpr f = new_fexpr(FEXPR_IDENT);
  fe(f)->ident = new_istring(id);
  return f;
}

FExpr fop(char* id) {
  FExpr f = new_fexpr(FEXPR_OP);
  fe(f)->ident = new_istring(id);
  return f;
}

bool cmp_ident(FExpr f, char* id) {
  return strcmp(istring_cstr(fe(f)->ident), id) == 0;
}

FExpr new_fcontainer(FExprKind kind) {
  FExpr f = alloc_FExpr();
  fe(f)->kind = kind;
  fe(f)->sons = nil_IListFExpr();
  return f;
}

void push_son(FExpr f, FExpr son) {
  fe(f)->sons = new_IListFExpr(son, fe(f)->sons);
}

void reverse_sons(FExpr f) {
  fe(f)->sons = IListFExpr_reverse(fe(f)->sons);
}

FExpr copy_fexpr(FExpr f) {
  FExpr newf = alloc_FExpr();
  *fe(newf) = *fe(f);
  return newf;
}

char* fexpr_tostring(FExpr f) {
  char buf[1024*1024] = {};
  int bufpos = 0;
  switch (fe(f)->kind) {
  case FEXPR_IDENT:
    return istring_cstr(fe(f)->ident);
  case FEXPR_OP:
    return istring_cstr(fe(f)->ident);
  case FEXPR_SYMBOL:
    return istring_cstr(fp(FSymbol, fe(f)->sym)->name);
  case FEXPR_INTLIT:
    snprintf(buf, 1024*1024, "%d", fe(f)->intval);
    return strdup(buf);
  case FEXPR_FLOATLIT:
    snprintf(buf, 1024*1024, "%f", fe(f)->floatval);
    return strdup(buf);
  case FEXPR_STRLIT:
    snprintf(buf, 1024*1024, "\"%s\"", istring_cstr(fe(f)->strval));
    return strdup(buf);
  case FEXPR_SEQ: {
    if (IListFExpr_len(fe(f)->sons) == 0) {
      return "<noneseq>";
    }
    bufpos += snprintf(buf + bufpos, 1024*1024, "%s", fexpr_tostring(IListFExpr_value(fe(f)->sons)));
    forlist (IListFExpr, FExpr, son, IListFExpr_next(fe(f)->sons)) {
      bufpos += snprintf(buf + bufpos, 1024*1024, " %s", fexpr_tostring(son));
    }
    return strdup(buf);
  };
  case FEXPR_ARRAY: {
    if (IListFExpr_len(fe(f)->sons) == 0) {
      return "[]";
    }
    bufpos += snprintf(buf + bufpos, 1024*1024, "[%s", fexpr_tostring(IListFExpr_value(fe(f)->sons)));
    forlist (IListFExpr, FExpr, son, IListFExpr_next(fe(f)->sons)) {
      bufpos += snprintf(buf + bufpos, 1024*1024, ", %s", fexpr_tostring(son));
    }
    bufpos += snprintf(buf + bufpos, 1024*1024, "]");
    return strdup(buf);
  };
  case FEXPR_LIST: {
    if (IListFExpr_len(fe(f)->sons) == 0) {
      return "()";
    }
    bufpos += snprintf(buf + bufpos, 1024*1024, "(%s", fexpr_tostring(IListFExpr_value(fe(f)->sons)));
    forlist (IListFExpr, FExpr, son, IListFExpr_next(fe(f)->sons)) {
      bufpos += snprintf(buf + bufpos, 1024*1024, ", %s", fexpr_tostring(son));
    }
    bufpos += snprintf(buf + bufpos, 1024*1024, ")");
    return strdup(buf);
  };
  case FEXPR_BLOCK: {
    if (IListFExpr_len(fe(f)->sons) == 0) {
      return "{}";
    }
    bufpos += snprintf(buf + bufpos, 1024*1024, "{\n");
    forlist (IListFExpr, FExpr, son, fe(f)->sons) {
      bufpos += snprintf(buf + bufpos, 1024*1024, "  %s\n", fexpr_tostring(son));
    }
    bufpos += snprintf(buf + bufpos, 1024*1024, "}");
    return strdup(buf);
  };
  }
  assert(false);
}