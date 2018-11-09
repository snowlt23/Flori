#include "flori.h"

DeclMap declmap;
FnDeclMap fndeclmap;

FExpr fnext_impl(IListFExpr* il) {
  check_next(*il, "require more token");
  FExpr ret = IListFExpr_value(*il);
  *il = IListFExpr_next(*il);
  return ret;
}

FType new_ftype(FTypeKind kind) {
  FType ft = alloc_FType();
  fp(FType, ft)->kind = kind;
  return ft;
}

FExpr new_ftypesym(FType t) {
  FExpr f = new_fexpr(FEXPR_SYMBOL);
  fe(f)->typsym = t;
  return f;
}

//
// search
//



//
// compare
//

bool is_typeseq(FExpr f) {
  if (fe(f)->kind != FEXPR_SEQ) return false;
  if (IListFExpr_len(fe(f)->sons) < 2) return false;
  return cmp_ident(IListFExpr_value(fe(f)->sons), "type");
}

// bool is_fncall() {
// }

bool is_fnseq(FExpr f) {
  if (fe(f)->kind != FEXPR_SEQ) return false;
  if (IListFExpr_len(fe(f)->sons) < 1) return false;
  return cmp_ident(IListFExpr_value(fe(f)->sons), "fn");
}

//
// semantic
//

void semantic_analysis(FExpr f) {
  if (fe(f)->kind == FEXPR_INTLIT) {
    fe(f)->typ = new_ftype(FTYPE_INT);
  } else if (is_typeseq(f)) {
    FExpr t = IListFExpr_value(IListFExpr_next(fe(f)->sons));
    if (cmp_ident(t, "int")) {
      *fe(f) = *fe(new_ftypesym(new_ftype(FTYPE_INT)));
    } else {
      error("undeclared %s type", istring_cstr(FExpr_ptr(t)->ident));
    }
  } else if (is_fnseq(f)) {
    fiter(it, fe(f)->sons);
    fnext(it);
    /* FExpr name = */ fnext(it);
    IListFExpr argsit = it;
    /* FExpr args = */ fnext(it);
    FExpr rettyp;
    FExpr body;
    if (FExpr_ptr(fcurr(it))->kind == FEXPR_BLOCK) {
      IListFExpr_ptr(argsit)->next = new_IListFExpr(new_ftypesym(new_ftype(FTYPE_VOID)), it);
      it = argsit;
      fnext(it);
      rettyp = fnext(it);
      body = fnext(it);
      semantic_analysis(body);
    } else {
      rettyp = fnext(it);
      body = fnext(it);
      semantic_analysis(rettyp);
      semantic_analysis(body);
    }
  } else if (fe(f)->kind == FEXPR_SEQ) {
    forlist (FExpr, son, fe(f)->sons) {
      semantic_analysis(son);
    }
  }
}
