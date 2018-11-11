#include "flori.h"

DeclMap declmap;
FnDeclMap fndeclmap;

void semantic_init() {
  declmap = nil_DeclMap();
  fndeclmap = nil_FnDeclMap();
}

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
// decls
//

void add_decl(Decl decl) {
  declmap = new_DeclMap(decl, declmap);
}

bool search_decl(IString name, Decl* retdecl) {
  forlist (DeclMap, Decl, decl, declmap) {
    if (istring_eq(decl.name, name)) {
      *retdecl = decl;
      return true;
    }
  }
  return false;
}

void add_fndecl() {
}

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

bool is_setseq(FExpr f) {
  if (fe(f)->kind != FEXPR_SEQ) return false;
  if (IListFExpr_len(fe(f)->sons) < 3) return false;
  return cmp_ident(IListFExpr_value(fe(f)->sons), "set");
}

bool is_jitseq(FExpr f) {
  if (fe(f)->kind != FEXPR_SEQ) return false;
  if (IListFExpr_len(fe(f)->sons) < 3) return false;
  return cmp_ident(IListFExpr_value(fe(f)->sons), "jit");
}

bool is_Xseq(FExpr f) {
  if (fe(f)->kind != FEXPR_SEQ) return false;
  if (IListFExpr_len(fe(f)->sons) < 2) return false;
  return cmp_ident(IListFExpr_value(fe(f)->sons), "X");
}

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
  } else if (fe(f)->kind == FEXPR_IDENT) {
    Decl decl;
    if (search_decl(fe(f)->ident, &decl)) {
      fe(f)->kind = FEXPR_SYMBOL;
      fe(f)->sym = decl.sym;
    }
    // } else {
      // error("undeclared %s ident.", istring_cstr(fe(f)->ident));
    // }
  } else if (is_typeseq(f)) {
    FExpr t = IListFExpr_value(IListFExpr_next(fe(f)->sons));
    if (cmp_ident(t, "int")) {
      *fe(f) = *fe(new_ftypesym(new_ftype(FTYPE_INT)));
    } else {
      error("undeclared %s type", istring_cstr(FExpr_ptr(t)->ident));
    }
  } else if (is_setseq(f)) {
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr name = fnext(it);
    FExpr value = fnext(it);
    IString namestr = fe(name)->ident;
    fe(name)->kind = FEXPR_SYMBOL;
    fe(name)->sym = alloc_FSymbol();
    add_decl((Decl){namestr, fe(name)->sym});
    semantic_analysis(value);
  } else if (is_jitseq(f)) {
    // discard
  } else if (is_Xseq(f)) {
    // discard
  } else if (is_fnseq(f)) {
    fiter(it, fe(f)->sons);
    fnext(it);
    /* FExpr name = */ fnext(it);
    IListFExpr argsit = it;
    FExpr args = fnext(it);
    forlist(IListFExpr, FExpr, arg, fe(args)->sons) {
      fiter(argit, fe(arg)->sons);
      FExpr argname = fnext(argit);
      // FExpr argtyp = fnext(argit);
      fe(arg)->kind = FEXPR_SYMBOL;
      fe(arg)->sym = alloc_FSymbol();
      fp(FSymbol, fe(arg)->sym)->f = argname;
      add_decl((Decl){fe(argname)->ident, fe(arg)->sym});
    }

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
    forlist (IListFExpr, FExpr, son, fe(f)->sons) {
      semantic_analysis(son);
    }
  } else if (fe(f)->kind == FEXPR_LIST) {
    forlist (IListFExpr, FExpr, son, fe(f)->sons) {
      semantic_analysis(son);
    }
  } else if (fe(f)->kind == FEXPR_BLOCK) {
    forlist (IListFExpr, FExpr, son, fe(f)->sons) {
      semantic_analysis(son);
    }
  }
}
