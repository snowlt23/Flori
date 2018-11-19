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

bool ftype_eq(FType a, FType b) {
  return fp(FType, a)->kind == fp(FType, b)->kind;
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

//
// fn decls
//

void add_fndecl(FnDecl decl) {
  forlist (FnDeclMap, FnDeclGroup, fngroup, fndeclmap) {
    if (istring_eq(fp(FnDeclGroup, fngroup)->name, decl.name)) {
      fp(FnDeclGroup, fngroup)->decls = new_IListFnDecl(decl, fp(FnDeclGroup, fngroup)->decls);
      return;
    }
  }
  // if unregistered function
  FnDeclGroup newgroup = alloc_FnDeclGroup();
  fp(FnDeclGroup, newgroup)->name = decl.name;
  fp(FnDeclGroup, newgroup)->decls = new_IListFnDecl(decl, nil_IListFnDecl());
  fndeclmap = new_FnDeclMap(newgroup, fndeclmap);
}

bool match_overload(IListFType fntypes, FTypeVec* argtypes) {
  IListFType curr = fntypes;
  for (int i=0; i<argtypes->len; i++) {
    FType fnt = IListFType_value(curr);
    FType argt = FTypeVec_get(argtypes, i);
    if (!ftype_eq(fnt, argt)) {
      return false;
    }
    curr = IListFType_next(curr);
  }
  return true;
}

bool search_fndecl_from_group(FnDeclGroup fngroup, IString name, FTypeVec* argtypes, FnDecl* retfndecl) {
  forlist (IListFnDecl, FnDecl, fndecl, fp(FnDeclGroup, fngroup)->decls) {
    if (match_overload(fndecl.argtypes, argtypes)) {
      *retfndecl = fndecl;
      return true;
    }
  }
  return false;
}

bool search_fndecl(IString name, FTypeVec* argtypes, FnDecl* retfndecl) {
  forlist (FnDeclMap, FnDeclGroup, fngroup, fndeclmap) {
    if (istring_eq(fp(FnDeclGroup, fngroup)->name, name)) {
      if (search_fndecl_from_group(fngroup, name, argtypes, retfndecl)) {
        return true;
      }
    }
  }
  return false;
}

//
// compare
//

bool is_typeseq(FExpr f) {
  if (fe(f)->kind != FEXPR_SEQ) return false;
  if (IListFExpr_len(fe(f)->sons) < 2) return false;
  return cmp_ident(IListFExpr_value(fe(f)->sons), "type");
}

bool is_fncall(FExpr f) {
  if (fe(f)->kind != FEXPR_SEQ) return false;
  if (IListFExpr_len(fe(f)->sons) < 1) return false;
  return true;
}

bool is_infixcall(FExpr f) {
  if (fe(f)->kind != FEXPR_SEQ) return false;
  if (IListFExpr_len(fe(f)->sons) < 3) return false;
  fiter(it, fe(f)->sons);
  fnext(it);
  FExpr second = fnext(it);
  if (fe(second)->kind == FEXPR_OP) {
    return true;
  } else {
    return false;
  }
}

FExpr split_infixseq(FExpr f) {
  fiter(it, fe(f)->sons);
  FExpr first = fnext(it);
  FExpr second = fnext(it);
  FExpr right;
  if (IListFExpr_len(it) == 1) {
    right = fnext(it);
  } else {
    right = new_fexpr(FEXPR_SEQ);
    fe(right)->sons = it;
  }
  FExpr newf = new_fcontainer(FEXPR_SEQ);
  push_son(newf, second);
  push_son(newf, first);
  push_son(newf, right);
  reverse_sons(newf);
  return newf;
}

bool is_setseq(FExpr f) {
  if (fe(f)->kind != FEXPR_SEQ) return false;
  if (IListFExpr_len(fe(f)->sons) < 3) return false;
  return cmp_ident(IListFExpr_value(fe(f)->sons), "set");
}

bool is_jitseq(FExpr f) {
  if (fe(f)->kind != FEXPR_SEQ) return false;
  if (IListFExpr_len(fe(f)->sons) < 5) return false;
  return cmp_ident(IListFExpr_value(fe(f)->sons), "jit");
}

bool is_Xseq(FExpr f) {
  if (fe(f)->kind != FEXPR_SEQ) return false;
  if (IListFExpr_len(fe(f)->sons) < 2) return false;
  return cmp_ident(IListFExpr_value(fe(f)->sons), "X");
}

bool is_ifseq(FExpr f) {
  if (fe(f)->kind != FEXPR_SEQ) return false;
  if (IListFExpr_len(fe(f)->sons) < 3) return false;
  return cmp_ident(IListFExpr_value(fe(f)->sons), "if");
}

bool is_fnseq(FExpr f) {
  if (fe(f)->kind != FEXPR_SEQ) return false;
  if (IListFExpr_len(fe(f)->sons) < 1) return false;
  return cmp_ident(IListFExpr_value(fe(f)->sons), "fn");
}

bool is_structseq(FExpr f) {
  if (fe(f)->kind != FEXPR_SEQ) return false;
  if (IListFExpr_len(fe(f)->sons) < 1) return false;
  return cmp_ident(IListFExpr_value(fe(f)->sons), "struct");
}

//
// utils
//

bool search_field(FExpr body, IString name, FExpr* retf) {
  forlist (IListFExpr, FExpr, field, fe(body)->sons) {
    fiter(fieldit, fe(field)->sons);
    FExpr fieldsym = fnext(fieldit);
    if (cmp_ident(fp(FSymbol, fe(fieldsym)->sym)->f, istring_cstr(name))) {
      *retf = field;
      return true;
    }
  }
  return false;
}

//
// semantic
//

void semantic_analysis(FExpr f) {
  if (fe(f)->kind == FEXPR_INTLIT) {
    fe(f)->typ = new_ftype(FTYPE_INT);
  } else if (fe(f)->kind == FEXPR_IDENT) {
    Decl decl;
    FnDecl fndecl;
    if (search_decl(fe(f)->ident, &decl)) {
      fe(f)->kind = FEXPR_SYMBOL;
      fe(f)->sym = decl.sym;
      fe(f)->typ = decl.typ;
    } else if (search_fndecl(fe(f)->ident, new_FTypeVec(), &fndecl)) {
      FExpr newf = new_fcontainer(FEXPR_SEQ);
      FExpr idf = alloc_FExpr();
      *fe(idf) = *fe(f);
      push_son(newf, idf);
      semantic_analysis(newf);
      *fe(f) = *fe(newf);
    } else {
      error("undeclared %s ident.", istring_cstr(fe(f)->ident));
    }
  } else if (is_typeseq(f)) {
    FExpr t = IListFExpr_value(IListFExpr_next(fe(f)->sons));
    Decl decl;
    if (search_decl(fe(t)->ident, &decl)) {
      FType ft = new_ftype(FTYPE_SYM);
      fp(FType, ft)->sym = decl.sym;
      *fe(f) = *fe(new_ftypesym(ft));
    } else if (cmp_ident(t, "int")) {
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
    semantic_analysis(value);
    add_decl((Decl){namestr, fe(name)->sym, fe(value)->typ});
  } else if (is_jitseq(f)) {
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr name = fnext(it);
    FExpr args = fnext(it);
    FExpr rettyp = fnext(it);
    FExpr body = fnext(it);
    IListFType argtypes = nil_IListFType();
    forlist(IListFExpr, FExpr, arg, fe(args)->sons) {
      semantic_analysis(arg);
      argtypes = new_IListFType(fe(arg)->typsym, argtypes);
    }
    semantic_analysis(rettyp);
    IString nameid = fe(name)->ident;
    fe(name)->kind = FEXPR_SYMBOL;
    fe(name)->sym = alloc_FSymbol();
    fp(FSymbol, fe(name)->sym)->f = body;
    fp(FSymbol, fe(name)->sym)->isjit = true;
    add_fndecl((FnDecl){nameid, argtypes, fe(rettyp)->typsym, true, fe(name)->sym});
  } else if (is_Xseq(f)) {
    // discard
  } else if (is_ifseq(f)) {
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr cond = fnext(it);
    FExpr body = fnext(it);
    semantic_analysis(cond);
    semantic_analysis(body);
    if (!isfnil(it)) {
      fnext(it);
      FExpr elsebody = fnext(it);
      semantic_analysis(elsebody);
    }
    fe(f)->typ = new_ftype(FTYPE_INT); // FIXME: if expression type
  } else if (is_structseq(f)) {
    // TODO:
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr name = fnext(it);
    IString namestr = fe(name)->ident;
    FExpr body = fnext(it);
    fe(name)->kind = FEXPR_SYMBOL;
    fe(name)->sym = alloc_FSymbol();
    add_decl((Decl){namestr, fe(name)->sym});
    forlist (IListFExpr, FExpr, field, fe(body)->sons) {
      if (fe(field)->kind != FEXPR_SEQ) error("struct field should be fseq");
      fiter(fieldit, fe(field)->sons);
      FExpr fieldname = fnext(fieldit);
      FExpr fieldtyp = fnext(fieldit);
      FExpr newfieldname = copy_fexpr(fieldname);
      fe(fieldname)->kind = FEXPR_SYMBOL;
      fe(fieldname)->sym = alloc_FSymbol();
      fp(FSymbol, fe(fieldname)->sym)->f = newfieldname;
      semantic_analysis(fieldtyp);
    }
  } else if (is_fnseq(f)) {
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr name = fnext(it);
    IListFExpr argsit = it;
    FExpr args = fnext(it);
    IListFType argtypes = nil_IListFType();
    forlist(IListFExpr, FExpr, arg, fe(args)->sons) {
      fiter(argit, fe(arg)->sons);
      FExpr argname = fnext(argit);
      FExpr argtyp = fnext(argit);
      semantic_analysis(argtyp);
      argtypes = new_IListFType(fe(argtyp)->typsym, argtypes);
      fe(arg)->kind = FEXPR_SYMBOL;
      fe(arg)->sym = alloc_FSymbol();
      fp(FSymbol, fe(arg)->sym)->f = argname;
      add_decl((Decl){fe(argname)->ident, fe(arg)->sym, fe(argtyp)->typsym});
    }
    IString nameid = fe(name)->ident;
    fe(name)->kind = FEXPR_SYMBOL;
    fe(name)->sym = alloc_FSymbol();
    fp(FSymbol, fe(name)->sym)->isjit = false;

    FExpr rettyp;
    FExpr body;
    if (FExpr_ptr(fcurr(it))->kind == FEXPR_BLOCK) {
      IListFExpr_ptr(argsit)->next = new_IListFExpr(new_ftypesym(new_ftype(FTYPE_VOID)), it);
      it = argsit;
      fnext(it);
      rettyp = fnext(it);
      body = fnext(it);
    } else {
      rettyp = fnext(it);
      body = fnext(it);
      semantic_analysis(rettyp);
    }
    add_fndecl((FnDecl){nameid, argtypes, fe(rettyp)->typsym, false, fe(name)->sym});
    semantic_analysis(body);
  } else if (is_infixcall(f)) {
    *fe(f) = *fe(split_infixseq(f));
    semantic_analysis(f);
  } else if (is_fncall(f)) {
    fiter(it, fe(f)->sons);
    FExpr first = fnext(it);
    FTypeVec* argtypes = new_FTypeVec();
    forlist (IListFExpr, FExpr, arg, it) {
      semantic_analysis(arg);
      FTypeVec_push(argtypes, fe(arg)->typ);
    }
    FnDecl fndecl;
    if (search_fndecl(fe(first)->ident, argtypes, &fndecl)) {
      if (fndecl.isjit) { // jit call
        FExpr jitbody = fp(FSymbol, fndecl.sym)->f;
        semantic_analysis(jitbody);
        FExpr callf = alloc_FExpr();
        *fe(callf) = *fe(f);
        FExpr newf = new_fcontainer(FEXPR_BLOCK);
        push_son(newf, jitbody);
        push_son(newf, callf);
        fe(newf)->typ = fndecl.returntype;
        fe(first)->kind = FEXPR_SYMBOL;
        fe(first)->sym = fndecl.sym;
        *fe(f) = *fe(newf);
      } else { // fn call
        fe(first)->kind = FEXPR_SYMBOL;
        fe(first)->sym = fndecl.sym;
        fe(f)->typ = fndecl.returntype;
      }
    } else {
      error("undeclared %s function", istring_cstr(fe(first)->ident));
    }
  } else if (fe(f)->kind == FEXPR_SEQ) {
    forlist (IListFExpr, FExpr, son, fe(f)->sons) {
      semantic_analysis(son);
    }
  } else if (fe(f)->kind == FEXPR_LIST) {
    forlist (IListFExpr, FExpr, son, fe(f)->sons) {
      semantic_analysis(son);
    }
    fe(f)->typ = fe(IListFExpr_value(fe(f)->sons))->typ;
  } else if (fe(f)->kind == FEXPR_BLOCK) {
    forlist (IListFExpr, FExpr, son, fe(f)->sons) {
      semantic_analysis(son);
    }
  }
}
