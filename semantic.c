#include "flori.h"
#include <string.h>

DeclMap declmap;
FnDeclMap fndeclmap;
InternalDeclMap internaldeclmap;
int fnstacksize;
int tmpcnt = 0;

void semantic_init() {
  declmap = nil_DeclMap();
  fndeclmap = nil_FnDeclMap();
  internaldeclmap = nil_InternalDeclMap();
}

//
// semantic
//

FExpr gen_tmpid() {
  char buf[1024] = {};
  snprintf(buf, 1024, "_tmp%d", tmpcnt++);
  return fident(buf);
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

FType copy_ftype(FType ft) {
  FType newft = alloc_FType();
  *fp(FType, newft) = *fp(FType, ft);
  return newft;
}

FExpr new_ftypesym(FType t) {
  FExpr f = new_fexpr(FEXPR_SYMBOL);
  fe(f)->istyp = true;
  fe(f)->typsym = t;
  return f;
}

bool ftype_is(FType t, char* name) {
  if (fp(FType, t)->kind != FTYPE_PRIM && fp(FType, t)->kind != FTYPE_SYM) return false;
  return strcmp(istring_cstr(fp(FSymbol, fp(FType, t)->sym)->name), name) == 0;
}

bool ftype_eq(FType a, FType b) {
  if (fp(FType, a)->kind == FTYPE_PRIM && fp(FType, b)->kind == FTYPE_PRIM) {
    return istring_eq(fp(FSymbol, fp(FType, a)->sym)->name, fp(FSymbol, fp(FType, b)->sym)->name);
  } else if (fp(FType, a)->kind == FTYPE_PTR && fp(FType, b)->kind == FTYPE_PTR) {
    return ftype_eq(fp(FType, a)->ptrof, fp(FType, b)->ptrof);
  } else if (fp(FType, a)->kind == FTYPE_SYM && fp(FType, b)->kind == FTYPE_SYM) {
    return istring_eq(fp(FSymbol, fp(FType, a)->sym)->name, fp(FSymbol, fp(FType, b)->sym)->name);
  } else {
    return false;
  }
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

FType type_void() {
  Decl decl;
  if (!search_decl(new_istring("void"), &decl)) error("undeclared void type, please import prelude");
  FType t = new_ftype(FTYPE_PRIM);
  fp(FType, t)->sym = decl.sym;
  return t;
}

FType type_int() {
  Decl decl;
  if (!search_decl(new_istring("int"), &decl)) error("undeclared int type, please import prelude");
  FType t = new_ftype(FTYPE_PRIM);
  fp(FType, t)->sym = decl.sym;
  return t;
}

FType type_cstring() {
  Decl decl;
  if (!search_decl(new_istring("cstring"), &decl)) error("undeclared cstring type, please import prelude");
  FType t = new_ftype(FTYPE_PRIM);
  fp(FType, t)->sym = decl.sym;
  return t;
}

FType type_pointer() {
  Decl decl;
  if (!search_decl(new_istring("pointer"), &decl)) error("undeclared pointer type, please import prelude");
  FType t = new_ftype(FTYPE_PRIM);
  fp(FType, t)->sym = decl.sym;
  return t;
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
  if (IListFType_len(fntypes) != argtypes->len) return false;
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
    if (fndecl.isinternal) {
      *retfndecl = fndecl;
      return true;
    }
    
    IListFType fnargtypes;
    if (fp(FSymbol, fndecl.sym)->rewrited) {
      fnargtypes = IListFType_next(fndecl.argtypes);
    } else {
      fnargtypes = fndecl.argtypes;
    }
    if (match_overload(fnargtypes, argtypes)) {
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
// internal decl
//

void add_internal_decl(InternalDecl decl) {
  internaldeclmap = new_InternalDeclMap(decl, internaldeclmap);
}

bool search_internal_decl(IString name, InternalDecl* retdecl) {
  forlist (InternalDeclMap, InternalDecl, decl, internaldeclmap) {
    if (istring_eq(decl.name, name)) {
      *retdecl = decl;
      return true;
    }
  }
  return false;
}

void init_def(char* name, void* fnaddr) {
  IString nameid = new_istring(name);
  FSymbol sym = alloc_FSymbol();
  fp(FSymbol, sym)->isjit = false;
  fp(FSymbol, sym)->isprim = false;
  fp(FSymbol, sym)->istoplevel = false;
  fp(FSymbol, sym)->isinternal = true;
  fp(FSymbol, sym)->name = nameid;
  fp(FSymbol, sym)->internalptr = fnaddr;
  add_decl((Decl){nameid, sym, type_pointer()});
}

void init_internal(char* name, void* fnaddr) {
  IString nameid = new_istring(name);
  InternalDecl decl;
  decl.name = nameid;
  decl.fnptr = fnaddr;
  add_internal_decl(decl);
}

//
// internal definitions
//

void internal_typeptr(FExpr f) {
  fiter(it, fe(f)->sons);
  fnext(it);
  FExpr typ = fnext(it);
  semantic_analysis(typ);
  assert(fe(typ)->kind == FEXPR_SYMBOL && fe(typ)->istyp);
  FType ft = new_ftype(FTYPE_PTR);
  fp(FType, ft)->ptrof = fe(typ)->typsym;
  *fe(f) = *fe(new_ftypesym(ft));
}

void internal_type(FExpr f) {
  fiter(it, fe(f)->sons);
  fnext(it);
  FExpr t = fnext(it);
  Decl decl;
  if (search_decl(fe(t)->ident, &decl)) {
    if (fp(FSymbol, decl.sym)->isprim) {
      FType ft = new_ftype(FTYPE_PRIM);
      fp(FType, ft)->sym = decl.sym;
      *fe(f) = *fe(new_ftypesym(ft));
    } else {
      FType ft = new_ftype(FTYPE_SYM);
      fp(FType, ft)->sym = decl.sym;
      *fe(f) = *fe(new_ftypesym(ft));
    }
  } else {
    error("undeclared %s type", istring_cstr(FExpr_ptr(t)->ident));
  }
}

void semantic_init_internal() {
  init_internal("type_ptr", internal_typeptr);
  init_internal("type", internal_type);
}

//
// isseq util functions
//

#define def_isseq(name, first, mlen)                       \
  bool name(FExpr f) {                                      \
    if (fe(f)->kind != FEXPR_SEQ) return false;             \
    if (IListFExpr_len(fe(f)->sons) < mlen) return false;   \
    return cmp_ident(IListFExpr_value(fe(f)->sons), first); \
  }

def_isseq(is_defprimseq, "defprimitive", 3);
def_isseq(is_defseq, ":=", 3);
def_isseq(is_varseq, "var", 3);
def_isseq(is_setseq, "=", 3);
def_isseq(is_jitseq, "jit", 5);
def_isseq(is_Xseq, "X", 2);
def_isseq(is_sizeofseq, "sizeof", 2);
def_isseq(is_dotseq, ".", 3);
def_isseq(is_getrefseq, "getref", 2);
def_isseq(is_derefseq, "deref", 2);
def_isseq(is_ifseq, "if", 3);
def_isseq(is_fnseq, "fn", 1);
def_isseq(is_structseq, "struct", 1);
def_isseq(is_whileseq, "while", 3);

bool is_fncall(FExpr f) {
  if (fe(f)->kind != FEXPR_SEQ) return false;
  if (IListFExpr_len(fe(f)->sons) < 1) return false;
  return true;
}

bool is_infixseq(FExpr f) {
  if (fe(f)->kind != FEXPR_SEQ) return false;
  if (fe(IListFExpr_value(fe(f)->sons))->kind == FEXPR_OP) return false;
  forlist (IListFExpr, FExpr, son, fe(f)->sons) {
    if (fe(son)->kind == FEXPR_OP) return true;
  }
  return false;
}

// splitter

void consume_infix_stack(FExpr* outstack, FExpr* opstack, int* outpos, int* oppos, int curpriority) {
  while (0 < *oppos) {
    FExpr op = opstack[*oppos-1];
    if (fe(op)->priority > curpriority) break;
    if (*outpos < 2) error("got operator, but illegal infix");
    (*oppos)--;
    FExpr right = outstack[--(*outpos)];
    FExpr left = outstack[--(*outpos)];
    fseq(infixcall, op, left, right);
    outstack[(*outpos)++] = infixcall;
  }
}

FExpr split_infixseq_priority(FExpr f) {
  FExpr outstack[1024] = {};
  FExpr opstack[1024] = {};
  FExpr seqstack[1024] = {};
  int outpos = 0;
  int oppos = 0;
  int seqpos = 0;
  forlist (IListFExpr, FExpr, son, fe(f)->sons) {
    if (fe(son)->kind == FEXPR_OP) {
      if (seqpos == 1) {
        outstack[outpos++] = seqstack[--seqpos];
      } else {
        FExpr sq = new_fcontainer(FEXPR_SEQ);
        while (0 < seqpos) {
          push_son(sq, seqstack[--seqpos]);
        }
        outstack[outpos++] = sq;
      }
      consume_infix_stack(outstack, opstack, &outpos, &oppos, fe(son)->priority);
      opstack[oppos++] = son;
    } else {
      seqstack[seqpos++] = son;
    }
  }
  if (seqpos == 1) {
    seqpos = 0;
    outstack[outpos++] = seqstack[seqpos];
  } else {
    FExpr sq = new_fcontainer(FEXPR_SEQ);
    while (0 < seqpos) {
      push_son(sq, seqstack[--seqpos]);
    }
    outstack[outpos++] = sq;
  }
  consume_infix_stack(outstack, opstack, &outpos, &oppos, 20);
  return outstack[0];
}

//
// utils
//

int get_type_size(FType t) {
  if (fp(FType, t)->kind == FTYPE_PRIM) {
    return fp(FSymbol, fp(FType, t)->sym)->size;
  } else if (fp(FType, t)->kind == FTYPE_PTR) {
    return 8;
  } else if (fp(FType, t)->kind == FTYPE_SYM) {
    return fp(FSymbol, fp(FType, t)->sym)->size;
  } else {
    assert(false);
    return 0;
  }
}

bool is_structtype(FType t) {
  return fp(FType, t)->kind == FTYPE_SYM;
}

bool search_field(FSymbol structsym, IString name, FExpr* retf) {
  FExpr body = fp(FSymbol, structsym)->f;
  forlist (IListFExpr, FExpr, field, fe(body)->sons) {
    fiter(fieldit, fe(field)->sons);
    FExpr fieldsym = fnext(fieldit);
    if (istring_eq(fp(FSymbol, fe(fieldsym)->sym)->name, name)) {
      *retf = fieldsym;
      return true;
    }
  }
  return false;
}

void decide_struct_size(FExpr structsym, FExpr body) {
  int curoffset = 0;
  forlist (IListFExpr, FExpr, field, fe(body)->sons) {
    fiter(fieldit, fe(field)->sons);
    FExpr fieldsym = fnext(fieldit);
    FExpr fieldtyp = fnext(fieldit);
    fp(FSymbol, fe(fieldsym)->sym)->varoffset = curoffset;
    curoffset += get_type_size(fe(fieldtyp)->typsym);
  }
  fp(FSymbol, fe(structsym)->sym)->size = curoffset;
}

bool is_lvalue(FExpr f) {
  if (fe(f)->kind == FEXPR_LIST && IListFExpr_len(fe(f)->sons) == 1) {
    return is_lvalue(IListFExpr_value(fe(f)->sons));
  } else if (fe(f)->kind == FEXPR_BLOCK && IListFExpr_len(fe(f)->sons) >= 1) {
    return is_lvalue(IListFExpr_last(fe(f)->sons));
  } else {
    return fe(f)->kind == FEXPR_SYMBOL || is_derefseq(f) || is_dotseq(f);
  }
}

FExpr generate_dot_copy(char* field) {
  fseq(leftdot, fop("."), fident("p"), fident(field));
  fseq(left, fident("getref"), leftdot);
  fseq(right, fop("."), fident("v"), fident(field));
  fseq(copyf, fident("copy"), left, right);
  return copyf;
}

FExpr generate_copyfn(FExpr copytyp, FExpr fields) {
  fseq(argv, fident("v"), copytyp);
  fseq(argtyp, fident("type_ptr"), copytyp);
  fseq(argp, fident("p"), argtyp);
  flist(args, argp, argv);

  FExpr body = new_fcontainer(FEXPR_BLOCK);
  forlist (IListFExpr, FExpr, field, fe(fields)->sons) {
    fiter(fieldit, fe(field)->sons);
    FExpr fieldsym = fnext(fieldit);
    FExpr c = generate_dot_copy(istring_cstr(fp(FSymbol, fe(fieldsym)->sym)->name));
    push_son(body, c);
  }
  reverse_sons(body);
  
  fseq(copyf, fident("fn"), fident("copy"), args, body);
  return copyf;
}

void rewrite_return_to_arg(FExpr f) {
  fiter(it, fe(f)->sons);
  fnext(it);
  FExpr fnsym = fnext(it);
  FExpr fnargs = fnext(it);
  FExpr rettyp = fnext(it);
  assert(fe(rettyp)->kind == FEXPR_SYMBOL);
  assert(fp(FType, fe(rettyp)->typsym)->kind == FTYPE_SYM);
  FExpr argtyp = new_fexpr(FEXPR_SYMBOL);
  fe(argtyp)->istyp = true;
  fe(argtyp)->typsym = new_ftype(FTYPE_PTR);
  fp(FType, fe(argtyp)->typsym)->ptrof = fe(rettyp)->typsym;
  fseq(argdecl, fident("result"), argtyp);
  push_son(fnargs, argdecl);
  fe(rettyp)->istyp = true;
  fe(rettyp)->typsym = type_void();
  fp(FSymbol, fe(fnsym)->sym)->rewrited = true;
}

bool is_rewrite_fn(FExpr fnsym) {
  return fp(FSymbol, fe(fnsym)->sym)->rewrited;
}

void rewrite_to_lvalue(FExpr f) {
  if (fe(f)->kind == FEXPR_LIST && IListFExpr_len(fe(f)->sons) == 1) {
    rewrite_to_lvalue(IListFExpr_value(fe(f)->sons));
  } else if (fe(f)->kind == FEXPR_BLOCK && IListFExpr_len(fe(f)->sons) >= 1) {
    rewrite_to_lvalue(IListFExpr_last(fe(f)->sons));
  } else if (is_derefseq(f)) {
    FExpr first = IListFExpr_value(fe(f)->sons);
    fe(first)->ident = new_istring("deref_lvalue");
  } else {
    FExpr srcf = copy_fexpr(f);
    fseq(newf, fident("getref"), srcf);
    *fe(f) = *fe(newf);
  }
}
  
FExpr new_fgetref(FExpr f) {
  FExpr getreff = new_fcontainer(FEXPR_SEQ);
  push_son(getreff, f);
  push_son(getreff, fident("getref"));
  return getreff;
}

FExpr inject_result_arg(FExpr f, FExpr result) {
  FExpr newf = copy_fexpr(f);
  fe(newf)->sons = nil_IListFExpr();
  fiter(it, fe(f)->sons);
  push_son(newf, fnext(it));
  push_son(newf, new_fgetref(result));
  forlist (IListFExpr, FExpr, arg, it) {
    push_son(newf, arg);
  }
  reverse_sons(newf);
  return newf;
}

//
// semantic
//

void semantic_analysis(FExpr f) {
  if (fe(f)->evaluated) return;
  // debug("%s", fexpr_tostring(f));

  if (fe(f)->kind == FEXPR_SEQ && IListFExpr_len(fe(f)->sons) >= 1) {
    fiter(it, fe(f)->sons);
    FExpr first = fnext(it);
    InternalDecl decl;
    if (search_internal_decl(fe(first)->ident, &decl)) {
      (decl.fnptr)(f);
      fe(f)->evaluated = true;
      return;
    }
  }
  
  if (fe(f)->kind == FEXPR_INTLIT) {
    fe(f)->typ = type_int();
  } else if (fe(f)->kind == FEXPR_STRLIT) {
    fe(f)->typ = type_cstring();
  } else if (fe(f)->kind == FEXPR_SYMBOL) {
    // discard
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
  } else if (is_fnseq(f)) {
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr name = fnext(it);
    IString nameid;
    if (fe(name)->kind == FEXPR_IDENT) {
      nameid = fe(name)->ident;
      fe(name)->kind = FEXPR_SYMBOL;
      fe(name)->sym = alloc_FSymbol();
      fp(FSymbol, fe(name)->sym)->name = nameid;
      fp(FSymbol, fe(name)->sym)->f = f;
      fp(FSymbol, fe(name)->sym)->isjit = false;
    } else {
      nameid = fp(FSymbol, fe(name)->sym)->name;
    }
    IListFExpr argsit = it;
    FExpr args = fnext(it);

    FExpr rettyp;
    FExpr body;
    fnstacksize = 0;
    if (fe(fcurr(it))->kind == FEXPR_BLOCK) {
      IListFExpr_ptr(argsit)->next = new_IListFExpr(new_ftypesym(type_void()), it);
      it = argsit;
      fnext(it);
      rettyp = fnext(it);
      body = fnext(it);
    } else {
      rettyp = fnext(it);
      body = fnext(it);
      semantic_analysis(rettyp);
      if (is_structtype(fe(rettyp)->typsym)) {
        rewrite_return_to_arg(f);
        semantic_analysis(f);
        return;
      }
    }
    IListFType argtypes = nil_IListFType();
    forlist(IListFExpr, FExpr, arg, fe(args)->sons) {
      fiter(argit, fe(arg)->sons);
      FExpr argname = fnext(argit);
      FExpr argtyp = fnext(argit);
      semantic_analysis(argtyp);
      assert(fe(argtyp)->kind == FEXPR_SYMBOL && fe(argtyp)->istyp);
      if (is_structtype(fe(argtyp)->typsym)) {
        fseq(newargtyp, fident("type_ptr"), copy_fexpr(argtyp));
        *fe(argtyp) = *fe(newargtyp);
        semantic_analysis(argtyp);
      }
      argtypes = new_IListFType(fe(argtyp)->typsym, argtypes);
      fe(arg)->kind = FEXPR_SYMBOL;
      fe(arg)->sym = alloc_FSymbol();
      fp(FSymbol, fe(arg)->sym)->name = fe(argname)->ident;
      fp(FSymbol, fe(arg)->sym)->f = argname;
      add_decl((Decl){fe(argname)->ident, fe(arg)->sym, fe(argtyp)->typsym});
    }
    argtypes = IListFType_reverse(argtypes);
    add_fndecl((FnDecl){nameid, argtypes, fe(rettyp)->typsym, false, fe(name)->sym, false});
    semantic_analysis(body);
    fp(FSymbol, fe(name)->sym)->stacksize = fnstacksize;
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
    argtypes = IListFType_reverse(argtypes);
    semantic_analysis(rettyp);
    IString nameid = fe(name)->ident;
    fe(name)->kind = FEXPR_SYMBOL;
    fe(name)->sym = alloc_FSymbol();
    fp(FSymbol, fe(name)->sym)->name = nameid;
    fp(FSymbol, fe(name)->sym)->f = body;
    fp(FSymbol, fe(name)->sym)->isjit = true;
    add_fndecl((FnDecl){nameid, argtypes, fe(rettyp)->typsym, true, fe(name)->sym, false});
  } else if (is_structseq(f)) {
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr name = fnext(it);
    IString namestr = fe(name)->ident;
    FExpr body = fnext(it);
    fe(name)->kind = FEXPR_SYMBOL;
    fe(name)->sym = alloc_FSymbol();
    fp(FSymbol, fe(name)->sym)->isprim = false;
    fp(FSymbol, fe(name)->sym)->name = namestr;
    fp(FSymbol, fe(name)->sym)->f = body;
    add_decl((Decl){namestr, fe(name)->sym});
    forlist (IListFExpr, FExpr, field, fe(body)->sons) {
      if (fe(field)->kind != FEXPR_SEQ) error("struct field should be fseq");
      fiter(fieldit, fe(field)->sons);
      FExpr fieldname = fnext(fieldit);
      FExpr fieldtyp = fnext(fieldit);
      FExpr newfieldname = copy_fexpr(fieldname);
      fe(fieldname)->kind = FEXPR_SYMBOL;
      fe(fieldname)->sym = alloc_FSymbol();
      fp(FSymbol, fe(fieldname)->sym)->name = fe(newfieldname)->ident;
      fp(FSymbol, fe(fieldname)->sym)->f = newfieldname;
      semantic_analysis(fieldtyp);
      fe(fieldname)->typ = fe(fieldtyp)->typsym;
    }
    FExpr typsym = new_fexpr(FEXPR_SYMBOL);
    fe(typsym)->evaluated = true;
    fe(typsym)->istyp = true;
    fe(typsym)->typsym = new_ftype(FTYPE_SYM);
    fp(FType, fe(typsym)->typsym)->sym = fe(name)->sym;
    FExpr copyfn = generate_copyfn(typsym, body);
    FExpr newf = copy_fexpr(f);
    decide_struct_size(name, body);
    semantic_analysis(copyfn);
    *fe(f) = *fe(new_fcontainer(FEXPR_BLOCK));
    push_son(f, copyfn);
    push_son(f, newf);
  } else if (is_infixseq(f)) {
    *fe(f) = *fe(split_infixseq_priority(f));
    semantic_analysis(f);
  } else if (is_defprimseq(f)) {
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr name = fnext(it);
    FExpr size = fnext(it);
    if (fe(name)->kind != FEXPR_IDENT) error("defprimitive should be specify type-name");
    if (fe(size)->kind != FEXPR_INTLIT) error("defprimitive should be specify type-size");
    IString nameid = fe(name)->ident;
    fe(name)->kind = FEXPR_SYMBOL;
    fe(name)->sym = alloc_FSymbol();
    fp(FSymbol, fe(name)->sym)->isprim = true;
    fp(FSymbol, fe(name)->sym)->name = nameid;
    fp(FSymbol, fe(name)->sym)->size = fe(size)->intval;
    add_decl((Decl){nameid, fe(name)->sym});
  } else if (is_defseq(f)) {
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr name = fnext(it);
    FExpr value = fnext(it);
    semantic_analysis(value);
    fseq(vdecl, fident("var"), copy_fexpr(name), new_ftypesym(fe(value)->typ));
    fseq(vinit, fident("="), copy_fexpr(name), value);
    fblock(blk, vdecl, vinit);
    semantic_analysis(blk);
    *fe(f) = *fe(blk);
  } else if (is_varseq(f)) {
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr name = fnext(it);
    if (fe(name)->kind == FEXPR_SYMBOL) return;
    FExpr typ = fnext(it);
    IString namestr = fe(name)->ident;
    fe(name)->kind = FEXPR_SYMBOL;
    fe(name)->sym = alloc_FSymbol();
    fp(FSymbol, fe(name)->sym)->name = namestr;
    semantic_analysis(typ);
    add_decl((Decl){namestr, fe(name)->sym, fe(typ)->typsym});
    fnstacksize += get_type_size(fe(typ)->typsym);
    fe(f)->typ = type_void();
  } else if (is_setseq(f)) {
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr lvalue = fnext(it);
    FExpr rvalue = fnext(it);
    rewrite_to_lvalue(lvalue);
    semantic_analysis(lvalue);
    semantic_analysis(rvalue);
    if (fp(FType, fe(lvalue)->typ)->kind != FTYPE_PTR) error("lvalue should be ^ptr type: %s", fexpr_tostring(lvalue));
    if (!ftype_eq(fp(FType, fe(lvalue)->typ)->ptrof, fe(rvalue)->typ)) {
      error("type mismatch in `=");
    }
    
    if (is_structtype(fp(FType, fe(lvalue)->typ)->ptrof)) {
      fseq(copycall, fident("copy"), lvalue, new_fgetref(rvalue));
      *fe(f) = *fe(copycall);
      semantic_analysis(f);
    }
    fe(f)->typ = type_void();
  } else if (is_Xseq(f)) {
    fiter(it, fe(f)->sons);
    fnext(it);
    semantic_analysis(fnext(it));
    fe(f)->typ = type_void();
  } else if (is_sizeofseq(f)) {
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr ftyp = fnext(it);
    semantic_analysis(ftyp);
    if (fe(ftyp)->kind != FEXPR_SYMBOL) error("sizeof argument should be type");
    fe(f)->kind = FEXPR_INTLIT;
    fe(f)->intval = get_type_size(fe(ftyp)->typsym);
    semantic_analysis(f);
  } else if (is_dotseq(f)) {
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr lvalue = fnext(it);
    FExpr fieldname = fnext(it);
    semantic_analysis(lvalue);
    FType structtyp;
    if (fp(FType, fe(lvalue)->typ)->kind == FTYPE_PTR) {
      structtyp = fp(FType, fe(lvalue)->typ)->ptrof;
    } else {
      structtyp = fe(lvalue)->typ;
    }
    if (fe(fieldname)->kind != FEXPR_IDENT) error("right of `. should be field-name, but got %s", FExprKind_tostring(fe(fieldname)->kind));
    if (!is_structtype(structtyp)) error("can't get %s field of no-struct value", istring_cstr(fe(fieldname)->ident));
    FSymbol structsym = fp(FType, structtyp)->sym;
    FExpr fieldsym;
    if (!search_field(structsym, fe(fieldname)->ident, &fieldsym)) error("%s struct hasn't %s field", istring_cstr(fp(FSymbol, structsym)->name), istring_cstr(fe(fieldname)->ident));
    *fe(fieldname) = *fe(fieldsym);
    fe(f)->typ = fe(fieldsym)->typ;
  } else if (is_getrefseq(f)) {
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr lvalue = fnext(it);
    semantic_analysis(lvalue);
    if (!is_lvalue(lvalue)) error("can't get address of expression, should be lvalue");
    fe(f)->typ = new_ftype(FTYPE_PTR);
    fp(FType, fe(f)->typ)->ptrof = fe(lvalue)->typ;
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
    fe(f)->typ = type_int(); // FIXME: if expression type
  } else if (is_whileseq(f)) {
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr cond = fnext(it);
    FExpr body = fnext(it);
    semantic_analysis(cond);
    semantic_analysis(body);
    fe(f)->typ = type_void();
  } else if (is_fncall(f)) {
    fiter(it, fe(f)->sons);
    FExpr first = fnext(it);
    
    if (fe(first)->kind == FEXPR_SYMBOL) {
      forlist (IListFExpr, FExpr, arg, it) {
        semantic_analysis(arg);
      }
      return;
    }
    
    FTypeVec* argtypes = new_FTypeVec();
    forlist (IListFExpr, FExpr, arg, it) {
      semantic_analysis(arg);
      FTypeVec_push(argtypes, fe(arg)->typ);
    }
    FnDecl fndecl;
    if (search_fndecl(fe(first)->ident, argtypes, &fndecl)) {
      if (fndecl.isjit) { // jit call
        FExpr jitbody = deepcopy_fexpr(fp(FSymbol, fndecl.sym)->f);
        semantic_analysis(jitbody);
        FExpr callf = copy_fexpr(f);
        FExpr newf = new_fcontainer(FEXPR_BLOCK);
        push_son(newf, jitbody);
        push_son(newf, callf);
        fe(newf)->typ = fndecl.returntype;
        fe(first)->kind = FEXPR_SYMBOL;
        fe(first)->sym = fndecl.sym;
        fe(newf)->srcf = copy_fexpr(callf);
        *fe(f) = *fe(newf);
      } else { // fn call
        fe(first)->kind = FEXPR_SYMBOL;
        fe(first)->sym = fndecl.sym;
        fe(f)->typ = fndecl.returntype;
        if (is_rewrite_fn(first)) { // for return struct value
          FExpr tmp = gen_tmpid();
          assert(fp(FType, IListFType_value(fndecl.argtypes))->kind == FTYPE_PTR);
          FType typ = fp(FType, IListFType_value(fndecl.argtypes))->ptrof;
          fseq(vardecl, fident("var"), copy_fexpr(tmp), new_ftypesym(typ));
          fblock(blk, vardecl, inject_result_arg(f, copy_fexpr(tmp)), copy_fexpr(tmp));
          semantic_analysis(blk);
          *fe(f) = *fe(blk);
        }
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
    if (IListFExpr_len(fe(f)->sons) >= 1) {
      fe(f)->typ = fe(IListFExpr_last(fe(f)->sons))->typ;
    }
  } else {
    assert(false);
  }
  fe(f)->evaluated = true;
}

void semantic_analysis_toplevel(FExpr f) {
  if (is_jitseq(f) || is_fnseq(f)) {
    semantic_analysis(f);
  } else if (is_infixseq(f)) {
    *fe(f) = *fe(split_infixseq_priority(f));
    semantic_analysis_toplevel(f);
  } else if (is_defseq(f)) {
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr name = fnext(it);
    FExpr value = fnext(it);
    IString namestr = fe(name)->ident;
    fe(name)->kind = FEXPR_SYMBOL;
    fe(name)->sym = alloc_FSymbol();
    fp(FSymbol, fe(name)->sym)->name = namestr;
    fp(FSymbol, fe(name)->sym)->istoplevel = true;
    semantic_analysis(value);
    add_decl((Decl){namestr, fe(name)->sym, fe(value)->typ});
    fe(f)->typ = type_void();
  } else if (fe(f)->kind == FEXPR_IDENT && cmp_ident(f, "internal_init_defs")) {
    semantic_init_defs();
    *fe(f) = *fe(new_fcontainer(FEXPR_SEQ));
  } else {
    semantic_analysis(f);
  }
}

//
// internal definitions
//

void internal_print(size_t x) {
  printf("%zd", x);
}

void semantic_init_defs() {
  init_def("internal_print_ptr", internal_print);
}
