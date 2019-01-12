#include "flori.h"
#include <string.h>

// semantic globals
DeclMap declmap;
FnDeclMap fndeclmap;
InternalDeclMap internaldeclmap;
int fnstacksize;
int tmpcnt = 0;

// codegen globals
#define write_hex2(id, ...) \
  uint8_t id[] = {__VA_ARGS__}; \
  jit_alloc_write(id, sizeof(id));
#define write_hex1(ln, ...) write_hex2(CONCAT(_hex, ln), __VA_ARGS__)
#define write_hex(...) write_hex1(__LINE__, __VA_ARGS__)
int curroffset;

//
// semantic
//

FExpr gen_tmpid() {
  char buf[1024] = {};
  snprintf(buf, 1024, "_tmp%d", tmpcnt++);
  return fident(buf);
}

FExpr fnext_impl(IListFExpr* il, int line) {
  check_next(*il, "%d: require more token", line);
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

bool is_defined_fexpr() {
  Decl decl;
  return search_decl(new_istring("fexpr"), &decl);
}

FType type_fexpr() {
  Decl decl;
  if (!search_decl(new_istring("fexpr"), &decl)) error("undeclared fexpr type, please import prelude");
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

//
// init utils
//

void init_def(char* name, void* fnaddr) {
  IString nameid = new_istring(name);
  FSymbol sym = alloc_FSymbol();
  fp(FSymbol, sym)->isjit = false;
  fp(FSymbol, sym)->ismacro = false;
  fp(FSymbol, sym)->isprim = false;
  fp(FSymbol, sym)->istoplevel = false;
  fp(FSymbol, sym)->isinternal = true;
  fp(FSymbol, sym)->name = nameid;
  fp(FSymbol, sym)->internalptr = fnaddr;
  add_decl((Decl){nameid, sym, type_pointer()});
}

void init_internal(char* name, bool issyntax, void* semfn, void* genfn, void* lvaluefn) {
  IString nameid = new_istring(name);
  InternalDecl decl;
  decl.name = nameid;
  decl.issyntax = issyntax;
  decl.semanticfn = semfn;
  decl.codegenfn = genfn;
  decl.lvaluegenfn = lvaluefn;
  add_internal_decl(decl);
}

//
// infix
//

bool is_infixseq(FExpr f) {
  if (fe(f)->kind != FEXPR_SEQ) return false;
  if (fe(IListFExpr_value(fe(f)->sons))->kind == FEXPR_OP) return false;
  forlist (IListFExpr, FExpr, son, fe(f)->sons) {
    if (fe(son)->kind == FEXPR_OP) return true;
  }
  return false;
}

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
  if (fe(f)->istoplevel) {
    fe(outstack[0])->istoplevel = true;
  }
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
  } else if (fe(f)->kind == FEXPR_SEQ && IListFExpr_len(fe(f)->sons) == 2 && cmp_ident(IListFExpr_value(fe(f)->sons), "deref")) {
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

bool has_ident(FExpr f) {
  return fe(f)->kind == FEXPR_IDENT || fe(f)->kind == FEXPR_OP;
}

//
// semantic
//

#define push_arg(x) asm volatile("push %0" : : "r"(x) : "rsp")
#define pop_args(n) asm volatile(".intel_syntax noprefix; add rsp, %0; .att_syntax;" : : "r"(n*8));

size_t call_macro_prim0(void* fnaddr) {
  size_t ret;
  asm volatile(".intel_syntax noprefix;"
               "mov rax, %1;"
               "call rax;"
               "mov %0, rax;"
               ".att_syntax;" : "=r"(ret) : "r"(fnaddr) : "rax");
  return ret;
}

size_t call_macro_prim1(void* fnaddr, size_t arg1) {
  size_t ret;
  asm volatile(".intel_syntax noprefix;"
               "mov rax, %1;"
               "push %2;"
               "call rax;"
               "add rsp, 8;"
               "mov %0, rax;"
               ".att_syntax;" : "=r"(ret) : "r"(fnaddr), "r"(arg1) : "rax", "rsp");
  return ret;
}

size_t call_macro_prim2(void* fnaddr, size_t arg1, size_t arg2) {
  size_t ret;
  asm volatile(".intel_syntax noprefix;"
               "mov rax, %1;"
               "push %3;"
               "push %2;"
               "call rax;"
               "add rsp, 16;"
               "mov %0, rax;"
               ".att_syntax;" : "=r"(ret) : "r"(fnaddr), "r"(arg1), "r"(arg2) : "rax", "rsp");
  return ret;
}

size_t call_macro_prim3(void* fnaddr, size_t arg1, size_t arg2, size_t arg3) {
  size_t ret;
  asm volatile(".intel_syntax noprefix;"
               "mov rax, %1;"
               "push %4;"
               "push %3;"
               "push %2;"
               "call rax;"
               "add rsp, 24;"
               "mov %0, rax;"
               ".att_syntax;" : "=r"(ret) : "r"(fnaddr), "r"(arg1), "r"(arg2), "r"(arg3) : "rax", "rsp");
  return ret;
}

FExpr call_macro(FSymbol sym, IListFExpr args) {
  void* fnaddr = jit_toptr(fp(FSymbol, sym)->fnidx);
  size_t argn = IListFExpr_len(args);
  size_t fidx;
  if (argn == 0) {
    fidx = call_macro_prim0(fnaddr);
  } else if (argn == 1) {
    fiter(it, args);
    fidx = call_macro_prim1(fnaddr, fnext(it).index);
  } else if (argn == 2) {
    fiter(it, args);
    fidx = call_macro_prim2(fnaddr, fnext(it).index, fnext(it).index);
  } else if (argn == 3) {
    fiter(it, args);
    fidx = call_macro_prim3(fnaddr, fnext(it).index, fnext(it).index, fnext(it).index);
  } else {
    assert(false);
  }
  return (FExpr){fidx};
}

FTypeVec* gen_fexpr_argtypes(int n) {
  FTypeVec* v = new_FTypeVec();
  FType fexprtyp = type_fexpr();
  for (int i=0; i<n; i++) {
    FTypeVec_push(v, fexprtyp);
  }
  return v;
}

void boot_semantic(FExpr f) {
  if (fe(f)->evaluated) return;
  // debug("semantic: %s", fexpr_tostring(f));

  if (fe(f)->kind == FEXPR_SEQ && IListFExpr_len(fe(f)->sons) != 0) {
    fiter(it, fe(f)->sons);
    FExpr first = fnext(it);
    InternalDecl decl;
    if (has_ident(first) && search_internal_decl(fe(first)->ident, &decl)) {
      if (!decl.issyntax && is_infixseq(f)) {
        *fe(f) = *fe(split_infixseq_priority(f));
        boot_semantic(f);
        return;
      }
      (decl.semanticfn)(f);
      fe(f)->evaluated = true;
      return;
    }
  }
  if (fe(f)->kind == FEXPR_SEQ && IListFExpr_len(fe(f)->sons) != 0) {
    fiter(it, fe(f)->sons);
    FExpr first = fnext(it);
    FnDecl fndecl;
    FTypeVec* argtypes = gen_fexpr_argtypes(IListFExpr_len(fe(f)->sons)-1);
    if (has_ident(first) && search_fndecl(fe(first)->ident, argtypes, &fndecl) && fp(FSymbol, fndecl.sym)->ismacro) {
      FExpr expanded = call_macro(fndecl.sym, IListFExpr_next(fe(f)->sons));
      // debug("expand: %s", fexpr_tostring(expanded));
      *fe(f) = *fe(expanded);
      boot_semantic(f);
      return;
    }
  }
  if (is_infixseq(f)) {
    *fe(f) = *fe(split_infixseq_priority(f));
    boot_semantic(f);
    return;
  }

  if (fe(f)->kind == FEXPR_INTLIT) {
    fe(f)->typ = type_int();
  } else if (fe(f)->kind == FEXPR_STRLIT) {
    fe(f)->typ = type_cstring();
  } else if (fe(f)->kind == FEXPR_SYMBOL) {
    // discard
  } else if (fe(f)->kind == FEXPR_IDENT) {
    Decl decl;
    if (search_decl(fe(f)->ident, &decl)) {
      fe(f)->kind = FEXPR_SYMBOL;
      fe(f)->sym = decl.sym;
      fe(f)->typ = decl.typ;
    } else {
      fseq(newf, copy_fexpr(f));
      boot_semantic(newf);
      *fe(f) = *fe(newf);
    }
  } else if (fe(f)->kind == FEXPR_SEQ && IListFExpr_len(fe(f)->sons) != 0) {
    fiter(it, fe(f)->sons);
    FExpr first = fnext(it);
    
    if (fe(first)->kind == FEXPR_SYMBOL) {
      forlist (IListFExpr, FExpr, arg, it) {
        boot_semantic(arg);
      }
      return;
    }
    
    FTypeVec* argtypes = new_FTypeVec();
    forlist (IListFExpr, FExpr, arg, it) {
      boot_semantic(arg);
      FTypeVec_push(argtypes, fe(arg)->typ);
    }
    FnDecl fndecl;
    if (search_fndecl(fe(first)->ident, argtypes, &fndecl)) {
      if (fndecl.isjit) { // jit call
        FExpr jitbody = deepcopy_fexpr(fp(FSymbol, fndecl.sym)->f);
        boot_semantic(jitbody);
        FExpr callf = copy_fexpr(f);
        FExpr newf = new_fcontainer(FEXPR_BLOCK);
        push_son(newf, jitbody);
        push_son(newf, callf);
        fe(newf)->typ = fndecl.returntype;
        fe(first)->kind = FEXPR_SYMBOL;
        fe(first)->sym = fndecl.sym;
        fe(newf)->srcf = copy_fexpr(callf);
        *fe(f) = *fe(newf);
      } else if (fp(FSymbol, fndecl.sym)->ismacro) { // macro call
        // FExpr newf = copy_fexpr(f);
        FExpr expanded = call_macro(fndecl.sym, IListFExpr_next(fe(f)->sons));
        *fe(f) = *fe(expanded);
        boot_semantic(f);
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
          boot_semantic(blk);
          *fe(f) = *fe(blk);
        }
      }
    } else {
      error("undeclared %s function", istring_cstr(fe(first)->ident));
    }
  } else if (fe(f)->kind == FEXPR_SEQ) {
    fe(f)->typ = type_void();
  } else if (fe(f)->kind == FEXPR_LIST) {
    forlist (IListFExpr, FExpr, son, fe(f)->sons) {
      boot_semantic(son);
    }
    fe(f)->typ = fe(IListFExpr_value(fe(f)->sons))->typ;
  } else if (fe(f)->kind == FEXPR_BLOCK) {
    forlist (IListFExpr, FExpr, son, fe(f)->sons) {
      boot_semantic(son);
    }
    if (IListFExpr_len(fe(f)->sons) >= 1) {
      fe(f)->typ = fe(IListFExpr_last(fe(f)->sons))->typ;
    } else {
      fe(f)->typ = type_void();
    }
  } else {
    assert(false);
  }
  fe(f)->evaluated = true;
}

//
// codegen
//

void write_lendian(int x) {
  int b1 = x & 0xFF;
  int b2 = (x >> 8) & 0xFF;
  int b3 = (x >> 16) & 0xFF;
  int b4 = (x >> 24) & 0xFF;
  write_hex(b1, b2, b3, b4);
}

void jit_fixup_lendian(int fixupidx, int x) {
  fixup_lendian32(jit_toptr(fixupidx), x);
}

void gen_prologue(int stacksize) {
  write_hex(
      0x55,             // push rbp
      0x48, 0x89, 0xe5, // mov rbp, rsp
      0x48, 0x81, 0xec  // sub rsp, ..
  );
  write_lendian(stacksize);
}
void gen_epilogue() {
  write_hex(
      0x48, 0x89, 0xec, // mov rsp, rbp
      0x5d,             // pop rbp
      0xc3              // ret
  )
}

void gen_push_int(int x) {
  write_hex(0x68);
  write_lendian(x);
}

void gen_cstring(char* s) {
  write_hex(0x48, 0xb8); // movabs rax, ..
  size_t jitidx = jit_getidx();
  write_hex(0, 0, 0, 0, 0, 0, 0, 0);
  write_hex(0x50); // push rax
  size_t dataidx = data_cstring(s);
  fixup_lendian64(jit_toptr(jitidx), (size_t)data_toptr(dataidx));
  reloc_add_info(jitidx, dataidx);
}

void boot_codegen_lvalue(FExpr f) {  
  if (fe(f)->kind == FEXPR_LIST && IListFExpr_len(fe(f)->sons) == 1) {
    boot_codegen_lvalue(IListFExpr_value(fe(f)->sons));
  } else if (fe(f)->kind == FEXPR_BLOCK && IListFExpr_len(fe(f)->sons) >= 1) {
    fiter(it, fe(f)->sons);
    while (!IListFExpr_isnil(IListFExpr_next(it))) {
      boot_codegen(fnext(it));
    }
    boot_codegen_lvalue(IListFExpr_last(fe(f)->sons));
  } else if (fe(f)->kind == FEXPR_SYMBOL) {
    if (fp(FSymbol, fe(f)->sym)->istoplevel) {
      write_hex(0x48, 0xb8); // movabs rax, ..
      size_t jitidx = jit_getidx();
      write_hex(0, 0, 0, 0, 0, 0, 0, 0);
      write_hex(0x50); // push rax
      fixup_lendian64(jit_toptr(jitidx), (size_t)data_toptr(fp(FSymbol, fe(f)->sym)->vardataidx));
      reloc_add_info(jitidx, fp(FSymbol, fe(f)->sym)->vardataidx);
    } else {
      write_hex(0x48, 0x8d, 0x85); // lea rax, [rbp-..]
      write_lendian(-fp(FSymbol, fe(f)->sym)->varoffset);
      write_hex(0x50); // push rax
    }
  } else if (fe(f)->kind == FEXPR_SEQ && IListFExpr_len(fe(f)->sons) >= 1) {
    fiter(it, fe(f)->sons);
    FExpr first = fnext(it);
    InternalDecl decl;
    if (has_ident(first) && search_internal_decl(fe(first)->ident, &decl) && decl.lvaluegenfn != NULL) {
      (decl.lvaluegenfn)(f);
    } else {
      error("%s isn't lvalue", fexpr_tostring(f));
    }
  } else {
    error("%s isn't lvalue", fexpr_tostring(f));
  }
}

void boot_codegen(FExpr f) {
  if (fe(f)->codegened) return;
  // debug("codegen: %s", fexpr_tostring(f));
  
  if (fe(f)->kind == FEXPR_SEQ && IListFExpr_len(fe(f)->sons) != 0) {
    fiter(it, fe(f)->sons);
    FExpr first = fnext(it);
    InternalDecl decl;
    if (has_ident(first) && search_internal_decl(fe(first)->ident, &decl)) {
      if (decl.codegenfn != NULL) {
        (decl.codegenfn)(f);
      }
      fe(f)->codegened = true;
      return;
    }
  }

  if (fe(f)->kind == FEXPR_INTLIT) {
    gen_push_int(fe(f)->intval);
  } else if (fe(f)->kind == FEXPR_STRLIT) {
    gen_cstring(istring_cstr(fe(f)->strval));
  } else if (fe(f)->kind == FEXPR_SYMBOL) {
    if (fp(FSymbol, fe(f)->sym)->istoplevel) {
      write_hex(0x48, 0xb8); // movabs rax, ..
      size_t jitidx = jit_getidx();
      write_hex(0, 0, 0, 0, 0, 0, 0, 0);
      write_hex(0xff, 0x30); // push [rax]
      fixup_lendian64(jit_toptr(jitidx), (size_t)data_toptr(fp(FSymbol, fe(f)->sym)->vardataidx));
      reloc_add_info(jitidx, fp(FSymbol, fe(f)->sym)->vardataidx);
    } else if (fp(FSymbol, fe(f)->sym)->isinternal) {
      write_hex(0x48, 0xb8); // movabs rax, ..
      size_t jitidx = jit_getidx();
      write_hex(0, 0, 0, 0, 0, 0, 0, 0);
      write_hex(0x50); // push rax
      fixup_lendian64(jit_toptr(jitidx), (size_t)fp(FSymbol, fe(f)->sym)->internalptr);
      // TODO: relocation for internal ffi
    } else {
      write_hex(0xff, 0xb5);
      write_lendian(-fp(FSymbol, fe(f)->sym)->varoffset);
    }
  } else if (fe(f)->kind == FEXPR_IDENT) {
    error("unresolved `%s ident.", istring_cstr(fe(f)->ident));
  } else if (fe(f)->kind == FEXPR_SEQ && IListFExpr_len(fe(f)->sons) != 0) {
    FExpr first = IListFExpr_value(fe(f)->sons);
    if (fe(first)->kind == FEXPR_SYMBOL) {
      forlist (IListFExpr, FExpr, arg, IListFExpr_next(fe(f)->sons)) {
        boot_codegen(arg);
      }
      if (fp(FSymbol, fe(first)->sym)->isjit) {
        return;
      }
      fiter(it, fe(fp(FSymbol, fe(first)->sym)->f)->sons);
      fnext(it); fnext(it); fnext(it);
      // FExpr rettyp = fnext(it);
      int callstacksize = IListFExpr_len(IListFExpr_next(fe(f)->sons))*8;
      // if (is_structtype(fe(rettyp)->typsym)) {
      //   write_hex(0x48, 0x81, 0xec); // sub rsp, ..
      //   write_lendian(get_type_size(fe(rettyp)->typsym));
      //   callstacksize += get_type_size(fe(rettyp)->typsym);
      // }
      int rel = fp(FSymbol, fe(first)->sym)->fnidx - jit_getidx() - 5;
      write_hex(0xE8); // call
      write_lendian(rel);
      write_hex(0x48, 0x81, 0xc4); // add rsp, ..
      write_lendian(callstacksize);
      if (!ftype_is(fe(f)->typ, "void")) {
        write_hex(0x50); // push rax
      }
    } else if (fe(first)->kind == FEXPR_IDENT) {
      error("unresolved `%s function", istring_cstr(fe(first)->ident));
    } else {
      error("unresolved %s", FExprKind_tostring(fe(first)->kind));
    }
  } else if (fe(f)->kind == FEXPR_SEQ) {
  } else if (fe(f)->kind == FEXPR_LIST) {
    forlist (IListFExpr, FExpr, son, fe(f)->sons) {
      boot_codegen(son);
    }
  } else if (fe(f)->kind == FEXPR_BLOCK) {
    forlist (IListFExpr, FExpr, son, fe(f)->sons) {
      boot_codegen(son);
    }
  } else {
    assert(false);
  }
  fe(f)->codegened = true;
}

void boot_eval_toplevel(FExpr f) {
  fe(f)->istoplevel = true;
  boot_semantic(f);
  boot_codegen(f);
}

//
// semantics
//

void semantic_typeptr(FExpr f) {
  fiter(it, fe(f)->sons);
  fnext(it);
  FExpr typ = fnext(it);
  boot_semantic(typ);
  assert(fe(typ)->kind == FEXPR_SYMBOL && fe(typ)->istyp);
  FType ft = new_ftype(FTYPE_PTR);
  fp(FType, ft)->ptrof = fe(typ)->typsym;
  *fe(f) = *fe(new_ftypesym(ft));
}

void semantic_type(FExpr f) {
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

void semantic_defprimitive(FExpr f) {
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
}

void semantic_fn(FExpr f) {
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
    fp(FSymbol, fe(name)->sym)->ismacro = false;
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
    boot_semantic(rettyp);
    if (is_structtype(fe(rettyp)->typsym)) {
      rewrite_return_to_arg(f);
      boot_semantic(f);
      return;
    }
  }
  IListFType argtypes = nil_IListFType();
  forlist(IListFExpr, FExpr, arg, fe(args)->sons) {
    fiter(argit, fe(arg)->sons);
    FExpr argname = fnext(argit);
    FExpr argtyp = fnext(argit);
    boot_semantic(argtyp);
    assert(fe(argtyp)->kind == FEXPR_SYMBOL && fe(argtyp)->istyp);
    if (is_structtype(fe(argtyp)->typsym)) {
      fseq(newargtyp, fident("type_ptr"), copy_fexpr(argtyp));
      *fe(argtyp) = *fe(newargtyp);
      boot_semantic(argtyp);
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
  boot_semantic(body);
  fp(FSymbol, fe(name)->sym)->stacksize = fnstacksize;
}

void semantic_jit(FExpr f) {
  fiter(it, fe(f)->sons);
  fnext(it);
  FExpr name = fnext(it);
  FExpr args = fnext(it);
  FExpr rettyp = fnext(it);
  FExpr body = fnext(it);
  IListFType argtypes = nil_IListFType();
  forlist(IListFExpr, FExpr, arg, fe(args)->sons) {
    boot_semantic(arg);
    argtypes = new_IListFType(fe(arg)->typsym, argtypes);
  }
  argtypes = IListFType_reverse(argtypes);
  boot_semantic(rettyp);
  IString nameid = fe(name)->ident;
  fe(name)->kind = FEXPR_SYMBOL;
  fe(name)->sym = alloc_FSymbol();
  fp(FSymbol, fe(name)->sym)->name = nameid;
  fp(FSymbol, fe(name)->sym)->f = body;
  fp(FSymbol, fe(name)->sym)->isjit = true;
  add_fndecl((FnDecl){nameid, argtypes, fe(rettyp)->typsym, true, fe(name)->sym, false});
}

void semantic_struct(FExpr f) {
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
    boot_semantic(fieldtyp);
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
  boot_semantic(copyfn);
  *fe(f) = *fe(new_fcontainer(FEXPR_BLOCK));
  push_son(f, copyfn);
  push_son(f, newf);
}

void semantic_macro(FExpr f) {
  fiter(macit, fe(f)->sons);
  FExpr mac = fnext(macit);
  fe(mac)->ident = new_istring("fn");
  boot_semantic(f);

  fiter(fnit, fe(f)->sons);
  fnext(fnit);
  FExpr fnsym = fnext(fnit);
  assert(fe(fnsym)->kind == FEXPR_SYMBOL);
  fp(FSymbol, fe(fnsym)->sym)->ismacro = true;
  boot_codegen(f);
}

void semantic_def(FExpr f) {
  fiter(it, fe(f)->sons);
  fnext(it);
  FExpr name = fnext(it);
  FExpr value = fnext(it);
  boot_semantic(value);
  fseq(vdecl, fident("var"), copy_fexpr(name), new_ftypesym(fe(value)->typ));
  fseq(vinit, fident("="), copy_fexpr(name), value);
  if (fe(f)->istoplevel) {
    fe(vdecl)->istoplevel = true;
    fseq(staticinit, fident("static"), vinit);
    vinit = staticinit;
  }
  fblock(blk, vdecl, vinit);
  boot_semantic(blk);
  *fe(f) = *fe(blk);
}

void semantic_var(FExpr f) {
  fiter(it, fe(f)->sons);
  fnext(it);
  FExpr name = fnext(it);
  if (fe(name)->kind == FEXPR_SYMBOL) return;
  FExpr typ = fnext(it);
  IString namestr = fe(name)->ident;
  fe(name)->kind = FEXPR_SYMBOL;
  fe(name)->sym = alloc_FSymbol();
  fp(FSymbol, fe(name)->sym)->name = namestr;
  fp(FSymbol, fe(name)->sym)->istoplevel = fe(f)->istoplevel;
  boot_semantic(typ);
  add_decl((Decl){namestr, fe(name)->sym, fe(typ)->typsym});
  fnstacksize += get_type_size(fe(typ)->typsym);
  fe(f)->typ = type_void();
  boot_codegen(f);
}

void semantic_set(FExpr f) {
  fiter(it, fe(f)->sons);
  fnext(it);
  FExpr lvalue = fnext(it);
  FExpr rvalue = fnext(it);
  rewrite_to_lvalue(lvalue);
  boot_semantic(lvalue);
  boot_semantic(rvalue);
  if (fp(FType, fe(lvalue)->typ)->kind != FTYPE_PTR) error("lvalue should be ^ptr type: %s", fexpr_tostring(lvalue));
  if (!ftype_eq(fp(FType, fe(lvalue)->typ)->ptrof, fe(rvalue)->typ)) {
    error("type mismatch in `=");
  }
    
  if (is_structtype(fp(FType, fe(lvalue)->typ)->ptrof)) {
    fseq(copycall, fident("copy"), lvalue, new_fgetref(rvalue));
    *fe(f) = *fe(copycall);
    boot_semantic(f);
  }
  fe(f)->typ = type_void();
  
}

void semantic_X(FExpr f) {
  fiter(it, fe(f)->sons);
  fnext(it);
  boot_semantic(fnext(it));
  fe(f)->typ = type_void();
}

void semantic_sizeof(FExpr f) {
  fiter(it, fe(f)->sons);
  fnext(it);
  FExpr ftyp = fnext(it);
  boot_semantic(ftyp);
  if (fe(ftyp)->kind != FEXPR_SYMBOL) error("sizeof argument should be type");
  fe(f)->kind = FEXPR_INTLIT;
  fe(f)->intval = get_type_size(fe(ftyp)->typsym);
  boot_semantic(f);
}

void semantic_dot(FExpr f) {
  fiter(it, fe(f)->sons);
  fnext(it);
  FExpr lvalue = fnext(it);
  FExpr fieldname = fnext(it);
  boot_semantic(lvalue);
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
}

void semantic_getref(FExpr f) {
  fiter(it, fe(f)->sons);
  fnext(it);
  FExpr lvalue = fnext(it);
  boot_semantic(lvalue);
  // if (!is_lvalue(lvalue)) error("can't get address of expression, should be lvalue");
  fe(f)->typ = new_ftype(FTYPE_PTR);
  fp(FType, fe(f)->typ)->ptrof = fe(lvalue)->typ;
}

void semantic_if(FExpr f) {
  fiter(it, fe(f)->sons);
  fnext(it);
  FExpr cond = fnext(it);
  FExpr body = fnext(it);
  boot_semantic(cond);
  boot_semantic(body);
  if (!isfnil(it)) {
    fnext(it);
    FExpr elsebody = fnext(it);
    boot_semantic(elsebody);
  }
  fe(f)->typ = type_int(); // FIXME: if expression type
}

void semantic_while(FExpr f) {
  fiter(it, fe(f)->sons);
  fnext(it);
  FExpr cond = fnext(it);
  FExpr body = fnext(it);
  boot_semantic(cond);
  boot_semantic(body);
  fe(f)->typ = type_void();
}

//
// codegen
//

void codegen_fn(FExpr f) {
  fiter(cur, fe(f)->sons);
  fnext(cur);

  FExpr fnsym = fnext(cur);
  FExpr fnargs = fnext(cur);
  /* FExpr rettyp = */ fnext(cur);
  FExpr fnbody = fnext(cur);

  int fnidx = jit_getidx();
  fp(FSymbol, fe(fnsym)->sym)->fnidx = fnidx;
  curroffset = 0;
  gen_prologue(fp(FSymbol, fe(fnsym)->sym)->stacksize);
  int argoffset = 16;
  forlist (IListFExpr, FExpr, arg, IListFExpr_reverse(fe(fnargs)->sons)) {
    fp(FSymbol, fe(arg)->sym)->varoffset = -argoffset;
    argoffset += 8;
  }
  boot_codegen(fnbody);
  write_hex(0x58); // pop rax ; for return value
  gen_epilogue();
}

void codegen_var(FExpr f) {
  fiter(it, fe(f)->sons);
  fnext(it);
  FExpr name = fnext(it);
  FExpr typ = fnext(it);
  
  if (fe(f)->istoplevel) {
    size_t dataidx = data_alloc(get_type_size(fe(typ)->typsym));
    fp(FSymbol, fe(name)->sym)->vardataidx = dataidx;
  } else {
    curroffset += get_type_size(fe(typ)->typsym);
    int offset = curroffset;
    fp(FSymbol, fe(name)->sym)->varoffset = offset;
  }
}

void codegen_set(FExpr f) {
  fiter(it, fe(f)->sons);
  fnext(it);
  FExpr lvalue = fnext(it);
  FExpr rvalue = fnext(it);
  boot_codegen(lvalue);
  boot_codegen(rvalue);
  write_hex(0x59);// pop rcx
  write_hex(0x58); // pop rax
  write_hex(0x48, 0x89, 0x08); // mov [rax], rcx
}

void codegen_X(FExpr f) {
  fiter(it, fe(f)->sons);
  fnext(it);
  FExpr opcode = fnext(it);
  if (fe(opcode)->kind != FEXPR_INTLIT) error("expected int literal in X.");
  write_hex(fe(opcode)->intval);
}

void codegen_dot(FExpr f) {
  boot_codegen_lvalue(f);
  write_hex(
    0x58, // pop rax
    0x48, 0x8b, 0x00, // mov rax, [rax]
    0x50 // push rax
  );
}

void codegen_lvalue_dot(FExpr f) {
  fiter(it, fe(f)->sons);
  fnext(it);
  FExpr lvalue = fnext(it);
  FExpr fieldsym = fnext(it);
  if (fp(FType, fe(lvalue)->typ)->kind == FTYPE_PTR) {
    boot_codegen(lvalue);
  } else {
    boot_codegen_lvalue(lvalue);
  }
  write_hex(0x58); // pop rax
  write_hex(0x48, 0x05); // add rax, ..
  write_lendian(fp(FSymbol, fe(fieldsym)->sym)->varoffset);
  write_hex(0x50); // push rax
}

void codegen_getref(FExpr f) {
  fiter(it, fe(f)->sons);
  fnext(it);
  FExpr lvalue = fnext(it);
  boot_codegen_lvalue(lvalue);
}

void codegen_if(FExpr f) {
  fiter(it, fe(f)->sons);

  int relocnum = 0;
  int relocs[1024];
  int fixup = 0;
  while (!isfnil(it)) {
    FExpr cur = fnext(it);
    if (relocnum == 0 && cmp_ident(cur, "if")) {
      FExpr cond = fnext(it);
      boot_codegen(cond);

      // cond if branching (need fixup)
      write_hex(0x58); // pop rax
      write_hex(0x48, 0x83, 0xf8, 0x00); // cmp rax, 0
      write_hex(0x0f, 0x84); // je rel
      fixup = jit_getidx();
      write_lendian(0); // fixup

      // body codegen.
      FExpr body = fnext(it);
      boot_codegen(body);

      // jmp end of if after body.
      write_hex(0xe9);
      relocs[relocnum] = jit_getidx();
      relocnum++;
      write_lendian(0); // fixup
    } else if (cmp_ident(cur, "elif")) {
      int fixuprel = jit_getidx() - fixup - 4;
      jit_fixup_lendian(fixup, fixuprel);

      // cond codegen.
      FExpr cond = fnext(it);
      boot_codegen(cond);

      // cond if branching (need fixup)
      write_hex(0x58); // pop rax
      write_hex(0x48, 0x83, 0xf8, 0x00); // cmp rax, 0
      write_hex(0x0f, 0x84); // je rel
      fixup = jit_getidx();
      write_lendian(0); // fixup

      // body codegen.
      FExpr body = fnext(it);
      boot_codegen(body);

      // jmp end of if after body.
      write_hex(0xe9);
      relocs[relocnum] = jit_getidx();
      relocnum++;
      write_lendian(0); // fixup
    } else if (cmp_ident(cur, "else")) {
      int fixuprel = jit_getidx() - fixup - 4;
      jit_fixup_lendian(fixup, fixuprel);

      FExpr body = fnext(it);
      boot_codegen(body);

      // fixup relocations of if-expression.
      for (int i=0; i<relocnum; i++) {
        int fixuprel = jit_getidx() - relocs[i] - 4;
        jit_fixup_lendian(relocs[i], fixuprel);
      }
      return;
    } else {
      error("unexpected token in if expression.");
    }
  }
  
  int fixuprel = jit_getidx() - fixup - 4;
  jit_fixup_lendian(fixup, fixuprel);

  // fixup relocations of if-expression.
  for (int i=0; i<relocnum; i++) {
    int fixuprel = jit_getidx() - relocs[i] - 4;
    jit_fixup_lendian(relocs[i], fixuprel);
  }
}

void codegen_while(FExpr f) {
  fiter(it, fe(f)->sons);
  fnext(it);
  FExpr cond = fnext(it);
  FExpr body = fnext(it);
  int startL = jit_getidx();
  boot_codegen(cond);
    
  write_hex(0x58); // pop rax
  write_hex(0x48, 0x83, 0xf8, 0x00); // cmp rax, 0
  write_hex(0x0f, 0x84); // je rel
  int fixup = jit_getidx();
  write_lendian(0); // fixup
    
  boot_codegen(body);
  write_hex(0xe9); // jmp ..
  int jmprel = startL - jit_getidx() - 4;
  write_lendian(jmprel);
    
  int fixuprel = jit_getidx() - fixup - 4;
  jit_fixup_lendian(fixup, fixuprel);
}

//
// internal initializers
//

void boot_init() {
  declmap = nil_DeclMap();
  fndeclmap = nil_FnDeclMap();
  internaldeclmap = nil_InternalDeclMap();
}

void internal_print(size_t x) {
  printf("%zd", x);
}

size_t internal_fintlit(size_t x) {
  FExpr f = new_fexpr(FEXPR_INTLIT);
  fe(f)->intval = x;
  return f.index;
}

size_t internal_fident(char* s) {
  return fident(s).index;
}

size_t internal_fseq() {
  return new_fcontainer(FEXPR_SEQ).index;
}
size_t internal_flist() {
  return new_fcontainer(FEXPR_LIST).index;
}
size_t internal_fblock() {
  return new_fcontainer(FEXPR_BLOCK).index;
}

size_t internal_gensym() {
  return gen_tmpid().index;
}

void internal_fexpr_push(size_t fidx, size_t sonidx) {
  FExpr f = (FExpr){fidx};
  FExpr son = (FExpr){sonidx};
  push_son(f, son);
}

size_t internal_fexpr_dup(size_t f) {
  return copy_fexpr((FExpr){f}).index;
}
size_t internal_fexpr_ddup(size_t f) {
  return deepcopy_fexpr((FExpr){f}).index;
}

void internal_opcode(size_t op) {
  write_hex(op);
}

void internal_createdef(size_t fidx) {
  FExpr f = (FExpr){fidx};
  boot_semantic(f);
  fiter(it, fe(f)->sons);
  fnext(it);
  FExpr sym = fnext(it);
  fp(FSymbol, fe(sym)->sym)->fnidx = jit_getidx();
  *fe(f) = *fe(new_fcontainer(FEXPR_BLOCK));
}

size_t internal_fexpr_get(size_t fidx, size_t index) {
  FExpr f = (FExpr){fidx};
  fiter(it, fe(f)->sons);
  FExpr ret = fnext(it);
  for (int i=0; i<index; i++) {
    ret = fnext(it);
  }
  return ret.index;
}

size_t internal_fexpr_kind(size_t fidx) {
  FExpr f = (FExpr){fidx};
  return (size_t)fe(f)->kind;
}

size_t internal_fexpr_len(size_t fidx) {
  FExpr f = (FExpr){fidx};
  return IListFExpr_len(fe(f)->sons);
}

size_t internal_fexpr_to_cstring(size_t fidx) {
  FExpr f = (FExpr){fidx};
  return (size_t)fexpr_tostring(f);
}

void internal_fexpr_replace(size_t didx, size_t sidx) {
  FExpr d = (FExpr){didx};
  FExpr s = (FExpr){sidx};
  *fe(d) = *fe(s);
}

void internal_init_defs(FExpr f) {
  init_def("internal_print_ptr", internal_print);
  init_def("internal_fintlit_ptr", internal_fintlit);
  init_def("internal_fident_ptr", internal_fident);
  init_def("internal_fseq_ptr", internal_fseq);
  init_def("internal_flist_ptr", internal_flist);
  init_def("internal_fblock_ptr", internal_fblock);
  init_def("internal_gensym_ptr", internal_gensym);
  init_def("internal_fexpr_push_ptr", internal_fexpr_push);
  init_def("internal_fexpr_dup_ptr", internal_fexpr_dup);
  init_def("internal_fexpr_ddup_ptr", internal_fexpr_ddup);
  init_def("internal_opcode_ptr", internal_opcode);
  init_def("internal_createdef_ptr", internal_createdef);
  init_def("internal_fexpr_get_ptr", internal_fexpr_get);
  init_def("internal_fexpr_kind_ptr", internal_fexpr_kind);
  init_def("internal_fexpr_len_ptr", internal_fexpr_len);
  init_def("internal_fexpr_to_cstring_ptr", internal_fexpr_to_cstring);
  
  init_def("internal_fexpr_replace_ptr", internal_fexpr_replace);
}

void boot_def_internals() {
  init_internal("type_ptr", false, semantic_typeptr, NULL, NULL);
  init_internal("type", false, semantic_type, NULL, NULL);
  init_internal("defprimitive", true, semantic_defprimitive, NULL, NULL);
  init_internal("fn", true, semantic_fn, codegen_fn, NULL);
  init_internal("jit", true, semantic_jit, NULL, NULL);
  init_internal("struct", true, semantic_struct, NULL, NULL);
  init_internal("macro", true, semantic_macro, NULL, NULL);
  init_internal(":=", false, semantic_def, NULL, NULL);
  init_internal("var", false, semantic_var, codegen_var, NULL);
  init_internal("=", false, semantic_set, codegen_set, NULL);
  init_internal("X", false, semantic_X, codegen_X, NULL);
  init_internal("sizeof", false, semantic_sizeof, NULL, NULL);
  init_internal(".", false, semantic_dot, codegen_dot, codegen_lvalue_dot);
  init_internal("getref", false, semantic_getref, codegen_getref, NULL);
  init_internal("if", false, semantic_if, codegen_if, NULL);
  init_internal("while", false, semantic_while, codegen_while, NULL);
  init_internal("internal_init_defs", false, internal_init_defs, NULL, NULL);
}

int boot_call_main() {
  FnDecl fndecl;
  if (!search_fndecl(new_istring("main"), new_FTypeVec(), &fndecl)) {
    error("undefined reference to `main");
  }
  return ((int (*)())jit_toptr(fp(FSymbol, fndecl.sym)->fnidx))();
}
