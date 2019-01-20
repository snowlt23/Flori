#include "flori.h"
#include <string.h>
#include <inttypes.h>

// semantic globals
int tmpcnt = 0;
int fnstacksize;
int curroffset;

// codegen globals
#define write_hex2(id, ...) \
  uint8_t id[] = {__VA_ARGS__}; \
  jit_alloc_write(id, sizeof(id));
#define write_hex1(ln, ...) write_hex2(CONCAT(_hex, ln), __VA_ARGS__)
#define write_hex(...) write_hex1(__LINE__, __VA_ARGS__)

//
// utils
//

FMap gen_tmpid() {
  char buf[1024] = {};
  snprintf(buf, 1024, "_tmp%d", tmpcnt++);
  return fident(new_istring(buf));
}

FMap infix_left(FMap f) {
  return IListFMap_value(fm(fmap_cget(f, "args"))->lst);
}
FMap infix_right(FMap f) {
  return IListFMap_value(IListFMap_next(fm(fmap_cget(f, "args"))->lst));
}

bool is_toplevel(FMap f) {
  return !FMap_isnil(fmap_cget(f, "toplevel"));
}

//
// symbol & type
//

FSymbol new_symbol(IString name) {
  FSymbol s = alloc_FSymbol();
  fp(FSymbol, s)->name = name;
  return s;
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

FMap new_ftypesym(FType t) {
  FSymbol s = new_symbol(new_istring("type"));
  fp(FSymbol, s)->t = t;
  return fsymbol(s);
}

bool ftype_is(FType t, char* name) {
  if (fp(FType, t)->kind != FTYPE_PRIM && fp(FType, t)->kind != FTYPE_SYM) return false;
  return strcmp(istring_cstr(fp(FSymbol, fp(FType, t)->sym)->name), name) == 0;
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

FType get_ftype(FMap f) {
  assert(istring_ceq(fm(f)->kind, "fsymbol"));
  return fp(FSymbol, fm(f)->sym)->t;
}

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
// codegen utils
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

//
// primitive
//

void semantic_fintlit(FMap f) {
  fmap_cpush(f, "type", new_ftypesym(type_int()));
}

void codegen_fintlit(FMap f) {
  gen_push_int(fm(f)->intval);
}

void semantic_fident(FMap f) {
  Decl decl;
  if (!search_decl(fm(f)->ident, &decl)) error("undeclared %s ident", fmap_tostring(f));
  *fm(f) = *fm(fsymbol(decl.sym));
  fmap_cpush(f, "type", new_ftypesym(decl.typ));
}

void semantic_fsymbol(FMap f) {
}

void codegen_fsymbol(FMap f) {
  write_hex(0xff, 0xb5);
  write_lendian(-fp(FSymbol, fm(f)->sym)->varoffset);
}

void codegen_lvalue_fsymbol(FMap f) {
  write_hex(0x48, 0x8d, 0x85); // lea rax, [rbp-..]
  write_lendian(-fp(FSymbol, fm(f)->sym)->varoffset);
  write_hex(0x50); // push rax
}

void semantic_block(FMap f) {
  forlist (IListFMap, FMap, e, fm(f)->lst) {
    boot_semantic(e);
  }
}

void codegen_block(FMap f) {
  forlist (IListFMap, FMap, e, fm(f)->lst) {
    boot_codegen(e);
  }
}

void semantic_fn(FMap f) {
  FMap name = fmap_cget(f, "name");
  assert(eq_kind(name, FMAP_IDENT));
  FSymbol sym = new_symbol(fm(name)->ident);
  fp(FSymbol, sym)->f = f;
  FMap fsym = fsymbol(sym);
  fmap_cpush(f, "sym", fsym);
  
  FMap argdecls = fmap_cget(f, "argdecls");
  FMap rettype = fmap_cget(f, "returntype");
  FMap body = fmap_cget(f, "body");
  boot_semantic(rettype);

  fnstacksize = 0;

  IListFType argtypes = nil_IListFType();
  forlist(IListFMap, FMap, argdecl, fm(argdecls)->lst) {
    FMap name = fmap_cget(argdecl, "name");
    FMap type = fmap_cget(argdecl, "type");
    IString nameid = fm(name)->ident;
    boot_semantic(type);
    argtypes = new_IListFType(get_ftype(type), argtypes);
    FSymbol sym = new_symbol(fm(name)->ident);
    *fm(name) = *fm(fsymbol(sym));
    add_decl((Decl){nameid, sym, get_ftype(type)});
  }
  argtypes = IListFType_reverse(argtypes);
  
  add_fndecl((FnDecl){fm(name)->ident, argtypes, get_ftype(rettype), sym, NULL});
  
  bool isinline = !FMap_isnil(fmap_cget(f, "inline"));
  if (!isinline) {
    boot_semantic(body);
    fp(FSymbol, sym)->stacksize = fnstacksize;
  }
}

void codegen_fn(FMap f) {
  bool isinline = !FMap_isnil(fmap_cget(f, "inline"));
  if (isinline) return;
  
  FMap fnsym = fmap_cget(f, "sym");
  FMap argdecls = fmap_cget(f, "argdecls");
  FMap body = fmap_cget(f, "body");

  int fnidx = jit_getidx();
  fp(FSymbol, fm(fnsym)->sym)->fnidx = fnidx;
  curroffset = 0;
  gen_prologue(fp(FSymbol, fm(fnsym)->sym)->stacksize);

  int argoffset = 16;
  forlist(IListFMap, FMap, argdecl, fm(argdecls)->lst) {
    FMap name = fmap_cget(argdecl, "name");
    fp(FSymbol, fm(name)->sym)->varoffset = -argoffset;
    argoffset += 8;
  }
  
  boot_codegen(body);
  write_hex(0x58); // pop rax ; for return value
  gen_epilogue();
}

void semantic_type(FMap f) {
  FMap t = fmap_cget(f, "t");
  assert(eq_kind(t, FMAP_IDENT));
  Decl decl;
  if (search_decl(fm(t)->ident, &decl)) {
    if (fp(FSymbol, decl.sym)->isprim) {
      FType ft = new_ftype(FTYPE_PRIM);
      fp(FType, ft)->sym = decl.sym;
      *fm(f) = *fm(new_ftypesym(ft));
    } else {
      FType ft = new_ftype(FTYPE_SYM);
      fp(FType, ft)->sym = decl.sym;
      *fm(f) = *fm(new_ftypesym(ft));
    }
  } else {
    error("undeclared %s type", istring_cstr(fm(t)->ident));
  }
}

void semantic_defprimitive(FMap f) {
  FMap name = fmap_cget(f, "name");
  FMap size = fmap_cget(f, "size");
  if (!eq_kind(name, FMAP_IDENT)) error("defprimitive should be specify type-name");
  if (!eq_kind(size, FMAP_INTLIT)) error("defprimitive should be specify type-size");
  FSymbol sym = new_symbol(fm(name)->ident);
  fp(FSymbol, sym)->isprim = true;
  fp(FSymbol, sym)->name = fm(name)->ident;
  fp(FSymbol, sym)->size = fm(size)->intval;
  fmap_cpush(f, "sym", fsymbol(sym));
  add_decl((Decl){fm(name)->ident, sym});
}

void semantic_call(FMap f) {
  FMap call = fmap_cget(f, "call");
  FMap args = fmap_cget(f, "args");
  if (!eq_kind(call, FMAP_IDENT)) error("%s isn't function.", fmap_tostring(call));
  InternalDecl decl;
  FnDecl fndecl;
  if (search_internal_decl(fm(call)->ident, &decl)) {
    (decl.semanticfn)(f);
    return;
  }

  FTypeVec* argtypes = new_FTypeVec();
  forlist (IListFMap, FMap, arg, fm(args)->lst) {
    boot_semantic(arg);
    FTypeVec_push(argtypes, get_ftype(fmap_cget(arg, "type")));
  }
  if (search_fndecl(fm(call)->ident, argtypes, &fndecl)) {
    bool isinline = !FMap_isnil(fmap_cget(fp(FSymbol, fndecl.sym)->f, "inline"));
    *fm(call) = *fm(fsymbol(fndecl.sym));
    if (isinline) {
      FMap body = fmap_cget(fp(FSymbol, fndecl.sym)->f, "body");
      boot_semantic(body);
      FMap callf = copy_fmap(f);
      FMap blk = flist();
      fm(blk)->kind = new_istring("block");
      flist_push(blk, body);
      flist_push(blk, callf);
      *fm(f) = *fm(blk);
      fmap_cpush(f, "type", new_ftypesym(fndecl.returntype));
    } else {
      fmap_cpush(f, "type", new_ftypesym(fndecl.returntype));
    }
  } else {
    error("undeclared %s function", fmap_tostring(call));
  }
}

void codegen_call(FMap f) {
  FMap call = fmap_cget(f, "call");
  FMap args = fmap_cget(f, "args");
  InternalDecl decl;
  if (eq_kind(call, FMAP_IDENT) && search_internal_decl(fm(call)->ident, &decl)) {
    (decl.codegenfn)(f);
    return;
  }
  
  forlist (IListFMap, FMap, arg, fm(args)->lst) {
    boot_codegen(arg);
  }
  
  bool isinline = !FMap_isnil(fmap_cget(fp(FSymbol, fm(call)->sym)->f, "inline"));
  if (isinline) return;
  
  int callstacksize = IListFMap_len(fm(args)->lst)*8;
  int rel = fp(FSymbol, fm(call)->sym)->fnidx - jit_getidx() - 5;
  write_hex(0xE8); // call
  write_lendian(rel);
  write_hex(0x48, 0x81, 0xc4); // add rsp, ..
  write_lendian(callstacksize);
  if (!ftype_is(get_ftype(fmap_cget(f, "type")), "void")) {
    write_hex(0x50); // push rax
  }
}

void semantic_X(FMap f) {
  FMap x = IListFMap_value(fm(fmap_cget(f, "args"))->lst);
  if (!eq_kind(x, FMAP_INTLIT)) error("X argument should be fintlit");
  boot_semantic(x);
  fmap_cpush(f, "type", new_ftypesym(type_void()));
}

void codegen_X(FMap f) {
  FMap x = IListFMap_value(fm(fmap_cget(f, "args"))->lst);
  write_hex(fm(x)->intval);
}

void semantic_var(FMap f) {
  FMap name = fmap_cget(f, "name");
  FMap type = fmap_cget(f, "vartype");
  if (eq_kind(name, FMAP_SYMBOL)) return;
  boot_semantic(type);
  FSymbol sym = new_symbol(fm(name)->ident);
  add_decl((Decl){fm(name)->ident, sym, get_ftype(type)});
  fnstacksize += get_type_size(get_ftype(type));
  fmap_cpush(f, "sym", fsymbol(sym));
  fmap_cpush(f, "type", new_ftypesym(type_void()));
}

void codegen_var(FMap f) {
  // FMap name = fmap_cget(f, "name");
  FMap type = fmap_cget(f, "vartype");
  FMap sym = fmap_cget(f, "sym");
  curroffset += get_type_size(get_ftype(type));
  fp(FSymbol, fm(sym)->sym)->varoffset = curroffset;
}

void semantic_set(FMap f) {
  FMap left = infix_left(f);
  FMap right = infix_right(f);
  boot_semantic(left);
  boot_semantic(right);
  if (!ftype_eq(get_ftype(fmap_cget(left, "type")), get_ftype(fmap_cget(right, "type")))) error("type mismatch in `=");
  fmap_cpush(f, "type", new_ftypesym(type_void()));
}

void codegen_set(FMap f) {
  FMap left = infix_left(f);
  FMap right = infix_right(f);
  boot_codegen_lvalue(left);
  boot_codegen(right);
  write_hex(0x59);// pop rcx
  write_hex(0x58); // pop rax
  write_hex(0x48, 0x89, 0x08); // mov [rax], rcx
}

void semantic_def(FMap f) {
  FMap left = infix_left(f);
  FMap right = infix_right(f);
  boot_semantic(right);
  def_fmap(varf, var, {
      def_field(name, left);
      def_field(vartype, new_ftypesym(get_ftype(fmap_cget(right, "type"))));
    });
  fcallseq(initf, fident(new_istring("=")), copy_fmap(left), right);
  fblockseq(blk, varf, initf);
  boot_semantic(blk);
  *fm(f) = *fm(blk);
}

void semantic_if(FMap f) {
  FMap elifs = fmap_cget(f, "elifs");
  forlist (IListFMap, FMap, elif, fm(elifs)->lst) {
    FMap cond = fmap_cget(elif, "cond");
    FMap body = fmap_cget(elif, "body");
    boot_semantic(cond);
    boot_semantic(body);
  }
  boot_semantic(fmap_cget(f, "else"));
  fmap_cpush(f, "type", new_ftypesym(type_void()));
}

void codegen_if(FMap f) {
  int relocnum = 0;
  int relocs[1024];
  int fixup = -1;
  FMap elifs = fmap_cget(f, "elifs");
  forlist (IListFMap, FMap, elif, fm(elifs)->lst) {
    FMap cond = fmap_cget(elif, "cond");
    FMap body = fmap_cget(elif, "body");
    
    if (fixup != -1) {
      int fixuprel = jit_getidx() - fixup - 4;
      jit_fixup_lendian(fixup, fixuprel);
    }

    boot_codegen(cond);

    // cond if branching (need fixup)
    write_hex(0x58); // pop rax
    write_hex(0x48, 0x83, 0xf8, 0x00); // cmp rax, 0
    write_hex(0x0f, 0x84); // je rel
    fixup = jit_getidx();
    write_lendian(0); // fixup

    boot_codegen(body);

    // jmp end of if after body.
    write_hex(0xe9);
    relocs[relocnum] = jit_getidx();
    relocnum++;
    write_lendian(0); // fixup
  }

  // else codegen (with fixup elifs)
  {
    int fixuprel = jit_getidx() - fixup - 4;
    jit_fixup_lendian(fixup, fixuprel);

    FMap body = fmap_cget(f, "else");
    boot_codegen(body);

    // fixup relocations of if-expression.
    for (int i=0; i<relocnum; i++) {
      int fixuprel = jit_getidx() - relocs[i] - 4;
      jit_fixup_lendian(relocs[i], fixuprel);
    }
  }
}

void def_internal(char* name, void* semfn, void* genfn, void* lvaluefn) {
  IString nameid = new_istring(name);
  InternalDecl decl;
  decl.name = nameid;
  decl.semanticfn = semfn;
  decl.codegenfn = genfn;
  decl.lvaluegenfn = lvaluefn;
  add_internal_decl(decl);
}

void boot_init_internals() {
  def_internal("fintlit", semantic_fintlit, codegen_fintlit, NULL);
  def_internal("fident", semantic_fident, NULL, NULL);
  def_internal("fsymbol", semantic_fsymbol, codegen_fsymbol, codegen_lvalue_fsymbol);
  def_internal("block", semantic_block, codegen_block, NULL);
  def_internal("fn", semantic_fn, codegen_fn, NULL);
  def_internal("type", semantic_type, NULL, NULL);
  def_internal("defprimitive", semantic_defprimitive, NULL, NULL);
  def_internal("call", semantic_call, codegen_call, NULL);
  def_internal("X", semantic_X, codegen_X, NULL);
  def_internal("var", semantic_var, codegen_var, NULL);
  def_internal("=", semantic_set, codegen_set, NULL);
  def_internal(":=", semantic_def, NULL, NULL);
  def_internal("if", semantic_if, codegen_if, NULL);
}

//
// eval
//

void boot_semantic(FMap f) {
  IString kind = fm(f)->kind;
  InternalDecl decl;
  if (!search_internal_decl(kind, &decl)) error("unknown %s fmap kind: %s", istring_cstr(kind), fmap_tostring(f));
  (decl.semanticfn)(f);
}

void boot_codegen_lvalue(FMap f) {
  IString kind = fm(f)->kind;
  InternalDecl decl;
  if (!search_internal_decl(kind, &decl)) error("unknown %s fmap kind: %s", istring_cstr(kind), fmap_tostring(f));
  if (decl.lvaluegenfn == NULL) error("%s should be lvalue", fmap_tostring(f));
  (decl.lvaluegenfn)(f);
}

void boot_codegen(FMap f) {
  IString kind = fm(f)->kind;
  InternalDecl decl;
  if (!search_internal_decl(kind, &decl)) error("unknown %s fmap kind: %s", istring_cstr(kind), fmap_tostring(f));
  if (decl.codegenfn != NULL) (decl.codegenfn)(f);
}

void boot_eval_toplevel(FMap f) {
  // debug("%s", fmap_tostring(f));
  fmap_cpush(f, "toplevel", fintlit(1));
  boot_semantic(f);
  boot_codegen(f);
}

int boot_call_main() {
  FnDecl fndecl;
  if (!search_fndecl(new_istring("main"), new_FTypeVec(), &fndecl)) {
    error("undefined reference to `main");
  }
  return ((int (*)())jit_toptr(fp(FSymbol, fndecl.sym)->fnidx))();
}
