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

FMap call_firstarg(FMap f) {
  return IListFMap_value(fm(fmap_cget(f, "args"))->lst);
}

bool call_is(FMap f, char* s) {
  return istring_ceq(fm(f)->kind, "call") && istring_ceq(fm(fmap_cget(f, "call"))->ident, s);
}

bool is_codegen(FMap f) {
  return !FMap_isnil(fmap_cget(f, "codegen"));
}

bool is_toplevel(FMap f) {
  return !FMap_isnil(fmap_cget(f, "toplevel"));
}

bool is_evaluated(FMap f) {
  return !FMap_isnil(fmap_cget(f, "evaluated"));
}

bool is_ptrtype(FMap f) {
  return !FMap_isnil(fmap_cget(f, "ptr"));
}

//
// symbol & type
//

FSymbol new_symbol(IString name) {
  FSymbol s = alloc_FSymbol();
  fp(FSymbol, s)->name = name;
  fp(FSymbol, s)->t = nil_FType();
  fp(FSymbol, s)->internalptr = NULL;
  fp(FSymbol, s)->ismacro = false;
  fp(FSymbol, s)->istoplevel = false;
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

String* ftype_tostring(FType t) {
  if (fp(FType, t)->kind == FTYPE_PRIM || fp(FType, t)->kind == FTYPE_SYM) {
    return new_string_by(istring_cstr(fp(FSymbol, fp(FType, t)->sym)->name));
  } else if (fp(FType, t)->kind == FTYPE_PTR) {
    String* s = new_string_by("ptr ");
    string_push(s, ftype_tostring(fp(FType, t)->ptrof)->data);
    return s;
  } else {
    assert(false);
  }
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

FType type_fmap() {
  Decl decl;
  if (!search_decl(new_istring("fmap"), &decl)) error("undeclared fmap type, please import prelude");
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

void semantic_fstrlit(FMap f) {
  fmap_cpush(f, "type", new_ftypesym(type_cstring()));
}

void codegen_fstrlit(FMap f) {
  gen_cstring(istring_cstr(fm(f)->strval));
}

void semantic_fident(FMap f) {
  Decl decl;
  if (!search_decl(fm(f)->ident, &decl)) error("undeclared %s ident", fmap_repr(f)->data);
  *fm(f) = *fm(fsymbol(decl.sym));
  fmap_cpush(f, "type", new_ftypesym(decl.typ));
}

void semantic_fsymbol(FMap f) {
}

void codegen_fsymbol(FMap f) {
  if (fp(FSymbol, fm(f)->sym)->internalptr != NULL) { // internal ptr
    write_hex(0x48, 0xb8); // movabs rax, ..
    size_t jitidx = jit_getidx();
    write_hex(0, 0, 0, 0, 0, 0, 0, 0);
    write_hex(0x50); // push rax
    fixup_lendian64(jit_toptr(jitidx), (size_t)fp(FSymbol, fm(f)->sym)->internalptr);
  } else if (fp(FSymbol, fm(f)->sym)->istoplevel) { // toplevel var
    write_hex(0x48, 0xb8); // movabs rax, ..
    size_t jitidx = jit_getidx();
    write_hex(0, 0, 0, 0, 0, 0, 0, 0);
    write_hex(0xff, 0x30); // push [rax]
    fixup_lendian64(jit_toptr(jitidx), (size_t)data_toptr(fp(FSymbol, fm(f)->sym)->vardataidx));
    reloc_add_info(jitidx, fp(FSymbol, fm(f)->sym)->vardataidx);
  } else {
    write_hex(0xff, 0xb5);
    write_lendian(-fp(FSymbol, fm(f)->sym)->varoffset);
  }
}

void semantic_flist(FMap f) {
  forlist (IListFMap, FMap, e, fm(f)->lst) {
    boot_semantic(e);
  }
  fmap_cpush(f, "type", fmap_cget(IListFMap_value(fm(f)->lst), "type"));
}

void codegen_flist(FMap f) {
  forlist (IListFMap, FMap, e, fm(f)->lst) {
    boot_codegen(e);
  }
}

void semantic_block(FMap f) {
  if (is_toplevel(f)) {
    forlist (IListFMap, FMap, e, fm(f)->lst) {
      fmap_cpush(e, "toplevel", fintlit(1));
      boot_semantic(e);
      boot_codegen(e);
    }
  } else {
    forlist (IListFMap, FMap, e, fm(f)->lst) {
      boot_semantic(e);
    }
  }
  if (IListFMap_len(fm(f)->lst) == 0) {
    fmap_cpush(f, "type", new_ftypesym(type_void()));
  } else {
    fmap_cpush(f, "type", fmap_cget(IListFMap_last(fm(f)->lst), "type"));
  }
}

void codegen_block(FMap f) {
  if (is_toplevel(f)) return;
  forlist (IListFMap, FMap, e, fm(f)->lst) {
    boot_codegen(e);
  }
}

void semantic_fn(FMap f) {
  get_field(name, f, "fn");
  assert(eq_kind(name, FMAP_IDENT));
  FSymbol sym = new_symbol(fm(name)->ident);
  fp(FSymbol, sym)->f = f;
  FMap fsym = fsymbol(sym);
  fmap_cpush(f, "sym", fsym);
  
  get_field(argdecls, f, "fn");
  get_field(returntype, f, "fn");
  get_field(body, f, "fn");
  boot_semantic(returntype);

  fnstacksize = 0;

  DeclMap localcp = get_local_checkpoint();

  IListFType argtypes = nil_IListFType();
  forlist(IListFMap, FMap, argdecl, fm(argdecls)->lst) {
    get_field(name, argdecl, "fn");
    get_field(type, argdecl, "fn");
    IString nameid = fm(name)->ident;
    boot_semantic(type);
    argtypes = new_IListFType(get_ftype(type), argtypes);
    FSymbol sym = new_symbol(fm(name)->ident);
    *fm(name) = *fm(fsymbol(sym));
    add_local((Decl){nameid, sym, get_ftype(type)});
  }
  argtypes = IListFType_reverse(argtypes);
  
  add_fndecl((FnDecl){fm(name)->ident, argtypes, get_ftype(returntype), sym, NULL});
  
  bool isinline = !FMap_isnil(fmap_cget(f, "inline"));
  if (!isinline) {
    boot_semantic(body);
    fp(FSymbol, sym)->stacksize = fnstacksize;
  }

  rollback_local(localcp);
  
  fmap_cpush(f, "type", new_ftypesym(type_void()));
}

void codegen_fn(FMap f) {
  bool isinline = !FMap_isnil(fmap_cget(f, "inline"));
  if (isinline) return;
  
  get_field(sym, f, "fn");
  get_field(argdecls, f, "fn");
  get_field(body, f, "fn");

  int fnidx = jit_getidx();
  fp(FSymbol, fm(sym)->sym)->fnidx = fnidx;
  curroffset = 0;
  gen_prologue(fp(FSymbol, fm(sym)->sym)->stacksize);

  int argoffset = 16;
  forlist(IListFMap, FMap, argdecl, fm(flist_reverse(argdecls))->lst) {
    FMap name = fmap_cget(argdecl, "name");
    fp(FSymbol, fm(name)->sym)->varoffset = -argoffset;
    argoffset += 8;
  }
  
  boot_codegen(body);
  write_hex(0x58); // pop rax ; for return value
  gen_epilogue();
}

void semantic_macro(FMap f) {
  fm(f)->kind = new_istring("fn");
  boot_semantic(f);
  get_field(sym, f, "macro");
  fp(FSymbol, fm(sym)->sym)->ismacro = true;
}

void semantic_syntax(FMap f) {
  fm(f)->kind = new_istring("fn");
  boot_semantic(f);
  get_field(name, f, "syntax");
  get_field(sym, f, "syntax");
  add_parser_decl((ParserDecl){fm(name)->ident, NULL, fm(sym)->sym});
}

void semantic_type(FMap f) {
  get_field(t, f, "type");

  if (is_ptrtype(f)) {
    boot_semantic(t);
    FType pt = new_ftype(FTYPE_PTR);
    fp(FType, pt)->ptrof = get_ftype(t);
    *fm(f) = *fm(new_ftypesym(pt));
    return;
  }
  
  Decl decl;
  if (!search_decl(fm(t)->ident, &decl)) error("undeclared %s type", istring_cstr(fm(t)->ident));

  FType ft;
  if (fp(FSymbol, decl.sym)->isprim) {
    ft = new_ftype(FTYPE_PRIM);
    fp(FType, ft)->sym = decl.sym;
  } else {
    ft = new_ftype(FTYPE_SYM);
    fp(FType, ft)->sym = decl.sym;
  }
  *fm(f) = *fm(new_ftypesym(ft));
}

void semantic_defprimitive(FMap f) {
  get_field(name, f, "defprimitive");
  get_field(size, f, "defprimitive");
  if (!eq_kind(name, FMAP_IDENT)) error("defprimitive should be specify type-name");
  if (!eq_kind(size, FMAP_INTLIT)) error("defprimitive should be specify type-size");
  FSymbol sym = new_symbol(fm(name)->ident);
  fp(FSymbol, sym)->isprim = true;
  fp(FSymbol, sym)->name = fm(name)->ident;
  fp(FSymbol, sym)->size = fm(size)->intval;
  fmap_cpush(f, "sym", fsymbol(sym));
  add_decl((Decl){fm(name)->ident, sym, nil_FType()});
}

FTypeVec* gen_fmap_argtypes(int n) {
  FTypeVec* v = new_FTypeVec();
  FType fmaptyp = type_fmap();
  for (int i=0; i<n; i++) {
    FTypeVec_push(v, fmaptyp);
  }
  return v;
}

void semantic_call(FMap f) {
  FMap call = fmap_cget(f, "call");
  FMap args = fmap_cget(f, "args");
  if (!eq_kind(call, FMAP_IDENT) && !eq_kind(call, FMAP_SYMBOL)) error("%s isn't function.", fmap_repr(call)->data);
  InternalDecl decl;
  FnDecl fndecl;
  if (search_internal_decl(fm(call)->ident, &decl) && decl.isfn) {
    (decl.semanticfn)(f);
    return;
  }

  if (search_fndecl(fm(call)->ident, gen_fmap_argtypes(IListFMap_len(fm(args)->lst)), &fndecl) && fp(FSymbol, fndecl.sym)->ismacro) {
    if (is_codegen(fp(FSymbol, fndecl.sym)->f)) {
      *fm(call) = *fm(fsymbol(fndecl.sym));
      fmap_cpush(f, "type", fmap_cget(fp(FSymbol, fndecl.sym)->f, "codegen_type"));
      return;
    }
    
    FMap expanded = call_macro(fndecl.sym, fm(args)->lst);
    if (is_toplevel(f)) {
      fmap_cpush(expanded, "toplevel", fintlit(1));
    }
    *fm(f) = *fm(expanded);
    boot_semantic(f);
    return;
  }

  FTypeVec* argtypes = new_FTypeVec();
  forlist (IListFMap, FMap, arg, fm(args)->lst) {
    boot_semantic(arg);
    FTypeVec_push(argtypes, get_ftype(fmap_cget(arg, "type")));
  }
  if (eq_kind(call, FMAP_SYMBOL)) return;
  if (search_fndecl(fm(call)->ident, argtypes, &fndecl)) {
    bool isinline = !FMap_isnil(fmap_cget(fp(FSymbol, fndecl.sym)->f, "inline"));
    *fm(call) = *fm(fsymbol(fndecl.sym));
    if (isinline) {
      FMap body = deepcopy_fmap(fmap_cget(fp(FSymbol, fndecl.sym)->f, "body"));
      boot_semantic(body);
      FMap callf = copy_fmap(f);
      fmap_cpush(callf, "type", new_ftypesym(fndecl.returntype));
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
    error("undeclared %s function: %s", fmap_repr(call)->data, fmap_repr(f)->data);
  }
}

void codegen_call(FMap f) {
  get_field(call, f, "call");
  get_field(args, f, "call");
  InternalDecl decl;
  if (eq_kind(call, FMAP_IDENT) && search_internal_decl(fm(call)->ident, &decl) && decl.isfn) {
    if (decl.codegenfn != NULL) (decl.codegenfn)(f);
    return;
  }

  if (fp(FSymbol, fm(call)->sym)->ismacro) {
    FMap expanded = call_macro(fm(call)->sym, fm(args)->lst);
    if (is_toplevel(f)) {
      fmap_cpush(expanded, "toplevel", fintlit(1));
    }
    *fm(f) = *fm(expanded);
    boot_semantic(f);
    boot_codegen(f);
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
  FMap x = call_firstarg(f);
  if (!eq_kind(x, FMAP_INTLIT)) error("X argument should be fintlit");
  boot_semantic(x);
  fmap_cpush(f, "type", new_ftypesym(type_void()));
}

void codegen_X(FMap f) {
  FMap x = call_firstarg(f);
  write_hex(fm(x)->intval);
}

void semantic_var(FMap f) {
  get_field(name, f, "var");
  get_field(vartype, f, "var");
  if (eq_kind(name, FMAP_SYMBOL)) return;
  boot_semantic(vartype);
  FSymbol sym = new_symbol(fm(name)->ident);
  if (is_toplevel(f)) {
    fp(FSymbol, sym)->istoplevel = true;
  }
  add_local((Decl){fm(name)->ident, sym, get_ftype(vartype)});
  fnstacksize += get_type_size(get_ftype(vartype));
  fmap_cpush(f, "sym", fsymbol(sym));
  fmap_cpush(f, "type", new_ftypesym(type_void()));
}

void codegen_var(FMap f) {
  // FMap name = fmap_cget(f, "name");
  FMap type = fmap_cget(f, "vartype");
  FMap sym = fmap_cget(f, "sym");
  if (is_toplevel(f)) {
    size_t dataidx = data_alloc(get_type_size(get_ftype(type)));
    fp(FSymbol, fm(sym)->sym)->vardataidx = dataidx;
  } else {
    curroffset += get_type_size(get_ftype(type));
    fp(FSymbol, fm(sym)->sym)->varoffset = curroffset;
  }
}

FMap fmap_lvaluegen(FMap f) {
  if (call_is(f, ".")) {
    return fcall(fident(new_istring(".lvalue")), fmap_cget(f, "args"));
  } else if (istring_ceq(fm(f)->kind, "call")) {
    FMap c = fmap_cget(f, "call");
    char idbuf[1024] = {};
    snprintf(idbuf, 1024, "%slvalue", istring_cstr(fm(c)->ident));
    fm(c)->ident = new_istring(idbuf);
    return f;
  } else {
    return fprefix(fident(new_istring("&")), deepcopy_fmap(f));
  }
}

void semantic_set(FMap f) {
  FMap left = infix_left(f);
  FMap right = infix_right(f);
  if (call_is(left, ".")) {
    FMap addrf = fcall(fident(new_istring(".lvalue")), fmap_cget(left, "args"));
    flistseq(args, addrf, right);
    *fm(f) = *fm(fcall(fident(new_istring("*=")), args));
  } else if (istring_ceq(fm(left)->kind, "call")) {
    FMap c = fmap_cget(left, "call");
    FMap args = fmap_cget(left, "args");
    char idbuf[1024] = {};
    snprintf(idbuf, 1024, "%s=", istring_cstr(fm(c)->ident));
    fm(c)->ident = new_istring(idbuf);
    *fm(args) = *fm(flist_reverse(args));
    flist_push(args, right);
    *fm(args) = *fm(flist_reverse(args));
    *fm(f) = *fm(left);
  } else {
    FMap addrf = fprefix(fident(new_istring("&")), deepcopy_fmap(left));
    flistseq(args, addrf, right);
    *fm(f) = *fm(fcall(fident(new_istring("*=")), args));
  }
  boot_semantic(f);
}

void codegen_set(FMap f) {
  assert(false);
  FMap left = infix_left(f);
  FMap right = infix_right(f);
  boot_codegen(left);
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
  if (is_toplevel(f)) {
    flistseq(args, initf);
    initf = fcall(fident(new_istring("static_macro")), args);
  }
  fblockseq(blk, varf, initf);
  if (is_toplevel(f)) {
    fmap_cpush(blk, "toplevel", fintlit(1));
  }
  boot_semantic(blk);
  *fm(f) = *fm(blk);
}

void semantic_if(FMap f) {
  get_field(elifs, f, "if");
  forlist (IListFMap, FMap, elif, fm(elifs)->lst) {
    get_field(cond, elif, "if");
    get_field(body, elif, "if");
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
  get_field(elifs, f, "if");
  forlist (IListFMap, FMap, elif, fm(elifs)->lst) {
    get_field(cond, elif, "if");
    get_field(body, elif, "if");
    
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

void semantic_while(FMap f) {
  get_field(cond, f, "while");
  get_field(body, f, "while");
  boot_semantic(cond);
  boot_semantic(body);
  fmap_cpush(f, "type", new_ftypesym(type_void()));
}

void codegen_while(FMap f) {
  get_field(cond, f, "while");
  get_field(body, f, "while");

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

void semantic_addr(FMap f) {
  FMap lvalue = call_firstarg(f);
  boot_semantic(lvalue);
  FType pt = new_ftype(FTYPE_PTR);
  fp(FType, pt)->ptrof = get_ftype(fmap_cget(lvalue, "type"));
  fmap_cpush(f, "type", new_ftypesym(pt));
}

void codegen_addr(FMap f) {
  FMap lvalue = call_firstarg(f);
  if (eq_kind(lvalue, FMAP_SYMBOL)) {
    if (fp(FSymbol, fm(lvalue)->sym)->istoplevel) {
      write_hex(0x48, 0xb8); // movabs rax, ..
      size_t jitidx = jit_getidx();
      write_hex(0, 0, 0, 0, 0, 0, 0, 0);
      write_hex(0x50); // push rax
      fixup_lendian64(jit_toptr(jitidx), (size_t)data_toptr(fp(FSymbol, fm(lvalue)->sym)->vardataidx));
      reloc_add_info(jitidx, fp(FSymbol, fm(lvalue)->sym)->vardataidx);
    } else {
      write_hex(0x48, 0x8d, 0x85); // lea rax, [rbp-..]
      write_lendian(-fp(FSymbol, fm(lvalue)->sym)->varoffset);
      write_hex(0x50); // push rax
    }
  } else if (call_is(lvalue, ".")) {
    boot_codegen(infix_left(lvalue));
    write_hex(0x58); // pop rax
    write_hex(0x48, 0x05); // add rax, ..
    write_lendian(fp(FSymbol, fm(fmap_cget(lvalue, "sym"))->sym)->varoffset);
    write_hex(0x50); // push rax
  } else {
    error("%s should be lvalue in &", fmap_repr(lvalue)->data);
  }
}

void decide_struct_size(FSymbol sym, FMap fields) {
  int curoffset = 0;
  forlist (IListFMap, FMap, field, fm(fields)->lst) {
    FMap fieldsym = fmap_cget(field, "sym");
    FMap fieldtype = fmap_cget(field, "type");
    fp(FSymbol, fm(fieldsym)->sym)->varoffset = curoffset;
    curoffset += get_type_size(get_ftype(fieldtype));
  }
  fp(FSymbol, sym)->size = curoffset;
}

void semantic_struct(FMap f) {
  FMap name = fmap_cget(f, "name");
  FMap fields = fmap_cget(f, "fields");
  FSymbol sym = new_symbol(fm(name)->ident);
  fmap_cpush(f, "sym", fsymbol(sym));
  fp(FSymbol, sym)->f = f;
  add_decl((Decl){fm(name)->ident, sym, nil_FType()});
  forlist (IListFMap, FMap, field, fm(fields)->lst) {
    FMap fieldname = fmap_cget(field, "name");
    FMap fieldtype = fmap_cget(field, "type");
    FSymbol fieldsym = new_symbol(fm(fieldname)->ident);
    fmap_cpush(field, "sym", fsymbol(fieldsym));
    fp(FSymbol, fieldsym)->f = field;
    boot_semantic(fieldtype);
  }
  decide_struct_size(sym, fields);
}

void semantic_sizeof(FMap f) {
  FMap t = call_firstarg(f);
  boot_semantic(t);
  *fm(f) = *fm(fintlit(get_type_size(get_ftype(t))));
  boot_semantic(f);
}

bool search_field(FSymbol structsym, IString name, FMap* retfield) {
  FMap fields = fmap_cget(fp(FSymbol, structsym)->f, "fields");
  forlist (IListFMap, FMap, field, fm(fields)->lst) {
    FMap fieldsym = fmap_cget(field, "sym");
    if (istring_eq(fp(FSymbol, fm(fieldsym)->sym)->name, name)) {
      *retfield = field;
      return true;
    }
  }
  return false;
}

void semantic_field(FMap f) {
  FMap left = infix_left(f);
  FMap right = infix_right(f);
  boot_semantic(left);
  FType lefttype = get_ftype(fmap_cget(left, "type"));
  if (fp(FType, lefttype)->kind == FTYPE_PTR) {
    lefttype = fp(FType, lefttype)->ptrof;
  } else {
    *fm(left) = *fm(fmap_lvaluegen(left));
    boot_semantic(left);
  }
  FMap field;
  if (!search_field(fp(FType, lefttype)->sym, fm(right)->ident, &field)) error("undeclared %s field in %s struct", fmap_repr(right)->data, ftype_tostring(lefttype)->data);
  fmap_cpush(f, "type", fmap_cget(field, "type"));
  fmap_cpush(f, "sym", fmap_cget(field, "sym"));
  fmap_cpush(f, "evaluated", fintlit(1));
  FMap addrf = fprefix(fident(new_istring("&")), deepcopy_fmap(f));
  FMap deref = fprefix(fident(new_istring("*")), addrf);
  *fm(f) = *fm(deref);
  boot_semantic(f);
}

void semantic_field_lvalue(FMap f) {
  fm(fmap_cget(f, "call"))->ident = new_istring(".");
  FMap left = infix_left(f);
  FMap right = infix_right(f);
  boot_semantic(left);
  FType lefttype = get_ftype(fmap_cget(left, "type"));
  if (fp(FType, lefttype)->kind == FTYPE_PTR) {
    lefttype = fp(FType, lefttype)->ptrof;
  } else {
    *fm(left) = *fm(fmap_lvaluegen(left));
    boot_semantic(left);
  }
  FMap field;
  if (!search_field(fp(FType, lefttype)->sym, fm(right)->ident, &field)) error("undeclared %s field in %s struct", fmap_repr(right)->data, ftype_tostring(lefttype)->data);
  fmap_cpush(f, "type", fmap_cget(field, "type"));
  fmap_cpush(f, "sym", fmap_cget(field, "sym"));
  fmap_cpush(f, "evaluated", fintlit(1));
  FMap addrf = fprefix(fident(new_istring("&")), deepcopy_fmap(f));
  *fm(f) = *fm(addrf);
  boot_semantic(f);
}

//
// internals
//

void internal_print(size_t x) {
  printf("%zd", x);
}

size_t internal_new_fmap(char* kind) {
  FMap f = fmap();
  fm(f)->kind = new_istring(kind);
  return f.index;
}
size_t internal_fmap() {
  return fmap().index;
}
size_t internal_new_flist(char* kind) {
  FMap f = flist();
  fm(f)->kind = new_istring(kind);
  return f.index;
}
size_t internal_flist() {
  return flist().index;
}
size_t internal_fident(char* s) {
  return fident(new_istring(s)).index;
}
size_t internal_fintlit(size_t x) {
  return fintlit(x).index;
}
size_t internal_fstrlit(char* s) {
  return fstrlit(new_istring(s)).index;
}

size_t internal_gensym() {
  return gen_tmpid().index;
}

size_t internal_intval(size_t fidx) {
  FMap f = (FMap){fidx};
  return fm(f)->intval;
}

void internal_fmap_set(size_t fidx, char* key, size_t vidx) {
  FMap f = (FMap){fidx};
  FMap value = (FMap){vidx};
  fmap_cpush(f, key, value);
}
size_t internal_fmap_get(size_t fidx, char* key) {
  FMap f = (FMap){fidx};
  return fmap_cget(f, key).index;
}

size_t internal_fmap_dup(size_t fidx) {
  FMap f = (FMap){fidx};
  return deepcopy_fmap(f).index;
}

char* internal_fmap_kind(size_t fidx) {
  FMap f = (FMap){fidx};
  return istring_cstr(fm(f)->kind);
}
char* internal_fmap_rootkind(size_t fidx) {
  FMap f = (FMap){fidx};
  return istring_cstr(fm(f)->parentkind);
}

void internal_flist_push(size_t fidx, size_t vidx) {
  FMap f = (FMap){fidx};
  FMap value = (FMap){vidx};
  flist_push(f, value);
}
size_t internal_flist_len(size_t fidx) {
  FMap f = (FMap){fidx};
  return IListFMap_len(fm(f)->lst);
}
size_t internal_flist_get(size_t fidx, size_t idx) {
  FMap f = (FMap){fidx};
  IListFMap it = fm(f)->lst;
  for (int i=0; i<idx; i++) it = IListFMap_next(it);
  return IListFMap_value(it).index;
}

void internal_fmap_replace(size_t idxa, size_t idxb) {
  FMap a = (FMap){idxa};
  FMap b = (FMap){idxb};
  *fm(a) = *fm(b);
}

char* internal_fmap_to_cstring(size_t fidx) {
  FMap f = (FMap){fidx};
  return fmap_tostring(f)->data;
}

size_t internal_fmap_to_flist(size_t fidx) {
  FMap f = (FMap){fidx};
  FMap retf = flist();
  forlist (IListField, Field, field, fm(f)->fields) {
    def_fmap(ff, field, {
        def_field(key, fident(field.key));
        def_field(value, field.value);
      });
    flist_push(retf, ff);
  }
  return retf.index;
}

void internal_semeval(size_t fidx) {
  FMap f = (FMap){fidx};
  boot_semantic(f);
}

size_t internal_parse() {
  return parse(gstrm).index;
}

size_t internal_parse_until(char c) {
  char buf[1024*10] = {};
  int bufpos = 0;
  while (true) {
    char n = stream_next(gstrm);
    if (n == c) {
      break;
    }
    buf[bufpos] = n;
    bufpos++;
  }
  return parse(new_stream(strdup(buf))).index;
}

size_t internal_parse_cstring(char* s) {
  return parse(new_stream(s)).index;
}

char internal_read_char() {
  return stream_next(gstrm);
}

void* internal_codeptr() {
  return jit_codeptr();
}

size_t internal_codeidx() {
  return jit_getidx();
}

void internal_inject_fnaddr(size_t fidx, size_t newaddr) {
  FMap f = (FMap){fidx};
  fp(FSymbol, fm(f)->sym)->fnidx = newaddr;
}

void internal_opcode(size_t op) {
  write_hex(op);
}

void def_internal_ptr(char* name, void* p) {
  IString nameid = new_istring(name);
  FSymbol sym = new_symbol(nameid);
  fp(FSymbol, sym)->internalptr = p;
  add_decl((Decl){nameid, sym, type_pointer()});
}

void internal_init_defs(FMap f) {
  def_internal_ptr("internal_print_ptr", internal_print);
  def_internal_ptr("internal_new_fmap_ptr", internal_new_fmap);
  def_internal_ptr("internal_fmap_ptr", internal_fmap);
  def_internal_ptr("internal_new_flist_ptr", internal_new_flist);
  def_internal_ptr("internal_flist_ptr", internal_flist);
  def_internal_ptr("internal_fident_ptr", internal_fident);
  def_internal_ptr("internal_fintlit_ptr", internal_fintlit);
  def_internal_ptr("internal_fstrlit_ptr", internal_fstrlit);
  def_internal_ptr("internal_gensym_ptr", internal_gensym);
  def_internal_ptr("internal_intval_ptr", internal_intval);
  def_internal_ptr("internal_fmap_set_ptr", internal_fmap_set);
  def_internal_ptr("internal_fmap_get_ptr", internal_fmap_get);
  def_internal_ptr("internal_fmap_dup_ptr", internal_fmap_dup);
  def_internal_ptr("internal_fmap_kind_ptr", internal_fmap_kind);
  def_internal_ptr("internal_fmap_rootkind_ptr", internal_fmap_rootkind);
  def_internal_ptr("internal_flist_push_ptr", internal_flist_push);
  def_internal_ptr("internal_flist_len_ptr", internal_flist_len);
  def_internal_ptr("internal_flist_get_ptr", internal_flist_get);
  def_internal_ptr("internal_fmap_replace_ptr", internal_fmap_replace);
  def_internal_ptr("internal_fmap_to_cstring_ptr", internal_fmap_to_cstring);
  def_internal_ptr("internal_fmap_to_flist_ptr", internal_fmap_to_flist);
  def_internal_ptr("internal_semeval_ptr", internal_semeval);
  def_internal_ptr("internal_parse_ptr", internal_parse);
  def_internal_ptr("internal_parse_until_ptr", internal_parse_until);
  def_internal_ptr("internal_parse_cstring_ptr", internal_parse_cstring);
  def_internal_ptr("internal_read_char_ptr", internal_read_char);
  def_internal_ptr("internal_codeptr_ptr", internal_codeptr);
  def_internal_ptr("internal_codeidx_ptr", internal_codeidx);
  def_internal_ptr("internal_inject_fnaddr_ptr", internal_inject_fnaddr);
  def_internal_ptr("internal_opcode_ptr", internal_opcode);
}

void def_internal(char* name, bool isfn, void* semfn, void* genfn) {
  IString nameid = new_istring(name);
  InternalDecl decl;
  decl.name = nameid;
  decl.isfn = isfn;
  decl.semanticfn = semfn;
  decl.codegenfn = genfn;
  add_internal_decl(decl);
}

void boot_init_internals() {
  def_internal("fintlit", false, semantic_fintlit, codegen_fintlit);
  def_internal("fstrlit", false, semantic_fstrlit, codegen_fstrlit);
  def_internal("fident", false, semantic_fident, NULL);
  def_internal("fsymbol", false, semantic_fsymbol, codegen_fsymbol);
  def_internal("flist", false, semantic_flist, codegen_flist);
  def_internal("block", false, semantic_block, codegen_block);
  def_internal("fn", false, semantic_fn, codegen_fn);
  def_internal("macro", false, semantic_macro, NULL);
  def_internal("syntax", false, semantic_syntax, NULL);
  def_internal("type", false, semantic_type, NULL);
  def_internal("defprimitive", false, semantic_defprimitive, NULL);
  def_internal("call", false, semantic_call, codegen_call);
  def_internal("X", true, semantic_X, codegen_X);
  def_internal("var", false, semantic_var, codegen_var);
  def_internal("=", true, semantic_set, codegen_set);
  def_internal(":=", true, semantic_def, NULL);
  def_internal("if", false, semantic_if, codegen_if);
  def_internal("while", false, semantic_while, codegen_while);
  def_internal("&", true, semantic_addr, codegen_addr);
  def_internal("struct", false, semantic_struct, NULL);
  def_internal("sizeof", true, semantic_sizeof, NULL);
  def_internal(".", true, semantic_field, NULL);
  def_internal(".lvalue", true, semantic_field_lvalue, NULL);
  def_internal("internal_init_defs", true, internal_init_defs, NULL);
}

//
// eval
//

void boot_semantic(FMap f) {
  if (is_evaluated(f)) return;
  IString kind = fm(f)->kind;
  InternalDecl decl;
  if (!search_internal_decl(kind, &decl)) error("unknown %s fmap kind: %s", istring_cstr(kind), fmap_tostring(f)->data);
  (decl.semanticfn)(f);
  fmap_cpush(f, "evaluated", fintlit(1));
}

void boot_codegen(FMap f) {
  IString kind = fm(f)->kind;
  InternalDecl decl;
  if (!search_internal_decl(kind, &decl)) error("unknown %s fmap kind: %s", istring_cstr(kind), fmap_tostring(f)->data);
  if (decl.codegenfn != NULL) (decl.codegenfn)(f);
}

void boot_eval_toplevel(FMap f) {
  // debug("%s", fmap_tostring(f)->data);
  // debug("%s", fmap_repr(f)->data);
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
