#include "flori.h"
#include <string.h>

#define emit_asm(...) { printf("  "); printf(__VA_ARGS__); printf("\n"); }

#define write_hex2(id, ...) \
  uint8_t id[] = {__VA_ARGS__}; \
  jit_alloc_write(id, sizeof(id));
#define write_hex1(ln, ...) write_hex2(CONCAT(_hex, ln), __VA_ARGS__)
#define write_hex(...) write_hex1(__LINE__, __VA_ARGS__)

int curroffset;
IListFnInfo fnmap;
IListJitInfo jitmap;
IListVarInfo varmap;

void codegen_init() {
  fnmap = nil_IListFnInfo();
  jitmap = nil_IListJitInfo();
  varmap = nil_IListVarInfo();
}

//
// FnInfo
//

void add_fninfo(IString key, int idx) {
  fnmap = new_IListFnInfo((FnInfo){key, idx}, fnmap);
}

FnInfo fninfo_nil() {
  FnInfo info;
  info.index = -1;
  return info;
}

bool fninfo_isnil(FnInfo info) {
  return info.index == -1;
}

FnInfo search_fn(char* name) {
  forlist (FnInfo, info, fnmap) {
    if (strcmp(istring_cstr(info.key), name) == 0) {
      return info;
    }
  }
  return fninfo_nil();
}

//
// JitInfo
//

void add_jitinfo(IString key, FExpr f) {
  jitmap = new_IListJitInfo((JitInfo){key, f}, jitmap);
}

JitInfo jitinfo_nil() {
  JitInfo info;
  info.body.index = -1;
  return info;
}

bool jitinfo_isnil(JitInfo info) {
  return info.body.index == -1;
}

JitInfo search_jit(char* name) {
  forlist (JitInfo, info, jitmap) {
    if (strcmp(istring_cstr(info.key), name) == 0) {
      return info;
    }
  }
  return jitinfo_nil();
}

//
// VarInfo
//

void add_varinfo(IString key, int offset) {
  varmap = new_IListVarInfo((VarInfo){key, offset}, varmap);
}

VarInfo varinfo_nil() {
  VarInfo info;
  info.offset= -1;
  return info;
}

bool varinfo_isnil(VarInfo info) {
  return info.offset == -1;
}

VarInfo search_var(char* name) {
  forlist (VarInfo, info, varmap) {
    if (strcmp(istring_cstr(info.key), name) == 0) {
      return info;
    }
  }
  return varinfo_nil();
}

//
// codegen
//

void gen_prologue() {
  write_hex(
      0x55,             // push rbp
      0x48, 0x89, 0xe5, // mov rbp, rsp
  )
}
void gen_epilogue() {
  write_hex(
      0x48, 0x89, 0xec, // mov rsp, rbp
      0x5d,             // pop rbp
      0xc3              // ret
  )
}

void write_lendian(int x) {
  int b1 = x & 0xFF;
  int b2 = (x >> 8) & 0xFF;
  int b3 = (x >> 16) & 0xFF;
  int b4 = (x >> 24) & 0xFF;
  write_hex(b1, b2, b3, b4);
}

void gen_push_int(int x) {
  write_hex(0x68);
  write_lendian(x);
}

bool codegen_internal_fseq(FExpr f) {
  %%fwith FExpr fobj = f;
  FExpr first = IListFExpr_value(fobj->sons);
  // function codegen
  if (cmp_ident(first, "fn")) {
    IListFExpr cur = fobj->sons;

    cur = IListFExpr_next(cur);
    check_next(cur, "expected name in fn");
    %%fwith FExpr fnname = IListFExpr_value(cur);

    cur = IListFExpr_next(cur);
    check_next(cur, "expected arguments in fn");
    %%fwith FExpr fnargs= IListFExpr_value(cur);

    cur = IListFExpr_next(cur);
    check_next(cur, "expected body in fn");
    FExpr fnbody = IListFExpr_value(cur);

    int fnidx = jit_getidx();
    add_fninfo(fnname->ident, fnidx);
    curroffset = 0;
    gen_prologue();
    int argoffset = 0;
    forlist (FExpr, arg, fnargs->sons) {
      %%fwith FExpr argobj = arg;
      argoffset += 8;
      curroffset += 8;
      write_hex(0xff, 0xb5); // push [rax-argoffset]
      write_lendian(-argoffset);
      add_varinfo(argobj->ident, argoffset);
    }
    codegen(fnbody);
    write_hex(0x58); // pop rax ; for return value
    gen_epilogue();
  } else if (cmp_ident(first, "jit")) {
    IListFExpr cur = fobj->sons;
    cur = IListFExpr_next(cur);
    check_next(cur, "expected name in jit");
    %%fwith FExpr jitname = IListFExpr_value(cur);
    cur = IListFExpr_next(cur);
    check_next(cur, "expected body in jit");
    FExpr jitbody = IListFExpr_value(cur);
    add_jitinfo(jitname->ident, jitbody);
  } else if (cmp_ident(first, "X")) {
    IListFExpr cur = fobj->sons;
    cur = IListFExpr_next(cur);
    check_next(cur, "expected name in fn");
    %%fwith FExpr opcode = IListFExpr_value(cur);
    if (opcode->kind != FEXPR_INTLIT) error("expected int literal in X.");
    write_hex(opcode->intval);
  } else if (cmp_ident(first, "set")) {
    IListFExpr cur = fobj->sons;

    cur = IListFExpr_next(cur);
    check_next(cur, "expected name in fn");
    %%fwith FExpr lvalue = IListFExpr_value(cur);

    cur = IListFExpr_next(cur);
    check_next(cur, "expected name in fn");
    FExpr value = IListFExpr_value(cur);

    curroffset += 8;
    int offset = curroffset;
    add_varinfo(lvalue->ident, offset);
    codegen(value);
    write_hex(0x58); // pop rax
    write_hex(0x48, 0x89, 0x85); // mov [rbp-offset], rax
    write_lendian(-offset);
  } else {
    return false;
  }
  return true;
}

void codegen(FExpr f) {
  %%fwith FExpr fobj = f;
  switch (fobj->kind) {
    case FEXPR_IDENT:
      {
        VarInfo vinfo = search_var(istring_cstr(fobj->ident));
        JitInfo jinfo = search_jit(istring_cstr(fobj->ident));
        if (!varinfo_isnil(vinfo)) {
          write_hex(0xff, 0xb5);
          write_lendian(-vinfo.offset);
        } else if (!jitinfo_isnil(jinfo)) {
          codegen(jinfo.body);
        } else {
          error("undeclared `%s ident.", istring_cstr(fobj->ident));
        }
      }
      break;
    case FEXPR_INTLIT:
      gen_push_int(fobj->intval);
      break;
    case FEXPR_SEQ:
      if (IListFExpr_len(fobj->sons) != 0) {
        if (codegen_internal_fseq(f)) {
          break;
        }
        %%fwith FExpr first = IListFExpr_value(fobj->sons);
        if (first->kind != FEXPR_IDENT) goto infixgen;
        JitInfo jitinfo = search_jit(istring_cstr(first->ident));
        FnInfo fninfo = search_fn(istring_cstr(first->ident));
        if (!jitinfo_isnil(jitinfo)) {
          forlist (FExpr, arg, IListFExpr_next(fobj->sons)) {
            codegen(arg);
          }
          codegen(jitinfo.body);
          break;
        } else if (!fninfo_isnil(fninfo)) {
          forlist (FExpr, arg, IListFExpr_next(fobj->sons)) {
            codegen(arg);
          }
          int rel = fninfo.index - jit_getidx() + 5;
          write_hex(0xE8); // call
          write_lendian(rel);
          write_hex(0x50); // push rax
          break;
        }
      infixgen: {
        if (IListFExpr_len(fobj->sons) != 3) goto genfail;
        %%fwith FExpr second = IListFExpr_value(IListFExpr_next(fobj->sons));
        if (second->kind == FEXPR_OP) {
          FExpr firstf = IListFExpr_value(fobj->sons);
          codegen(firstf);
          if (IListFExpr_len(IListFExpr_next(IListFExpr_next(fobj->sons))) != 1) {
            FExpr restf = new_fexpr(FEXPR_SEQ);
            %%fwith FExpr restfobj = restf;
            restfobj->sons = IListFExpr_next(IListFExpr_next(fobj->sons));
            codegen(restf);
          } else {
            codegen(IListFExpr_value(IListFExpr_next(IListFExpr_next(fobj->sons))));
          }
          JitInfo jitinfo = search_jit(istring_cstr(second->ident));
          FnInfo fninfo = search_fn(istring_cstr(second->ident));
          if (!jitinfo_isnil(jitinfo)) {
            codegen(jitinfo.body);
            break;
          } else if (!fninfo_isnil(fninfo)) {
            int rel = fninfo.index - jit_getidx() + 5;
            write_hex(0xE8); // call
            write_lendian(rel);
            write_hex(0x50); // push rax
            break;
          } else {
            error("undeclared %s function.", istring_cstr(first->ident));
          }
        }
      }
      genfail: {
        error("undeclared %s function.", istring_cstr(first->ident));
      }
      }
      break;
    case FEXPR_BLOCK:
      {
        forlist (FExpr, son, fobj->sons) {
          codegen(son);
        }
      }
      break;
    default:
      error("unsupported %s codegen in currently.", FExprKind_tostring(fobj->kind));
      break;
  }
}

int call_main() {
  FnInfo mainp = search_fn("main");
  if (fninfo_isnil(mainp)) {
    error("undefined reference to `main");
  }
  return ((int (*)())jit_toptr(mainp.index))();
}
