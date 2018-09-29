#include "flori.h"
#include <string.h>

#define emit_asm(...) { printf("  "); printf(__VA_ARGS__); printf("\n"); }

#define write_hex2(id, ...) \
  uint8_t id[] = {__VA_ARGS__}; \
  jit_alloc_write(id, sizeof(id));
#define write_hex1(ln, ...) write_hex2(CONCAT(_hex, ln), __VA_ARGS__)
#define write_hex(...) write_hex1(__LINE__, __VA_ARGS__)

IListFnPair fnmap;

void codegen_init() {
  fnmap = nil_IListFnPair();
}

void add_fnpair(IString key, int idx) {
  fnmap = new_IListFnPair((FnPair){key, idx}, fnmap);
}

FnPair fnpair_nil() {
  FnPair pair;
  pair.index = -1;
  return pair;
}

bool fnpair_isnil(FnPair pair) {
  return pair.index == -1;
}

FnPair search_fn(char* name) {
  forlist (FnPair, pair, fnmap) {
    if (strcmp(istring_cstr(pair.key), name) == 0) {
      return pair;
    }
  }
  return fnpair_nil();
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

void codegen_fseq(FExpr f) {
  %%fwith FExpr fobj = f;
  FExpr first = IListFExpr_value(fobj->sons);
  // function codegen
  if (cmp_ident(first, "fn")) {
    IListFExpr cur = fobj->sons;
    cur = IListFExpr_next(cur);
    check_next(cur, "expected name in fn");
    %%fwith FExpr fnname = IListFExpr_value(cur);
    cur = IListFExpr_next(cur);
    check_next(cur, "expected body in fn");
    FExpr fnbody = IListFExpr_value(cur);
    int fnidx = jit_getidx();
    add_fnpair(fnname->ident, fnidx);
    gen_prologue();
    codegen(fnbody);
    write_hex(0x58); // pop rax ; for return value
    gen_epilogue();
  } else if (cmp_ident(first, "X")) {
    IListFExpr cur = fobj->sons;
    cur = IListFExpr_next(cur);
    check_next(cur, "expected name in fn");
    %%fwith FExpr opcode = IListFExpr_value(cur);
    if (opcode->kind != FEXPR_INTLIT) error("expected int literal in X.");
    write_hex(opcode->intval);
  } else {
    error("undefined `%s syntax", istring_cstr(FExpr_ptr(first)->ident));
  }
}

void codegen(FExpr f) {
  %%fwith FExpr fobj = f;
  switch (fobj->kind) {
    case FEXPR_INTLIT:
      gen_push_int(fobj->intval);
      break;
    case FEXPR_SEQ:
      if (IListFExpr_len(fobj->sons) != 0) {
        codegen_fseq(f);
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
  FnPair mainp = search_fn("main");
  if (fnpair_isnil(mainp)) {
    error("undefined reference to `main");
  }
  return ((int (*)())jit_toptr(mainp.index))();
}
