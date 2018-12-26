#include "flori.h"
#include <string.h>

#define emit_asm(...) { printf("  "); printf(__VA_ARGS__); printf("\n"); }

#define write_hex2(id, ...) \
  uint8_t id[] = {__VA_ARGS__}; \
  jit_alloc_write(id, sizeof(id));
#define write_hex1(ln, ...) write_hex2(CONCAT(_hex, ln), __VA_ARGS__)
#define write_hex(...) write_hex1(__LINE__, __VA_ARGS__)

int curroffset;

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

void codegen_lvalue(FExpr f) {
  if (fe(f)->kind == FEXPR_LIST && IListFExpr_len(fe(f)->sons) == 1) {
    codegen_lvalue(IListFExpr_value(fe(f)->sons));
  } else if (fe(f)->kind == FEXPR_BLOCK && IListFExpr_len(fe(f)->sons) >= 1) {
    fiter(it, fe(f)->sons);
    while (!IListFExpr_isnil(IListFExpr_next(it))) {
      codegen(fnext(it));
    }
    codegen_lvalue(IListFExpr_last(fe(f)->sons));
  } else if (fe(f)->kind == FEXPR_SYMBOL) {
    write_hex(0x48, 0x8d, 0x85); // lea rax, [rbp-..]
    write_lendian(-fp(FSymbol, fe(f)->sym)->varoffset);
    write_hex(0x50); // push rax
  } else if (is_dotseq(f)) {
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr lvalue = fnext(it);
    FExpr fieldsym = fnext(it);
    if (fp(FType, fe(lvalue)->typ)->kind == FTYPE_PTR) {
      codegen(lvalue);
    } else {
      codegen_lvalue(lvalue);
    }
    write_hex(0x58); // pop rax
    write_hex(0x48, 0x05); // add rax, ..
    write_lendian(fp(FSymbol, fe(fieldsym)->sym)->varoffset);
    write_hex(0x50); // push rax
  } else if (is_derefseq(f)) {
    fiter(it, fe(f)->sons);
    fnext(it);
    codegen(fnext(it));
  } else {
    assert(false);
  }
}

bool codegen_internal_fseq(FExpr f) {
  FExpr first = IListFExpr_value(fe(f)->sons);
  // function codegen
  if (cmp_ident(first, "fn")) {
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
    codegen(fnbody);
    write_hex(0x58); // pop rax ; for return value
    gen_epilogue();
  } else if (cmp_ident(first, "defprimitive")) {
    // discard
  } else if (cmp_ident(first, "struct")) {
    // discard
  } else if (cmp_ident(first, "jit")) {
    // discard
  } else if (cmp_ident(first, "X")) {
    IListFExpr cur = fe(f)->sons;
    cur = IListFExpr_next(cur);
    check_next(cur, "expected name in fn");
    FExpr opcode = IListFExpr_value(cur);
    if (fe(opcode)->kind != FEXPR_INTLIT) error("expected int literal in X.");
    write_hex(fe(opcode)->intval);
  } else if (cmp_ident(first, ".")) {
    codegen_lvalue(f);
    write_hex(
      0x58, // pop rax
      0x48, 0x8b, 0x00, // mov rax, [rax]
      0x50 // push rax
    );
  } else if (cmp_ident(first, "getref")) {
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr lvalue = fnext(it);
    codegen_lvalue(lvalue);
  } else if (cmp_ident(first, "deref")) {
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr pvalue = fnext(it);
    codegen(pvalue);
    write_hex(
      0x58, // pop rax
      0x48, 0x8b, 0x00, // mov rax, [rax]
      0x50 // push rax
    );
  } else if (cmp_ident(first, "if")) {
    IListFExpr cur = fe(f)->sons;

    int relocnum = 0;
    int relocs[1024];
    int fixup = 0;
    for (;;) {
      if (relocnum == 0 && cmp_ident(IListFExpr_value(cur), "if")) {
        // cond codegen.
        cur = IListFExpr_next(cur);
        check_next(cur, "expected cond in if-expression");
        FExpr cond = IListFExpr_value(cur);
        codegen(cond);

        // cond if branching (need fixup)
        write_hex(0x58) // pop rax
        write_hex(0x48, 0x83, 0xf8, 0x00); // cmp rax, 0
        write_hex(0x0f, 0x84); // je rel
        fixup = jit_getidx();
        write_lendian(0); // fixup

        // body codegen.
        cur = IListFExpr_next(cur);
        check_next(cur, "expected body in if-expression");
        FExpr body = IListFExpr_value(cur);
        codegen(body);

        // jmp end of if after body.
        write_hex(0xe9);
        relocs[relocnum] = jit_getidx();
        relocnum++;
        write_lendian(0); // fixup

        cur = IListFExpr_next(cur);
      } else if (cmp_ident(IListFExpr_value(cur), "elif")) {
        int fixuprel = jit_getidx() - fixup - 4;
        jit_fixup_lendian(fixup, fixuprel);

        // cond codegen.
        cur = IListFExpr_next(cur);
        check_next(cur, "expected cond in if-expression");
        FExpr cond = IListFExpr_value(cur);
        codegen(cond);

        // cond if branching (need fixup)
        write_hex(0x58) // pop rax
        write_hex(0x48, 0x83, 0xf8, 0x00); // cmp rax, 0
        write_hex(0x0f, 0x84); // je rel
        fixup = jit_getidx();
        write_lendian(0); // fixup

        // body codegen.
        cur = IListFExpr_next(cur);
        check_next(cur, "expected body in if-expression");
        FExpr body = IListFExpr_value(cur);
        codegen(body);

        // jmp end of if after body.
        write_hex(0xe9);
        relocs[relocnum] = jit_getidx();
        relocnum++;
        write_lendian(0); // fixup

        cur = IListFExpr_next(cur);
      } else if (cmp_ident(IListFExpr_value(cur), "else")) {
        int fixuprel = jit_getidx() - fixup - 4;
        jit_fixup_lendian(fixup, fixuprel);

        cur = IListFExpr_next(cur);
        check_next(cur, "expected name in fn");
        FExpr body = IListFExpr_value(cur);
        codegen(body);

        // fixup relocations of if-expression.
        for (int i=0; i<relocnum; i++) {
          int fixuprel = jit_getidx() - relocs[i] - 4;
          jit_fixup_lendian(relocs[i], fixuprel);
        }
        cur = IListFExpr_next(cur);
        break;
      } else {
        error("unexpected token in if expression.");
      }
    }
  } else if (cmp_ident(first, ":=")) {
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr name = fnext(it);
    FExpr value = fnext(it);
    curroffset += get_type_size(fe(value)->typ);
    int offset = curroffset;
    fp(FSymbol, fe(name)->sym)->varoffset = offset;
    codegen(value);
    write_hex(0x58); // pop rax
    write_hex(0x48, 0x89, 0x85); // mov [rbp-offset], rax
    write_lendian(-offset);
  } else if (cmp_ident(first, "var")) {
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr name = fnext(it);
    FExpr typ = fnext(it);
    curroffset += get_type_size(fe(typ)->typsym);
    int offset = curroffset;
    fp(FSymbol, fe(name)->sym)->varoffset = offset;
  } else if (cmp_ident(first, "=")) {
    fiter(it, fe(f)->sons);
    fnext(it);
    FExpr lvalue = fnext(it);
    FExpr rvalue = fnext(it);
    codegen_lvalue(lvalue);
    codegen(rvalue);
    write_hex(0x59);// pop rcx
    write_hex(0x58); // pop rax
    write_hex(0x48, 0x89, 0x08); // mov [rax], rcx
  } else {
    return false;
  }
  return true;
}

void codegen(FExpr f) {
  switch (fe(f)->kind) {
    case FEXPR_IDENT:
      error("unresolved `%s ident.", istring_cstr(fe(f)->ident));
      break;
    case FEXPR_SYMBOL: {
        write_hex(0xff, 0xb5);
        write_lendian(-fp(FSymbol, fe(f)->sym)->varoffset);
      }
      break;
    case FEXPR_INTLIT:
      gen_push_int(fe(f)->intval);
      break;
    case FEXPR_STRLIT:
      gen_cstring(istring_cstr(fe(f)->strval));
      break;
    case FEXPR_SEQ:
      if (IListFExpr_len(fe(f)->sons) != 0) {
        if (codegen_internal_fseq(f)) {
          break;
        }
        FExpr first = IListFExpr_value(fe(f)->sons);
        if (fe(first)->kind == FEXPR_SYMBOL) {
          forlist (IListFExpr, FExpr, arg, IListFExpr_next(fe(f)->sons)) {
            codegen(arg);
          }
          if (fp(FSymbol, fe(first)->sym)->isjit) {
            break;
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
      }
      break;
    case FEXPR_LIST:
      {
        forlist (IListFExpr, FExpr, son, fe(f)->sons) {
          codegen(son);
        }
      }
      break;
    case FEXPR_BLOCK:
      {
        forlist (IListFExpr, FExpr, son, fe(f)->sons) {
          codegen(son);
        }
      }
      break;
    default:
      error("unsupported %s codegen in currently.", FExprKind_tostring(fe(f)->kind));
      break;
  }
}

int call_main() {
  FnDecl fndecl;
  if (!search_fndecl(new_istring("main"), new_FTypeVec(), &fndecl)) {
    error("undefined reference to `main");
  }
  return ((int (*)())jit_toptr(fp(FSymbol, fndecl.sym)->fnidx))();
}
