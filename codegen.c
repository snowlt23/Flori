#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "flori.h"

FILE* codegenhandle;

#define emit(...) fprintf(codegenhandle, __VA_ARGS__)
#define emit_asm(...) {emit("  "); emit(__VA_ARGS__); emit("\n");}

void emit_label(char* l) {
  emit("%s:\n", l);
}

void emit_zeroclear(char* reg) {
  emit_asm("xor %s, %s", reg, reg);
}

void emit_sysexit(char* retcode) {
  emit_asm("mov rdi, %s", retcode);
  emit_asm("mov rax, 60");
  emit_asm("syscall");
}

void init_codegen(FILE* handle) {
  codegenhandle = handle;
}

bool cmp_infix(fexpr f, char* infix) {
  return fexpr_ptr(f)->kind == FEXPR_INFIX && fexpr_ptr(fexpr_ptr(f)->call)->kind == FEXPR_IDENT && strcmp(istring_cstr(fexpr_ptr(fexpr_ptr(f)->call)->ident), infix) == 0;
}

varpair* new_varpair(char* name, int offset) {
  varpair* vp = malloc(sizeof(varpair));
  vp->name = name;
  vp->offset = offset;
  return vp;
}

genctx* new_genctx() {
  genctx* gen = malloc(sizeof(genctx));
  gen->varmap = new_vector();
  gen->varpos = 0;
  return gen;
}

void assign_variable_position(genctx* gen, char* name, int size) {
  gen->varpos += size;
  vector_push(gen->varmap, new_varpair(name, gen->varpos));
}

int get_variable_offset(genctx* gen, char* name) {
  for (int i=0; i<gen->varmap->len; i++) {
    varpair* vp = (varpair*)vector_get(gen->varmap, i);
    if (strcmp(vp->name, name) == 0) return vp->offset;
  }
  return -1;
}

void assign_variable(genctx* gen, fexpr f) {
  if (cmp_infix(f, ":=")) {
    fexpr left = iarray_fexpr_get(fexpr_ptr(f)->arguments, 0);
    assign_variable_position(gen, istring_cstr(fexpr_ptr(left)->ident), 8);
  } else if (fexpr_ptr(f)->kind == FEXPR_BLOCK) {
    for (int i=0; i<fexpr_ptr(f)->sons.len; i++) {
      fexpr son = iarray_fexpr_get(fexpr_ptr(f)->sons, i);
      assign_variable(gen, son);
    }
  }
}

void codegen(genctx* gen, fexpr f) {
  if (fexpr_ptr(f)->kind == FEXPR_IDENT) {
    int offset = get_variable_offset(gen, istring_cstr(fexpr_ptr(f)->ident));
    emit_asm("mov rax, [rbp-%d]", offset);
    emit_asm("push rax");
  } else if (fexpr_ptr(f)->kind == FEXPR_INTLIT) {
    emit_asm("push %d", fexpr_ptr(f)->intval);
  } else if (cmp_infix(f, ":=")) {
    fexpr left = iarray_fexpr_get(fexpr_ptr(f)->arguments, 0);
    fexpr right = iarray_fexpr_get(fexpr_ptr(f)->arguments, 1);
    assert(fexpr_ptr(left)->kind == FEXPR_IDENT);
    int offset = get_variable_offset(gen, istring_cstr(fexpr_ptr(left)->ident));
    codegen(gen, right);
    emit_asm("pop rax");
    emit_asm("mov [rbp-%d], rax", offset);
  } else if (cmp_infix(f, "+")) {
    fexpr left = iarray_fexpr_get(fexpr_ptr(f)->arguments, 0);
    fexpr right = iarray_fexpr_get(fexpr_ptr(f)->arguments, 1);
    codegen(gen, left);
    codegen(gen, right);
    emit_asm("pop rcx");
    emit_asm("pop rax");
    emit_asm("add rax, rcx");
    emit_asm("push rax");
  } else if (cmp_infix(f, "-")) {
    fexpr left = iarray_fexpr_get(fexpr_ptr(f)->arguments, 0);
    fexpr right = iarray_fexpr_get(fexpr_ptr(f)->arguments, 1);
    codegen(gen, left);
    codegen(gen, right);
    emit_asm("pop rcx");
    emit_asm("pop rax");
    emit_asm("sub rax, rcx");
    emit_asm("push rax");
  } else if (cmp_infix(f, "*")) {
    fexpr left = iarray_fexpr_get(fexpr_ptr(f)->arguments, 0);
    fexpr right = iarray_fexpr_get(fexpr_ptr(f)->arguments, 1);
    codegen(gen, left);
    codegen(gen, right);
    emit_asm("mov rdx, 0");
    emit_asm("pop rcx");
    emit_asm("pop rax");
    emit_asm("mul rcx");
    emit_asm("push rax");
  } else if (cmp_infix(f, "/")) {
    fexpr left = iarray_fexpr_get(fexpr_ptr(f)->arguments, 0);
    fexpr right = iarray_fexpr_get(fexpr_ptr(f)->arguments, 1);
    codegen(gen, left);
    codegen(gen, right);
    emit_asm("pop rcx");
    emit_asm("pop rax");
    emit_asm("div rcx");
    emit_asm("push rax");
  } else if (fexpr_ptr(f)->kind == FEXPR_BLOCK) {
    for (int i=0; i<fexpr_ptr(f)->sons.len; i++) {
      codegen(gen, iarray_fexpr_get(fexpr_ptr(f)->sons, i));
    }
  } else if (fexpr_ptr(f)->kind == FEXPR_CALL && fexpr_ptr(fexpr_ptr(f)->call)->kind == FEXPR_IDENT) {
    for (int i=0; i<fexpr_ptr(f)->arguments.len; i++) {
      codegen(gen, iarray_fexpr_get(fexpr_ptr(f)->arguments, i));
    }
    emit_asm("call %s", istring_cstr(fexpr_ptr(fexpr_ptr(f)->call)->ident)); // FIXME: call stack
    emit_asm("push rax");
  } else {
    assert(false);
  }
}

void codegen_fn(fexpr f) {
  assert(cmp_infix(f, "=>"));
  genctx* gen = new_genctx();
  fexpr left = iarray_fexpr_get(fexpr_ptr(f)->arguments, 0);
  fexpr right = iarray_fexpr_get(fexpr_ptr(f)->arguments, 1);
  assign_variable(gen, right);
  emit_label(istring_cstr(fexpr_ptr(left)->ident));
  emit_asm("push rbp");
  emit_asm("mov rbp, rsp");
  emit_asm("sub rsp, %d", gen->varpos);
  codegen(gen, right);
  emit_asm("pop rax");
  emit_asm("mov rsp, rbp");
  emit_asm("pop rbp");
  emit_asm("ret");
}

void codegen_startup() {
  emit_asm("global _start");
  emit_label("_start");
  emit_asm("call main");
  emit_sysexit("rax");
}
