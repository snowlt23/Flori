#include <stdio.h>
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

void codegen(fexpr f) {
  if (fexpr_ptr(f)->kind == FEXPR_INTLIT) {
    emit_asm("push %d", fexpr_ptr(f)->intval);
  } else if (fexpr_ptr(f)->kind == FEXPR_INFIX && fexpr_ptr(fexpr_ptr(f)->call)->kind == FEXPR_IDENT && strcmp(istring_cstr(fexpr_ptr(fexpr_ptr(f)->call)->ident), "+") == 0) {
    fexpr left = iarray_fexpr_get(fexpr_ptr(f)->arguments, 0);
    fexpr right = iarray_fexpr_get(fexpr_ptr(f)->arguments, 1);
    codegen(left);
    codegen(right);
    emit_asm("pop rcx");
    emit_asm("pop rax");
    emit_asm("add rax, rcx");
    emit_asm("push rax");
  } else if (fexpr_ptr(f)->kind == FEXPR_INFIX && fexpr_ptr(fexpr_ptr(f)->call)->kind == FEXPR_IDENT && strcmp(istring_cstr(fexpr_ptr(fexpr_ptr(f)->call)->ident), "-") == 0) {
    fexpr left = iarray_fexpr_get(fexpr_ptr(f)->arguments, 0);
    fexpr right = iarray_fexpr_get(fexpr_ptr(f)->arguments, 1);
    codegen(left);
    codegen(right);
    emit_asm("pop rcx");
    emit_asm("pop rax");
    emit_asm("sub rax, rcx");
    emit_asm("push rax");
  } else {
    assert(false);
  }
}

void codegen_main(fexpr f) {
  emit_asm("global _start");
  emit_label("_start");
  codegen(f);
  emit_asm("pop rax");
  emit_sysexit("rax");
}
