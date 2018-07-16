#include <stdio.h>

FILE* codegenhandle;

#define emit(...) fprintf(codegenhandle, __VA_ARGS__)
#define emit_asm(...) {emit("  "); emit(__VA_ARGS__); emit("\n");}

void emit_label(char* l) {
  emit("%s:\n", l);
}

void emit_zeroclear(char* reg) {
  emit_asm("xor %s, %s", reg, reg);
}

int main() {
  codegenhandle = stdout;
  emit_asm("global _start");
  emit_label("_start");
  emit_asm("mov rax, 60");
  emit_asm("mov rdi, 1");
  emit_asm("syscall");
}

