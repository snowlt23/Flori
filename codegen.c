#include "flori.h"

#define emit_asm(...) { printf("  "); printf(__VA_ARGS__); printf("\n"); }

void codegen(FExpr f) {
  %%fwith FExpr fobj = f;
  switch (fobj->kind) {
    case FEXPR_INTLIT:
      emit_asm("mov rax, %d", fobj->intval);
      break;
    default:
      error("unsupported %s codegen in currently.", FExprKind_tostring(fobj->kind));
      break;
  }
}
