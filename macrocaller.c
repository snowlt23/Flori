#include "flori.h"

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

size_t call_macro_prim4(void* fnaddr, size_t arg1, size_t arg2, size_t arg3, size_t arg4) {
  size_t ret;
  asm volatile(".intel_syntax noprefix;"
               "mov rax, %1;"
               "push %5;"
               "push %4;"
               "push %3;"
               "push %2;"
               "call rax;"
               "add rsp, 32;"
               "mov %0, rax;"
               ".att_syntax;" : "=r"(ret) : "r"(fnaddr), "r"(arg1), "r"(arg2), "r"(arg3), "r"(arg4) : "rax", "rsp");
  return ret;
}

size_t fnextidx(IListFMap* it) {
  FMap ret = IListFMap_value(*it);
  *it = IListFMap_next(*it);
  return ret.index;
}

FMap call_macro(FSymbol sym, IListFMap args) {
  void* fnaddr = jit_toptr(fp(FSymbol, sym)->fnidx);
  size_t argn = IListFMap_len(args);
  size_t fidx;
  IListFMap it = args;
  if (argn == 0) {
    fidx = call_macro_prim0(fnaddr);
  } else if (argn == 1) {
    fidx = call_macro_prim1(fnaddr, fnextidx(&it));
  } else if (argn == 2) {
    fidx = call_macro_prim2(fnaddr, fnextidx(&it), fnextidx(&it));
  } else if (argn == 3) {
    fidx = call_macro_prim3(fnaddr, fnextidx(&it), fnextidx(&it), fnextidx(&it));
  } else if (argn == 4) {
    fidx = call_macro_prim4(fnaddr, fnextidx(&it), fnextidx(&it), fnextidx(&it), fnextidx(&it));
  } else {
    error("unsupported %zd length macro call in currently", argn);
  }
  return (FMap){fidx};
}
