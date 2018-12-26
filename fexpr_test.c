#include "flori.h"
#include <string.h>

#define cmptest(a, b) if (strcmp(fexpr_tostring(parse(new_stream(a))), b) != 0) {debug("%s", fexpr_tostring(parse(new_stream(a)))); assert(false);}

int main() {
  linmem_init(1024*1024);
  cmptest("yukayuka", "yukayuka");
  cmptest("1515", "1515");
  cmptest("555", "555");
  cmptest("\"testes\"", "\"testes\"");
  cmptest("+", "+");
  cmptest("+=+", "+=+");
  cmptest("a bb ccc", "a bb ccc");
  // cmptest("[]", "[]");
  // cmptest("[aaa,   bb,    c]", "[aaa, bb, c]");
  cmptest("()", "()");
  cmptest("(aaa,   bb,    c)", "(aaa, bb, c)");
  cmptest("{}", "{}");
  cmptest("{aaa\nbb; c;}", "{\n  aaa\n  bb\n  c\n}");
  cmptest("jit return(^int) ^int {}", "jit return (type int) type int {}");
}