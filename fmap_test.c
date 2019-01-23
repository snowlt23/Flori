#include "flori.h"
#include <string.h>

#define cmptest(a, b) if (strcmp(fmap_tostring(parse(new_stream(a))), b) != 0) {debug("%s", fmap_tostring(parse(new_stream(a)))); assert(false);}

int main() {
  linmem_init(1024*1024);
  fmap_init();
  decls_init();
  parser_init_internal();
  cmptest("yukayuka", "yukayuka");
  cmptest("1515", "1515");
  cmptest("555", "555");
  cmptest("\"testes\"", "\"testes\"");
  //cmptest("a bb ccc", "a bb ccc");
  // cmptest("[]", "[]");
  // cmptest("[aaa,   bb,    c]", "[aaa, bb, c]");
  //cmptest("()", "()");
  //cmptest("(aaa,   bb,    c)", "(aaa, bb, c)");
  cmptest("{}", "( )");
  cmptest("{ aaa bb c }", "( aaa bb c )");
  cmptest("{ \naaa\nbb\nc\n }", "( aaa bb c )");
  cmptest("fn main() {}", "%m{\n  kind: fn,\n  body: ( ),\n  returntype: %m{\n    kind: type,\n    t: void,\n  },\n  argdecls: ( ),\n  name: main,\n}");
  cmptest("1 + 2", "%m{\n  kind: call,\n  args: ( 1 2 ),\n  call: +,\n}");
  // cmptest("inline fn return(r int) int {}", "%m{\n  kind: jit,\n  body: ( ),\n  returntype: int,\n  argdecls: ( ),\n  name: return,\n}");
  cmptest("add(1, 2)", "%m{\n  kind: call,\n  args: ( 1 2 ),\n  call: add,\n}");
  cmptest("&1", "%m{\n  kind: call,\n  args: ( 1 ),\n  call: &,\n}");
}
