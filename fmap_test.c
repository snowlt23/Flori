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
  cmptest("+", "+");
  cmptest("+=+", "+=+");
  //cmptest("a bb ccc", "a bb ccc");
  // cmptest("[]", "[]");
  // cmptest("[aaa,   bb,    c]", "[aaa, bb, c]");
  //cmptest("()", "()");
  //cmptest("(aaa,   bb,    c)", "(aaa, bb, c)");
  cmptest("{}", "( )");
  cmptest("{ aaa bb c }", "( aaa bb c )");
  cmptest("{ \naaa\nbb\nc\n }", "( aaa bb c )");
  cmptest("fn main() {}", "%h{\n  kind: fn,\n  body: ( ),\n  returntype: %h{\n    kind: type,\n    t: \"void\",\n  },\n  args: ( ),\n  name: main,\n}");
  cmptest("1 + 2", "%h{\n  kind: call,\n  args: ( 1 2 ),\n  call: +,\n}");
  //cmptest("jit return(^int) ^int {}", "jit return (type int) type int {}");
}
