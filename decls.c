#include "flori.h"
#include <string.h>

ParserDeclMap parserdeclmap;

void decls_init() {
  parserdeclmap = nil_ParserDeclMap();
}

//
// parser decl
//

ParserDecl new_internal_parserdecl(IString hook, FMap (*internalfn)(Stream* s)) {
  ParserDecl decl;
  decl.hook = hook;
  decl.internalfn = internalfn;
  return decl;
}

ParserDecl new_parserdecl(IString hook, FSymbol sym) {
  ParserDecl decl;
  decl.hook = hook;
  decl.internalfn = NULL;
  decl.sym = sym;
  return decl;
}

void add_parser_decl(ParserDecl decl) {
  parserdeclmap = new_ParserDeclMap(decl, parserdeclmap);
}

bool search_parser_decl(IString id, ParserDecl* retdecl) {
  forlist (ParserDeclMap, ParserDecl, decl, parserdeclmap) {
    if (istring_eq(decl.hook, id)) {
      *retdecl = decl;
      return true;
    }
  }
  return false;
}
