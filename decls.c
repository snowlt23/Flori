#include "flori.h"
#include <string.h>

DeclMap declmap;
DeclMap localmap;
FnDeclMap fndeclmap;
//SyntaxDeclMap syntaxdeclmap;
InternalDeclMap internaldeclmap;
ParserDeclMap parserdeclmap;

void decls_init() {
  declmap = nil_DeclMap();
  localmap = nil_DeclMap();
  fndeclmap = nil_FnDeclMap();
  internaldeclmap = nil_InternalDeclMap();
  parserdeclmap = nil_ParserDeclMap();
}

//
// decls
//

void add_decl(Decl decl) {
  declmap = new_DeclMap(decl, declmap);
}

void add_local(Decl decl) {
  localmap = new_DeclMap(decl, localmap);
}

DeclMap get_local_checkpoint() {
  return localmap;
}

void rollback_local(DeclMap map) {
  localmap = map;
}

bool search_decl(IString name, Decl* retdecl) {
  {
    forlist (DeclMap, Decl, decl, declmap) {
      if (istring_eq(decl.name, name)) {
        *retdecl = decl;
        return true;
      }
    }
  }
  {
    forlist (DeclMap, Decl, decl, localmap) {
      if (istring_eq(decl.name, name)) {
        *retdecl = decl;
        return true;
      }
    }
  }
  return false;
}

//
// fn decls
//

void add_fndecl(FnDecl decl) {
  forlist (FnDeclMap, FnDeclGroup, fngroup, fndeclmap) {
    if (istring_eq(fp(FnDeclGroup, fngroup)->name, decl.name)) {
      fp(FnDeclGroup, fngroup)->decls = new_IListFnDecl(decl, fp(FnDeclGroup, fngroup)->decls);
      return;
    }
  }
  // if unregistered function
  FnDeclGroup newgroup = alloc_FnDeclGroup();
  fp(FnDeclGroup, newgroup)->name = decl.name;
  fp(FnDeclGroup, newgroup)->decls = new_IListFnDecl(decl, nil_IListFnDecl());
  fndeclmap = new_FnDeclMap(newgroup, fndeclmap);
}

bool match_overload(IListFType fntypes, FTypeVec* argtypes) {
  if (IListFType_len(fntypes) != argtypes->len) return false;
  IListFType curr = fntypes;
  for (int i=0; i<argtypes->len; i++) {
    FType fnt = IListFType_value(curr);
    FType argt = FTypeVec_get(argtypes, i);
    if (!ftype_eq(fnt, argt)) {
      return false;
    }
    curr = IListFType_next(curr);
  }
  return true;
}

bool search_fndecl_from_group(FnDeclGroup fngroup, IString name, FTypeVec* argtypes, FnDecl* retfndecl) {
  forlist (IListFnDecl, FnDecl, fndecl, fp(FnDeclGroup, fngroup)->decls) {
    if (fndecl.internalfn != NULL) {
      *retfndecl = fndecl;
      return true;
    }
    
    IListFType fnargtypes;
    if (fp(FSymbol, fndecl.sym)->rewrited) {
      fnargtypes = IListFType_next(fndecl.argtypes);
    } else {
      fnargtypes = fndecl.argtypes;
    }
    if (match_overload(fnargtypes, argtypes)) {
      *retfndecl = fndecl;
      return true;
    }
  }
  return false;
}

bool search_fndecl(IString name, FTypeVec* argtypes, FnDecl* retfndecl) {
  forlist (FnDeclMap, FnDeclGroup, fngroup, fndeclmap) {
    if (istring_eq(fp(FnDeclGroup, fngroup)->name, name)) {
      if (search_fndecl_from_group(fngroup, name, argtypes, retfndecl)) {
        return true;
      }
    }
  }
  return false;
}

//
// internal decl
//

void add_internal_decl(InternalDecl decl) {
  internaldeclmap = new_InternalDeclMap(decl, internaldeclmap);
}

bool search_internal_decl(IString name, InternalDecl* retdecl) {
  forlist (InternalDeclMap, InternalDecl, decl, internaldeclmap) {
    if (istring_eq(decl.name, name)) {
      *retdecl = decl;
      return true;
    }
  }
  return false;
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
