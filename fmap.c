#include "flori.h"
#include <string.h>
#include <inttypes.h>

IString FMAP_MAP;
IString FMAP_LIST;
IString FMAP_IDENT;
IString FMAP_SYMBOL;
IString FMAP_INTLIT;
IString FMAP_STRLIT;
IString FMAP_CALL;

void fmap_init() {
  FMAP_MAP = new_istring("fmap");
  FMAP_LIST = new_istring("flist");
  FMAP_IDENT = new_istring("fident");
  FMAP_SYMBOL = new_istring("fsymbol");
  FMAP_INTLIT = new_istring("fintlit");
  FMAP_STRLIT = new_istring("fstrlit");
  FMAP_CALL = new_istring("call");
}

FMap new_fmap(IString kind) {
  FMap fmap = alloc_FMap();
  fm(fmap)->parentkind = kind;
  fm(fmap)->kind = kind;
  fm(fmap)->fields = nil_IListField();
  return fmap;
}

FMap copy_fmap(FMap f) {
  FMap newf = alloc_FMap();
  *fm(newf) = *fm(f);
  return newf;
}

FMap deepcopy_fmap(FMap f) {
  FMap newf = copy_fmap(f);
  fm(newf)->fields = nil_IListField();
  forlist (IListField, Field, field, fm(f)->fields) {
    fmap_push(newf, field.key, deepcopy_fmap(field.value));
  }
    
  if (eq_kind(f, FMAP_LIST)) {
    fm(newf)->lst = nil_IListFMap();
    forlist (IListFMap, FMap, e, fm(f)->lst) {
      flist_push(newf, deepcopy_fmap(e));
    }
    *fm(newf) = *fm(flist_reverse(newf));
  }
  
  return newf;
}

bool eq_kind(FMap map, IString s) {
  return istring_eq(fm(map)->parentkind, s) || istring_eq(fm(map)->kind, s);
}

FMap fmap() {
  FMap fmap = new_fmap(FMAP_MAP);
  return fmap;
}

FMap flist() {
  FMap fmap = new_fmap(FMAP_LIST);
  fm(fmap)->lst = nil_IListFMap();
  return fmap;
}

FMap fident(IString id) {
  FMap fmap = new_fmap(FMAP_IDENT);
  fm(fmap)->ident = id;
  return fmap;
}

FMap fsymbol(FSymbol sym) {
  FMap fmap = new_fmap(FMAP_SYMBOL);
  fm(fmap)->sym = sym;
  return fmap;
}

FMap fintlit(int64_t x) {
  FMap fmap = new_fmap(FMAP_INTLIT);
  fm(fmap)->intval = x;
  return fmap;
}

FMap fstrlit(IString s) {
  FMap fmap = new_fmap(FMAP_STRLIT);
  fm(fmap)->strval = s;
  return fmap;
}

FMap fcall(FMap call, FMap args) {
  def_fmap(f, call, {
      def_field(call, call);
      def_field(args, args);
    });
  return f;
}

FMap fprefix(FMap call, FMap arg) {
  flistseq(args, arg);
  return fcall(call, args);
}

//
// operations
//

void fmap_push(FMap f, IString k, FMap v) {
  fm(f)->fields = new_IListField((Field){k, v}, fm(f)->fields);
}

void fmap_cpush(FMap f, char* k, FMap v) {
  fmap_push(f, new_istring(k), v);
}

FMap fmap_cget(FMap f, char* k) {
  forlist (IListField, Field, field, fm(f)->fields) {
    if (istring_ceq(field.key, k)) return field.value;
  }
  return nil_FMap();
}

FMap fmap_get(FMap f, IString k) {
  return fmap_cget(f, istring_cstr(k));
}

void flist_push(FMap f, FMap val) {
  assert(eq_kind(f, FMAP_LIST));
  fm(f)->lst = new_IListFMap(val, fm(f)->lst);
}

FMap flist_reverse(FMap f) {
  FMap newf = copy_fmap(f);
  fm(newf)->lst = nil_IListFMap();
  forlist (IListFMap, FMap, e, fm(f)->lst) {
    flist_push(newf, e);
  }
  return newf;
}

FMap first(IListFMap lst) {
  return IListFMap_value(lst);
}
IListFMap rest(IListFMap lst) {
  return IListFMap_next(lst);
}

void write_indent(char* buf, int indent) {
  for (int i=0; i<indent; i++) {
    *buf = ' ';
    buf++;
  }
}

char* fmap_tostring_inside(FMap f, int indent) {
  if (FMap_isnil(f)) return "nil";
  
  char buf[1024*10] = {};
  int bufpos = 0;
  if (eq_kind(f, FMAP_MAP)) {
    if (IListField_len(fm(f)->fields) == 0) {
      snprintf(buf, 1024*1024-bufpos, "%%m{kind: %s}", istring_cstr(fm(f)->kind));
      return strdup(buf);
    }
    strcpy(buf + bufpos, "%m{\n");
    bufpos += strlen("%m{\n");
    indent += 2;
    write_indent(buf + bufpos, indent); bufpos += indent;
    bufpos += snprintf(buf + bufpos, 1024*1024-bufpos, "kind: %s,\n", istring_cstr(fm(f)->kind));
    forlist (IListField, Field, field, fm(f)->fields) {
      write_indent(buf + bufpos, indent); bufpos += indent;
      bufpos += snprintf(buf + bufpos, 1024*1024-bufpos, "%s: %s,\n", istring_cstr(field.key), fmap_tostring_inside(field.value, indent));
    }
    indent -= 2;
    write_indent(buf + bufpos, indent); bufpos += indent;
    bufpos += snprintf(buf + bufpos, 1024*1024-bufpos, "}");
  } else if (eq_kind(f, FMAP_LIST)) {
    bufpos += snprintf(buf + bufpos, 1024*1024-bufpos, "( ");
    forlist (IListFMap, FMap, e, fm(f)->lst) {
      bufpos += snprintf(buf + bufpos, 1024*1024-bufpos, "%s ", fmap_tostring_inside(e, indent));
    }
    bufpos += snprintf(buf + bufpos, 1024*1024-bufpos, ")");
  } else if (eq_kind(f, FMAP_IDENT)) {
    snprintf(buf, 1024*1024, "%s", istring_cstr(fm(f)->ident));
  } else if (eq_kind(f, FMAP_SYMBOL)) {
    if (!FType_isnil(fp(FSymbol, fm(f)->sym)->t)) {
      return ftype_tostring(fp(FSymbol, fm(f)->sym)->t);
    }
    snprintf(buf, 1024*1024, "%s", istring_cstr(fp(FSymbol, fm(f)->sym)->name));
  } else if (eq_kind(f, FMAP_INTLIT)) {
    snprintf(buf, 1024*1024, "%" PRId64, fm(f)->intval);
  } else if (eq_kind(f, FMAP_STRLIT)) {
    snprintf(buf, 1024*1024, "\"%s\"", istring_cstr(fm(f)->strval));
  } else {
    assert(false);
  }
  return strdup(buf);
}

char* fmap_tostring(FMap f) {
  return fmap_tostring_inside(f, 0);
}

char* fmap_repr_indent(FMap f, int indent) {
  if (FMap_isnil(f)) return "nil";
  
  char buf[1024*10] = {};
  int bufpos = 0;

  if (eq_kind(f, new_istring("call"))) {
    bufpos += snprintf(buf + bufpos, 1024*1024 - bufpos, "%s%s", fmap_repr_indent(fmap_cget(f, "call"), indent), fmap_repr_indent(fmap_cget(f, "args"), indent));
  } else if (eq_kind(f, new_istring("block"))) {
    if (IListField_len(fm(f)->fields) == 0) {
      snprintf(buf, 1024*1024-bufpos, "{}");
      return strdup(buf);
    }
    strcpy(buf + bufpos, "{\n");
    bufpos += strlen("{\n");
    indent += 2;
    write_indent(buf + bufpos, indent); bufpos += indent;
    bufpos += snprintf(buf + bufpos, 1024*1024-bufpos, "kind: %s,\n", istring_cstr(fm(f)->kind));
    forlist (IListField, Field, field, fm(f)->fields) {
      write_indent(buf + bufpos, indent); bufpos += indent;
      bufpos += snprintf(buf + bufpos, 1024*1024-bufpos, "%s: %s,\n", istring_cstr(field.key), fmap_repr_indent(field.value, indent));
    }
    indent -= 2;
    write_indent(buf + bufpos, indent); bufpos += indent;
    bufpos += snprintf(buf + bufpos, 1024*1024-bufpos, "}");
  } else if (eq_kind(f, FMAP_MAP)) {
    if (IListField_len(fm(f)->fields) == 0) {
      snprintf(buf, 1024*1024-bufpos, "%%m{kind: %s}", istring_cstr(fm(f)->kind));
      return strdup(buf);
    }
    strcpy(buf + bufpos, "%m{\n");
    bufpos += strlen("%m{\n");
    indent += 2;
    write_indent(buf + bufpos, indent); bufpos += indent;
    bufpos += snprintf(buf + bufpos, 1024*1024-bufpos, "kind: %s,\n", istring_cstr(fm(f)->kind));
    forlist (IListField, Field, field, fm(f)->fields) {
      write_indent(buf + bufpos, indent); bufpos += indent;
      bufpos += snprintf(buf + bufpos, 1024*1024-bufpos, "%s: %s,\n", istring_cstr(field.key), fmap_repr_indent(field.value, indent));
    }
    indent -= 2;
    write_indent(buf + bufpos, indent); bufpos += indent;
    bufpos += snprintf(buf + bufpos, 1024*1024-bufpos, "}");
  } else if (eq_kind(f, FMAP_LIST)) {
    bufpos += snprintf(buf + bufpos, 1024*1024-bufpos, "(");
    forlist (IListFMap, FMap, e, fm(f)->lst) {
      bufpos += snprintf(buf + bufpos, 1024*1024-bufpos, "%s, ", fmap_repr_indent(e, indent));
    }
    bufpos += snprintf(buf + bufpos, 1024*1024-bufpos, ")");
  } else {
    return fmap_tostring_inside(f, indent);
  }
  return strdup(buf);
}

char* fmap_repr(FMap f) {
  return fmap_repr_indent(f, 0);
}
