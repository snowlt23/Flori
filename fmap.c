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
  return fmap;
}

bool eq_kind(FMap map, IString s) {
  return istring_eq(fm(map)->parentkind, s) || istring_eq(fm(map)->kind, s);
}

FMap fmap() {
  FMap fmap = new_fmap(FMAP_MAP);
  fm(fmap)->fields = nil_IListField();
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

//
// operations
//

void fmap_push(FMap fmap, IString k, FMap v) {
  assert(eq_kind(fmap, FMAP_MAP));
  fm(fmap)->fields = new_IListField((Field){k, v}, fm(fmap)->fields);
}

FMap fmap_get(FMap fmap, IString k) {
  assert(eq_kind(fmap, FMAP_MAP));
  forlist (IListField, Field, field, fm(fmap)->fields) {
    if (istring_eq(field.key, k)) return field.value;
  }
  return nil_FMap();
}

void flist_push(FMap fmap, FMap val) {
  assert(eq_kind(fmap, FMAP_LIST));
  fm(fmap)->lst = new_IListFMap(val, fm(fmap)->lst);
}

FMap flist_reverse(FMap fmap) {
  FMap newf = flist();
  forlist (IListFMap, FMap, f, fm(fmap)->lst) {
    flist_push(newf, f);
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

char* fmap_tostring_inside(FMap fmap, int indent) {
  char buf[1024*1024] = {};
  int bufpos = 0;
  if (eq_kind(fmap, FMAP_MAP)) {
    if (IListField_len(fm(fmap)->fields) == 0) {
      snprintf(buf, 1024*1024-bufpos, "%%h{kind: %s}", istring_cstr(fm(fmap)->kind));
      return strdup(buf);
    }
    strcpy(buf + bufpos, "%h{\n");
    bufpos += strlen("%h{\n");
    indent += 2;
    write_indent(buf + bufpos, indent); bufpos += indent;
    bufpos += snprintf(buf + bufpos, 1024*1024-bufpos, "kind: %s,\n", istring_cstr(fm(fmap)->kind));
    forlist (IListField, Field, field, fm(fmap)->fields) {
      write_indent(buf + bufpos, indent); bufpos += indent;
      bufpos += snprintf(buf + bufpos, 1024*1024-bufpos, "%s: %s,\n", istring_cstr(field.key), fmap_tostring_inside(field.value, indent));
    }
    indent -= 2;
    write_indent(buf + bufpos, indent); bufpos += indent;
    bufpos += snprintf(buf + bufpos, 1024*1024-bufpos, "}");
  } else if (eq_kind(fmap, FMAP_LIST)) {
    bufpos += snprintf(buf + bufpos, 1024*1024-bufpos, "( ");
    forlist (IListFMap, FMap, f, fm(fmap)->lst) {
      bufpos += snprintf(buf + bufpos, 1024*1024-bufpos, "%s ", fmap_tostring_inside(f, indent));
    }
    bufpos += snprintf(buf + bufpos, 1024*1024-bufpos, ")");
  } else if (eq_kind(fmap, FMAP_IDENT)) {
    snprintf(buf, 1024*1024, "%s", istring_cstr(fm(fmap)->ident));
  } else if (eq_kind(fmap, FMAP_SYMBOL)) {
    assert(false); // TODO:
  } else if (eq_kind(fmap, FMAP_INTLIT)) {
    snprintf(buf, 1024*1024, "%" PRId64, fm(fmap)->intval);
  } else if (eq_kind(fmap, FMAP_STRLIT)) {
    snprintf(buf, 1024*1024, "\"%s\"", istring_cstr(fm(fmap)->strval));
  } else {
    assert(false);
  }
  return strdup(buf);
}


char* fmap_tostring(FMap fmap) {
  return fmap_tostring_inside(fmap, 0);
}
