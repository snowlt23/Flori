#include "flori.h"
#include <string.h>
#include <inttypes.h>

String* new_string_cap(size_t cap) {
  String* s = malloc(sizeof(String));
  s->data = malloc(cap+1);
  s->cap = cap;
  s->len = 0;
  return s;
}

String* new_string() {
  return new_string_cap(1024);
}

String* new_string_by(char* cs) {
  String* s = new_string();
  string_push(s, cs);
  return s;
}

void string_extend(String* s, size_t len) {
  while (s->len + len > s->cap) s->cap *= 2;
  s->data = realloc(s->data, s->cap+1);
}

void string_push(String* s, char* cs) {
  string_extend(s, strlen(cs));
  strcpy(s->data + s->len, cs);
  s->len += strlen(cs);
}

void string_push_int(String* s, int x) {
  char buf[16] = {};
  string_extend(s, 16);
  snprintf(buf, 16, "%d", x);
  strcpy(s->data + s->len, buf);
  s->len += strlen(buf);
}

void string_push_int64(String* s, int64_t x) {
  char buf[64] = {};
  string_extend(s, 64);
  snprintf(buf, 64, "%" PRId64, x);
  strcpy(s->data + s->len, buf);
  s->len += strlen(buf);
}

void string_indent(String* s, int indent) {
  for (int i=0; i<indent; i++) {
    string_push(s, " ");
  }
}
