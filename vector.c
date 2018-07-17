#include <stdlib.h>
#include <assert.h>
#include "flori.h"

vector* new_vector_cap(int cap) {
  vector* v = (vector*)malloc(sizeof(vector));
  v->data = (void**)malloc(cap*sizeof(void*));
  v->cap = cap;
  v->len = 0;
  return v;
}
vector* new_vector() {
  return new_vector_cap(256);
}

void vector_extend(vector* v) {
  if (v->cap < v->len+1) {
    v->data = (void**)realloc(v->data, v->cap*2*sizeof(void*));
    v->cap *= 2;
  }
}

void* vector_get(vector* v, int index) {
  assert(0 <= index && index < v->len);
  return v->data[index];
}
void vector_set(vector* v, int index, void* elem) {
  assert(0 <= index && index < v->len);
  v->data[index] = elem;
}

void vector_push(vector* v, void* elem) {
  vector_extend(v);
  v->len += 1;
  vector_set(v, v->len-1, elem);
}
