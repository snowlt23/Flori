#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "vector.c"

int* pint(int x) {
  int* p = (int*)malloc(sizeof(int));
  *p = x;
  return p;
}

int main() {
  vector* v = new_vector_cap(1);
  vector_push(v, pint(1));
  vector_push(v, pint(2));
  vector_push(v, pint(3));
  vector_push(v, pint(4));
  vector_push(v, pint(5));

  assert(1 == *(int*)vector_get(v, 0));
  assert(2 == *(int*)vector_get(v, 1));
  assert(3 == *(int*)vector_get(v, 2));
  assert(4 == *(int*)vector_get(v, 3));
  assert(5 == *(int*)vector_get(v, 4));
  assert(5 == v->len);

  return 0;
}
