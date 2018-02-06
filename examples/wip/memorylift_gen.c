
typedef struct {
  void* p;
  size_t position;
  size_t size;
} Heap;

Heap new_heap(size_t size) {
  Heap heap;
  heap.p = malloc(size);
  heap.position = 0;
  heap.size = size;
  return heap;
}

void delete_heap(Heap heap) {
  free(heap.p);
}

size_t heap_position(Heap heap) {
  return heap.position;
}

void heap_rollback(Heap heap, size_t position) {
  heap.position = position;
}

void* heap_alloc_noinit(Heap heap, size_t size) {
  size_t curpos = heap.position;
  heap.position += size;
  return p + curpos;
}

void* heap_alloc(Heap heap, size_t size) {
  void* p = heap_alloc_noinit(heap, size);
  memset(p, 0, size);
  return p;
}

Heap flori_heap;

//
// generated
//

typedef struct {
  int32_t* p;
  int32_t len;
} Vec_Int32;

Vec_Int32 new_vec_Int32(int32_t len) {
  return Vec_Int32{heap_alloc(flori_heap, sizeof(int32_t)*len), len};
}

void excl_eq(Vec_Int32* vec, int32_t index, int32_t value) {
  v->p[index] = value;
}

void push(Vec_Int32* vec, int32_t value) {
  vec->p = realloc(vec->p, sizeof(int32_t)*(vec->len+1));
  set_nth_excl(&v, vec->len, value);
  vec->len = vec->len + 1;
}

void memorylift_main() {
  size_t __flori_heappos = heap_position(flori_heap);
  Vec_Int32 top = new_vec_Int32(Int32, 1);
  for (int i = 0; i <= 5; i++) {
    size_t __flori_heappos = heap_position(flori_heap);
    int32_t len_tmp = 1;
    Vec_Int32 v = new_vec_Int32(len_tmp);
    excl_eq(&v, 0, 9);
    push(&v, 10);
    heap_rollback(flori_heap, __flori_heappos);
    top = v;
  }
  heap_rollback(flori_heap, __flori_heappos);
}

void memorylift_init() {
  memorylift_main();
}

int main() {
  heap = new_heap(1048576);
  memorylift_init();
}
