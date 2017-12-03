
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

void vec_main() {
  Vec_Int32 top = new_vec_Int32(1);
  excl_eq(top, 0, 9);
  for (int i = 0; i <= 5; i++) {
    Vec_Int32 v = new_vec_Int32(1);
    excl_eq(top, 0, 9);
    push(v, 10);
    push(v, excl(v, 0));
    destructor_Vec_Int32(top);
    top = v;
  }
  destructor_Vec_Int32(top);
}

void file_main() {
  File f = open_file("voiceroids.txt", "r")
  for (int i = 0; i < len(voiceroids), i++) {
    String v = voiceroids[i];
    write(f, v);
  }
  destructor_File(f);
}

int main() {
  vec_main()
  file_main()
}
