#include "flori.h"
#include <string.h>

int main() {
  String* s = new_string_cap(5);
  string_push(s, "Yuka");
  string_push(s, "Aka");
  string_push(s, "Ao");
  string_push_int(s, 3);
  assert(strcmp(s->data, "YukaAkaAo3") == 0);
}
