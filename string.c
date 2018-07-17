#include <stdlib.h>
#include <string.h>

char* string_copy(char* s) {
  char* copied = malloc((strlen(s)+1) * sizeof(char));
  strcpy(copied, s);
  return s;
}
