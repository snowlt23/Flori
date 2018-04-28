
#include <stdlib.h>
#include <vector>

int* pint(int x) {
  int* i = (int*)malloc(sizeof(int));
  *i = x;
  return i;
}

int main() {
  for (int i = 0; i < 1000000; i++) {
    std::vector<int*> v;
    v.push_back(pint(1));
    v.push_back(pint(2));
    v.push_back(pint(3));
    v.push_back(pint(4));
    v.push_back(pint(5));
  }
}
