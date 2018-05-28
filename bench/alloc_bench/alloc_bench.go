
package main

import "runtime"

func pint(x int) *int {
  i := new(int)
  *i = x
  return i;
}

func main() {
  for i := 0; i < 1000000; i++ {
    v := make([]*int, 0, 8)
    v = append(v, pint(1))
    v = append(v, pint(2))
    v = append(v, pint(3))
    v = append(v, pint(4))
    v = append(v, pint(5))
    _ = v
  }
  runtime.GC()
}
