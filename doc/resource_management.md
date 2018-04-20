
# Resource Management

value classification to 2 category.

- copy
- resource

allocate space classification to 3 category.

- heap
- region
- global

# copyable

normal values (example: Int, Float, Vec[copy T])

when substitute, copy value in always.

```
x := 1
y := 2
x = y # copy y to x
```

```
x := v[Int](1, 2, 3)
y := v[Int](4, 5, 6)
x = y # destruct x, copy y to x
```

# resource

resource should be unique, cannot copy. (example: File, Socket)

```
```

# region

```
ret := in_region {
  p := alloc[Int](1) # allocate by region
  unref(p) = 9
  p # copy region value to heap
}
```
