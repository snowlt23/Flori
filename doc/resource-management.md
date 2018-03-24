
# Resource Management

Statically Automatic Resource Management came true by multiple compile time system.

- Compile Time Reference Counting
- Effect System
- Extent Lifting
  - Depend Lifting
  - Loop Lifting by `UniqueVec[T]`
- Explicit Destroy

# Compile Time Reference Counting

```
fn main() {
  f := open_file("test.txt") # f.cnt = 1
  # f.cnt = 0
  # destructor(f)
}
```

# Effect System

```
fn eff(v ref Vec[File], f File) {
  push(v, f)
  # track(v -> f)
}
fn main() {
  v := vec[File]()
  f := open_file("test.txt")
  eff(v, f)
  # track(v -> f)
}
```

# Extent Lifting

**Depend Lifting**
```
fn vec_eff() Vec[File] {
  v := vec[File]()
  f := open_file("test.txt")
  push(v, f)
  v # return v, and f because depend by v
}
fn main() {
  v := vec_eff() # retruned v, and f
  # v.cnt = 1, f.cnt = 2
  # v.cnt = 0, f.cnt = 0
  # destructor(f)
  # destructor(v)
}
```

**Loop Lifting**
```
fn vec_eff() Vec[File] {
  v := vec[File]()
  for i in range(1, 10) {
    f := open_file("test" & to_s(i) & ".txt")
    push(v, f)
  }
  v # return v, and loop.f(UniqueVec[File]) because depend by v
}
fn main() {
  v := vec_eff() # returned v, and loop.f(UniqueVec[File])
  # v.cnt = 1, loop.f.cnt = 2
  # v.cnt = 0, loop.f.cnt = 0
  # destructor(loop.f)
  # destructor(v)
}
```

# Explicit Destroy

```
fn d() File {
  f := open_file("test.txt")
  destructor(f) # explicit destructor call
  f # illegal at compile time, because f is destroyed.
}
fn main() {
  d()
}
```
