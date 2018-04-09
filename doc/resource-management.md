
# Resource Management System

Statically Automatic Resource Management came true by multiple compile time system.

- compile time reference counting (CTRC)
- effect system
- extent lifting
- field tracking
- explicit destruct
- dynamic type
- dynamic type polymorphism
- ctrc polymorphism

# Compile Time Reference Counting

```
fn main() {
  f := open_file("test.txt") # f.cnt = 1
  # f.cnt = 0
  # destruct(f)
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
  # destruct(f)
  # destruct(v)
}
```

# Field Tracking

# Explicit Destruct

```
fn d() File {
  f := open_file("test.txt")
  destruct(f) # explicit destructor call
  f # illegal at compile time, because f is destroyed.
}
fn main() {
  d()
}
```

# Dynamic Type

- unique
- borrow
- share
- pool

**share value wrapped by ShareCont[T]**

```
type ShareCont[T] {
  owned Bool
  value T
}
destructor[T](sc ShareCont[T]) {
  if (sc.owned) {
    destruct(sc.value)
  }
}
```

**Example Type**

```
const TableSize := 1024
type Table[K, V] {
  data Array[TableSize, dynamic V]
}
fn set[K, V](table Table[K, V], key K, value V) {
  set(table.data, hash(key), value)
}

type Enemy {
  hp Int
  mp Int
}
```

**Rules**
- if all set value has uniqueness, apply unique to dynamic

```
# example
fn spawn_enemy(tbl Table[String, Enemy], name String, hp Int, mp Int) {
  e := init(Enemy){hp; mp}
  set(tbl, name, e) # e has uniqueness!
}
fn main() {
  tbl := new_table[String, Enemy]() # tbl.typ = Table[String, unique Enemy]
  for i in range(1, 10) {
    spawn_enemy(tbl, "Zombie" & to_s(i), i * 100, i * 10)
  }
}
```

- if all set value has borrowness, apply borrow to dynamic

```
fn main() {
  enemies := vec[Enemy]() # enemies.typ = Vec[unique Enemy]
  for i in range(1, 10) {
    push(enemies, init(Enemy){i*100; i*10})
  }
  tbl := new_table[String, Enemy]() # tbl.typ = Table[String, borrow Enemy]
  for i in range(0, length(enemies)-1) {
    set(tbl, "Zombie" & to_s(i), get(enemies, i)) # set value has borrowness!
  }
}
```

- if value has mixed uniqueness and borrowness, apply share to dynamic

```
# example
fn main() {
  enemies := vec[Enemy]() # enemies.typ = Vec[unique Enemy]
  for i in range(1, 10) {
    push(enemies, init(Enemy){i*100; i*10})
  }
  tbl := new_table[String, Enemy]() # tbl.typ = Table[String, share Enemy] => Table[String, ShareCont[Enemy]]
  for i in range(0, length(enemies)-1) {
    set(tbl, "Zombie" & to_s(i), get(enemies, i)) # value is borrow
  }
  set(tbl, "BossZombie", init(Enemy){10000; 1000}) # value is unique
}
```

- if dynamic value partial returned, apply share to dynamic and set false to `owned field.

```
# example
fn spawn_enemy(tbl Table[String, Enemy], name String, hp Int, mp Int) {
  e := init(Enemy){hp; mp}
  set(tbl, name, e) # set value has uniqueness!
}
fn get_first_enemy() Enemy {
  tbl := new_table[String, Enemy]() # tbl.typ = Table[String, share Enemy] => Table[String, ShareCont[Enemy]]
  for i in range(1, 10) {
    spawn_enemy(tbl, "Zombie" & to_s(i), 100 * i, 10 * i)
  }
  return tbl.data[0] # tbl.data[0].owned = false
}
fn main() {
  fe := get_first_enemy() # fe is unique
}
```

share-value manage resource by reference counting. (RC)
reference counting decide `initial-value, `increment-amount and `decrenment-amount at compile time.

# Dynamic Type Polymorphism
# CTRC Polymorphism (for know refcount amount at compile time)
