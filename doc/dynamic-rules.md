
# Dynamic Rules

**Type is default unique mode**
```
const TableSize := 1024
# Table is unique mode by default
type Table[K, V] {
  data Array[TableSize, Dynamic[V]]
}

# if unique
type Table[K, V] {
  data Array[TableSize, V]
}
# if sharable
type Table[K, V] {
  data Array[TableSize, Rc[V]]
}
```

**Function can apply effect to variable type**
```
fn set[K, V](table Table[K, V], key K, value unique V) {
  get(table.data, hash(key)) = value
}
# if not unique value, apply sharable mode effect to table type.
fn set[K, V](table Table[K, V], key K, value share V) {
  effect {
    sharable -> table
  }
  get(table.data, hash(key)) = value
}
```

**Function known variable type uniqueness.**
```
fn del[K, V](table Table[K, V], key K) {
  when (is_unique(table)) {
    destruct(get(table.data, hash(key)))
  }
  get(table.data, hash(key)) = empty[V]()
}
```

**Explicit Uniqueness**
```
type Ctx {
  enemytable unique Table[String, Enemy]
}
fn spawn_enemy(ctx ref Ctx, name String, e Enemy) {
  set(ctx.enemytable, name, e) # e should be unique value
}
fn kill_enemy(ctx ref Ctx, name String) {
  del(ctx.enemytable, name)
}
```

**Sharable Lifting by scope level**
```
fn spawn_enemy() {
  tbl := table[String, Enemy]()
  e := enemy(100, 10)
  set(tbl, "ZombieA", e) # unique value
  block {
    g := graph[Enemy](e) # sharable value, but `g scope level is lower than `tbl, so `tbl has keep uniqueness.
  }
}
```
