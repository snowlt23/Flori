
# Onwed Marking Resource Management

- owned marking with scope level
- explicit destruct
- dynamic type
- dynamic type polymorphism

# owned marking

**basic**
```
fn main() {
  f := open_file("test.txt") # compiletime: f.owned = true
  # destruct(f)
}
```

**function**
```
fn owned_fn() {
  f := open_file("test.txt") # compiletime: f.owned = true
  return f # compiletime: f.owned = false
}

fn main() {
  f := owned_fn() # compiletime: f.owned = true
  # destruct(f)
}
```

**field**
```
type FiledEnemy {
  hp Int
  file File
}
fn field_fn() {
  e := init(FiledEnemy){100; open_file("enemy1.txt")} # e.owned = true, e.hp.owned = true, e.file.owned = true
  return e.file # e.file.owned = false
  # destruct(e.hp)
  # destruct(e)
}
fn main() {
  
}
```

# explicit destruct

```
fn exfn() File {
  f := open_file("test.txt")
  destruct(f) # explicit destruct call
  return f # illegal at compile time, because f has been destroyed.
}
fn main() {
  exfn()
}
```

# dynamic type

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

- if all set value has uniqueness, apply unique to dynamic

```
# example
fn spawn_enemy(tbl Table[String, Enemy], name String, hp Int, mp Int) {
  e := init(Enemy){hp; mp}
  set(tbl, name, e) # e has uniqueness! # compiletime: value.owned = true
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
    push(enemies, init(Enemy){i*100; i*10}) # value.owned = true
  }
  tbl := new_table[String, Enemy]() # tbl.typ = Table[String, borrow Enemy]
  for i in range(0, length(enemies)-1) {
    set(tbl, "Zombie" & to_s(i), get(enemies, i)) # set value has borrowness! compiletime: value.owned = false, because tbl.scope.level <= enemies.scope.level
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
    set(tbl, "Zombie" & to_s(i), get(enemies, i)) # value is borrow, runtime: value.owned = false
  }
  set(tbl, "BossZombie", init(Enemy){10000; 1000}) # value is unique, runtime: value.owned = true
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
    spawn_enemy(tbl, "Zombie" & to_s(i), 100 * i, 10 * i) # runtime: value.owned = true
  }
  return tbl.data[0] # runtime: tbl.data[0].owned = false
}
fn main() {
  fe := get_first_enemy() # fe is unique
}
```

# dynamic type polymorphism
