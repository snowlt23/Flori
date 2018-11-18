# new semantics

## Region Type Symbols

- HR: Higher Region
- R1: Region level 1
- R2: Region level 2
- Rn: Region level n
- LR: Local Region
- R1 <- R2: set R1 region variable from R2 region.
- return R: return value from R by copy.

## Region Type Rules

- R1 <- R1 correct
- R2 <- R1 correct
- HR <- HR correct
- R1 <- HR correct
- R1 <- R2 incorrect!
- HR <- R1 incorrect!
- R1 <- return R2 correct
- HR <- return R1 correct
- LR <- R1 incorrect!
- LR <- HR incorrect!
- LR <- LR incorrect!
- LR <- return R1 correct
- LR <- return HR correct
- LR <- return LR correct

## Solution1: Do(Loop) notation

```
struct enemy {
  hp ^int
  name ^string
}

struct state {
  enemies ^map[^enemy]
}

fn copy(r ^region, st ^state) ^state {
  init ^state {
    enemies: copy(r, st.enemies)
  }
}

fn update(st ^ref state) {
  msg := get_msg()
  if (msg.kind == SPAWN) {
    insert(st.enemies, msg.name, new_enemy())
  } elif (msg.kind == KILL) {
    delete(st.enemies, msg.name)
  }
}

fn main_loop() {
  loop [st <- init_state()] {
    update(st)
  }
}
# converted by macro =>
fn main_loop() {
  block {
    lr1 := new_region()
    lr2 := new_region()
    st'lr1 := init_state()
    unsafe_while(true) {
      in_reg {
        update(st'lr1)
      }
      if (is_over(lr1)) {
        swap(lr1, lr2)
        st = copy(lr1, st)
        unsafe_clear(lr2)
      }
    }
  }
}
```
