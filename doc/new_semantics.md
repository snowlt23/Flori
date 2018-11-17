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

## Do(Loop) notation

```
struct enemy {
  hp ^int
  name ^string
}

struct state {
  enemies ^map[^enemy]
}

fn update(st ^state) ^state {
  msg := get_msg()
  if (msg.kind == SPAWN) {
    inherit(st) {
      enemies: insert(st.enemies, msg.name, new_enemy())
    }
  } elif (msg.kind == KILL) {
    inherit(st) {
      enemies: delete(st.enemies, msg.name)
    }
  }
}

fn main_do() {
  do [st <- init_state()] {
    st <- update(st)
    st <- update(st)
    st <- update(st)
    st <- update(st)
    return st
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
    st_reg1 := new_region()
    st_reg2 := new_region()
    local_reg(st_reg1) {
      st := init_state()
    }
    unsafe_while(true) {
      local_reg(st_reg1) {
        st = in_reg {
          update(st)
        }
      }
      swap(st_reg1, st_reg2)
      unsafe_clear(st_reg1)
    }
  }
}
```
