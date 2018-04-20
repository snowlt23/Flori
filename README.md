
# Flori

**Warning:** This project is still in experimental.

Flori is statically typed programming language without GC for system/application programming.

# Features

- Statically typed.
- Automatic resource management by **Owned Marking**.
- Flexible meta syntax, It's **F Expression**.
- Native compile via C. (It's so fast and portable!)
- Blazing fast memory management by **Memory Lifting**.
- Ultimate power for Metaprogramming.
- Interactive development environment. (Common Lisp SLIME like)

# Inspired languages

- Nim
- REBOL
- Clojure
- Rust
- Common Lisp

# Examples

### Hello World
```
import "core"

println("Hello World!")
```

### Fibonacci
```
import "core"

fn fib(n Int) Int {
  if (n < 2) {
    n
  } else {
    fib(n-1) + fib(n-2)
  }
}

println(fib(38))
```

### File I/O
```
import "core"
import "std/fileio"

fn main() {
  f := openFile("voiceroids.txt", "w")
  write(f, "Yukari")
  write(f, "Maki")
  write(f, "Akane")
  write(f, "Aoi")
  # automatic release file of `f variable here!
}

main()
```

# Usage

```sh
$ flori c <filename>
```

# Compiler Instructions

**Requirements**

- Nim (devel branch)
  - nimble
- GCC (GNU C Compiler)
- TCC (Tiny C Compiler)

```sh
# Build
$ nimble install nake
$ nimble install docopt
$ nake build
$ export PATH="$PATH:<flori-dir>/bin"
```

```sh
# Test
$ nake test
```

# TODO

- Condition System
- Memory Lifting
- Type level macro
- Concurrency
- Standard Library
- Reduction Build Times
- LiveReload
- Disassembler, Debugger, Profiler
- Build System & Package Manager
- IDE tool (Rabbit)
- Self Hosting
