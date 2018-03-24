
# Flori

**Warning:** This project is still in experimental.

Flori is statically typed programming language without GC for system/application programming.

# Features

- Native compile via C. (It's so fast and portable!)
- Statically typed.
- Flexible meta syntax. It's F Expression.
- Automatic resource management by **Compile Time Reference Counting**. (I call it CTRC)
- Blazing fast memory management by **Memory Lifting**.
- Ultimate power for Metaprogramming.
- Interactive development environment. (Common Lisp like)

# Inspired languages

- Nim
- REBOL
- Clojure
- Rust
- Common Lisp

# Examples

### Hello World
```
import "core/prelude"
import "core/io"

println("Hello World!")
```

### Fibonacci
```
import "core/prelude"
import "core/io"

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
import "core/prelude"
import "core/fileio"

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

# Build

**Requirements**

- Nim (devel branch)
  - nimble
- GCC (GNU C Compiler)
- TCC (Tiny C Compiler)

```sh
# Build
$ nimble install docopt
$ nim c -d:release compiler/flori.nim
$ export PATH="$PATH:<flori-dir>/bin"
```

```sh
# Test
$ nim c -r tests/tester.nim
```

# TODO

- Loop Lifting
- Condition System
- Memory Lifting
- Type level macro
- Standard Library
- Reduction Build Times
- LiveReload
- Disassembler, Debugger, Profiler
- Build System & Package Manager
- IDE tool (Rabbit)
- Concurrency
- Self Hosting
