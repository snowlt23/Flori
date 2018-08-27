
# Flori

**Warning**: This project is work in progress. (very early stage)  

Flori is statically typed programming language without GC for system/application programming.

# Features

- Statically typed.
- Blazing fast and easy memory management by **Explicit Region**.
- Flexible meta syntax, It's **F Expression**.
- Native compile via C. (It's so fast and portable!)
- Ultimate power for Metaprogramming.
- Interactive development environment. (Common Lisp SLIME like)

# Inspired languages

- Nim
- Forth
- bone-lisp
- xtlang
- REBOL
- Clojure
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

Other examples, please reference [/examples](https://github.com/snowlt23/Flori/tree/master/examples) or [/tests/floritests](https://github.com/snowlt23/Flori/tree/master/tests/floritests).

# Usage

```sh
$ flori c <filename>
```

# Prebuilt Binaries

[Releases](https://github.com/snowlt23/Flori/releases)

# Compiler Instructions

**Requirements**

- Nim (0.18.0)
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

- Documentation
- Error Handling System
- LiveReload
- ImageDump
- Concurrency
- Standard Library
- Reduction Build Times
- Disassembler, Debugger, Profiler
- Region Profiler
- Package Manager
- IDE tool (Rabbit)
- Self Hosting
