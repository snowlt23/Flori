
# Flori

**Warning**: This project is work in progress. (very early stage)  

Flori is statically typed programming language without GC for system/application programming.

# Features

- Statically typed.
- Blazing fast and easy memory management by **Explicit Region**.
- Flexible meta syntax, It's **F Expression**.
- Standalone native-compiler. (including assembler, linker, optimizer)
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
fn main() {
  print "Hello World!"
}
```

### Fibonacci
```
fn fib(n ^int) ^int {
  if (n < 2) {
    n
  } else {
    (fib(n-1)) + (fib(n-2))
  }
}

fn main() {
  fib 9
}
```

# Usage

<TODO>

# Prebuilt Binaries

<TODO>

# Compiler Instructions

**Requirements**

- GCC (GNU C Compiler)
- Make
- Adhocc (<https://github.com/snowlt23/adhocc>)

```sh
$ make
$ make test
```

# TODO

- **Rewrite compiler** (high priority!)
- Documentation
- Error Handling System
- Concurrency
- Standard Library
- Disassembler, Debugger, Profiler
- Package Manager
- IDE tool
- Self Hosting
