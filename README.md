# Flori

**Warning**: This project is work in progress. (very early stage)

Flori is statically typed low fat programming language without GC for system/application programming.

# Features

- Low fat computing.
- Statically typed.
- Blazing fast and easy memory management by **Explicit Region**.
- User can be defining original syntax.
- Standalone native-compiler. (including assembler, linker, optimizer)
- Ultimate power for Metaprogramming.
- Interactive development environment. (Common Lisp and Forth like)

# Inspired languages

- Forth
- Lisp-family
- C
- Nim

# Examples

### Hello World
```
fn main() {
  print("Hello World!")
}
```

### Fibonacci
```
fn fib(n ^int) ^int {
  if n < 2 {
    n
  } else {
    fib(n - 1) + fib(n - 2)
  }
}

fn main() {
  fib(9)
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
