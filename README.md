
# Flori

**Warning**: This project is work in progress. (very early stage)

Flori is statically typed programming language without GC for system/application programming.

# Features

- Simple
- Statically typed with type inference.
- AOT/JIT native code generation.
  - x86 (in future)
  - x86_64 (WIP)
  - WASM (in future)
  - ARM (in future)
  - RISC-V (in future)
- Easy cross compilation.
- Blazing fast and easy memory management by **Explicit Region**. (no GC)
- Flexible meta syntax, It's **F Expression**.
- Code-generation as a abstraction tool.
- Interactive development with REPL and Editor.

# Inspired languages

- C
- Golang
- OCaml
- bone-lisp
- xtlang
- Common Lisp

# Examples

### Hello World
```
println("Hello World")
```

### Fibonacci
```
fib(n) =>
  if n<2: n
  else: fib(n-1) + fib(n-2)
println(fib(38))
```

# Usage
**TODO**

# Prebuilt Binaries

~~[Releases](https://github.com/snowlt23/Flori/releases)~~ (rewrite-compiler branch has still in development)

# Compiler Instructions

**Requirements**

- gcc
- make

```sh
# build
$ make
```

```sh
# test
$ make test
```

# Language Plan

- Compiler Tools for Editor/IDE
- Continuation (or Return Stack)
- Concurrency
- Standard Library
- Disassembler, Debugger, Profiler
- Region Profiler
- Package Manager
- Self Hosting
