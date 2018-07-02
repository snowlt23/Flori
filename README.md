
# Flori

**Warning**: This project is work in progress. (very early stage)

Flori is statically typed programming language without GC for system/application programming.

# Features

- Statically typed.
- Concise syntax with type/argument inference.
- AOT/JIT native code generation.
  - x86
  - x86_64 (in future)
  - WASM (in future)
  - ARM (in future)
  - RISC-V (in future)
- Easy cross compilation.
- Blazing fast and easy memory management by **Explicit Region**. (no GC)
- Flexible meta syntax, It's **F Expression**.
- Metaprogramming support. (user defined macro/syntax)
- Interactive development with REPL and Editor.

# Inspired languages

- Nim
- Standard ML
- bone-lisp
- xtlang
- REBOL
- Clojure
- Common Lisp

# Examples

### Hello World
```
import core

println("Hello World")
```

### Fibonacci
```
import core

fib =>
  if n<2: n
  else: fib(n-1) + fib(n-2)
println(fib(38))
```

Other examples, please reference `Flori/examples` or `Flori/tests/floritests`.

# Usage
**TODO**

# Prebuilt Binaries

~~[Releases](https://github.com/snowlt23/Flori/releases)~~ (image-compiler branch has still in development)

# Compiler Instructions

**Requirements**

- Nim (0.18.0)
  - nimble

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

# Language Plan

- Compiler Tools for Editor/IDE
- Continuation
- Concurrency
- Standard Library
- Disassembler, Debugger, Profiler
- Region Profiler
- Package Manager
- Self Hosting
