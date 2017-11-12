
# Flori

**Warning:** This project is still in experimental.

Flori is statically typed programming language without GC for system/application programming.

# Features

- Native compile via C
- Statically typed
- Flexible meta syntax, it's F expression.
- Automatic resource management by **Compile Time Reference Counting** (I call it CTRC)
- Blazing fast memory management by **Memory Lifting**.
- Interactive development (Common Lisp like)

# Philosophy

- Fast
- Safe
- Small & Simple
- Easy
- Interactive
- Ultimate power for Metaprogramming

# Inspired languages

- Nim
- REBOL
- Clojure
- Rust
- Common Lisp

# Examples

### Hello World
```
import io

println("Hello World!")
```

### File I/O
```
import fileio

fn main() {
  var f = openFile("voiceroids.txt, "w")
  write(f, "Yukari")
  write(f, "Maki")
  write(f, "Akane")
  write(f, "Aoi")
  # automatic release file of 'f variable here!
}

main()
```

# TODO

- Improve CTRefCount
- Macro system
- Hot Reload
- Disassembler, Debugger, Profiler
- Standard Library
- Build System & Package Manager
- Concurrency
- Self Hosting
