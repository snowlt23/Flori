
# Flori

**Warning:** This project is still in experimental.

Flori is statically typed lisp programming language without GC for system/application programming.

# Features

- Native compile via C. (It's so fast and portable!)
- Statically typed.
- Flexible meta syntax (like Clojure), it's **F expression**.
- Automatic resource management by **Compile Time Reference Counting**. (I call it CTRC)
- Blazing fast memory management by **Memory Lifting**.
- Ultimate power for Metaprogramming.
- Interactive development. (Common Lisp like)

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

### Fibonacci
```
import io
(require :io)

(defn fib [^int n] ^int
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(println (fib 38))
```

### File I/O
```
(require :fileio)

(defn main []
  (let [f (open-file "voiceroids.txt", "w")]
    (write f "Yukari")
    (write f "Maki")
    (write f "Akane")
    (write f "Aoi")
    ;; automatic release file of 'f variable here!))

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
