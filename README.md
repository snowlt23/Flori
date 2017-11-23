
# Flori

**Warning:** This project is still in experimental.

Flori is statically typed lisp programming language without GC for system/application programming.

# Features

- Native compile via C. (It's so fast and portable!)
- Statically typed.
- Flexible meta syntax. (like Clojure)
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
(require io :refer :all)

(println "Hello World!")
```

### Fibonacci
```
(require io :refer :all)

(defn fib [^int n] ^int
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(println (fib 38))
```

### File I/O
```
(require fileio :refer :all)

(defn main []
  (let [f (open-file "voiceroids.txt", "w")]
    (write f "Yukari")
    (write f "Maki")
    (write f "Akane")
    (write f "Aoi")
    ;; automatic release file of 'f variable here!))

(main)
```

# TODO

- CTRefCount
- Hot Reload
- Macro system
- Memory Lifting
- Condition System
- Standard Library
- Disassembler, Debugger, Profiler
- Build System & Package Manager
- Concurrency
- Self Hosting
