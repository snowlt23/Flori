
# Flori

**Warning:** This project is still in experimental.

Flori is statically typed lisp programming language without GC for system/application programming.

# Features

- Native compile via C
- Statically typed
- Automatic resource management by **compile time reference counting** (I call it CTRefCount)
- Interactive development (Common Lisp like)

# Philosophy

- Fast
- Safe
- Small
- Simple
- Easy
- Ultimate power for Metaprogramming

# Inspired languages

- Clojure (Syntax)
- Nim (C Backend, Macro)
- Rust (Semantics)
- Common Lisp (Interactivity)

# Examples

### Hello World
```
(require prelude :refer :all)
(require io :refer :all)

(print "Hello World!\n")
```

### File I/O
```
(require prelude :refer :all)
(require fileio :refer :all)

@(:)
(defn main []
  (var f (open-file "voiceroids.txt" "w"))
  (write f "Yukari!")
  (write f "Maki!")
  (write f "Akane!")
  (write f "Aoi!")
  ;; automatic release file of 'f variable!
  )
(main)
```

# TODO

- Macro system
- Hot Reload
- Disassembler, Debugger, Profiler
- Standard Library
- Build System & Package Manager
- Concurrency
- Self Hosting
