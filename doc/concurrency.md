# Concurrency

**DRAFT**

Flori has concurrency model with region.  
Flori region is thread local, so region can't data race.  

Flori has mutable reference type system (`ref`), so can detect data race of argument by type check.

So I introduce new pragma, `concurrent`, this is add annotate region block to function, type checking of mutable reference, and mark to function as concurrentional.

## Example

**Correct**

```
fn conc() $[concurrent] {
  v := vec[Int]()
  push(v, THREAD_ID)
}
```

```
# expanded
fn conc() $[concurrent_check] {
  in_region { # thread local region
    v := vec[Int]() # thread local vec
    push(v, THREAD_ID)
  }
}
```

**Illegal**

```
fn conc(v ref Vec[Int]) $[concurrent] {
  push(v, THREAD_ID)
}
```

```
# expanded
fn conc(v ref Vec[Int]) $[concurrent_check] {
  in_region { # thread local region
    push(v, THREAD_ID) # illegal: v is mutable reference value outside of thread. 
  }
}
```
