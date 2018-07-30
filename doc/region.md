
# Explicit Region

What is Region? Region has some names (Arena, Zone...), I call it Region here.
Region definition is a type of value lifetime (in my language).

Why Region as core of Flori? How Region be useful?

Region has some pros.

- Determine
- No memory leak in Region range.
- Implementation is smaller and simple. (better GC written is hard work.)
- Faster than dynamic memory allocation. (malloc/free is costly function.)

Region is good memory management solution in this way, but Why many languages hasn't Region system?  
Region isn't silver bullet (or gungnir) for memory management.

Region has a little cons. (but important)

- Region can't control dynamic lifetime allocations. (Region is lexical/dynamic scope memory management method.)

How does we this to better? First, dynamic lifetime allocation needs dynamic memory management. So we should use dynamic memory management in dynamic lifetime allocations. (So Rust has Rc<T>, etc.. for this. other languages has GC.)

## Solution

We have some Solution.

- Memory Pool
- Object Pool

