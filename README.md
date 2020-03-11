# Tjr_lru_cache

[TOC]

## Description

Tjr_lru_cache is an LRU cache, a component of tjr_kv and ImpFS. 

Tjr_kv is a key-value store in OCaml. ImpFS will hopefully be a filesystem for OCaml.

Tjr_kv architecture:

<img src="https://docs.google.com/drawings/d/e/2PACX-1vSnTmJGnVDyxnrBZ_VOVZ7T0O9etqZa-BDPu-EPH9ziiNjY375TMgO-ENB9UO4e-HT3qmtbJKvFOFl0/pub?w=453&amp;h=373">



## Note on src/

- in-mem: the basic implementation; maintains a "cache state";
  expressed as state-passing functions; requires help to execute a
  call to the lower layer; API returns result together with a
  typically-empty list of evictees

- multithreaded: includes lists of threads blocked on particular keys
  (multiple reader, single writer); 
  
  The idea is that the first thread that needs to make a "slow" call
  to disk to execute find(k), first creates a list of waiters, then
  calls out to disk; on return, the thread activates all waiters
  before returning; in the meantime, if another thread needs find(k),
  they discover that the call is in process, and so they suspend
  themselves after adding themselves to the wait list


## Performance testing

In directory test/ there are various test programs. One does
exhaustive testing. The other does performance testing. Type `make run_performance_test` to see the output, which resembles:

~~~
find _build -name "test_main.exe" -exec cp \{\} . \;
./test_main.exe -count 10000 -cap 100 -evict 10
count is 10_000 (src/test_performance.ml)
capacity is 100 (src/test_performance.ml)
evict count is 10 (src/test_performance.ml)
Following timings are in nanoseconds
              pqwy functional lru, op=add-and-trim 5_834_736
              pqwy imperative lru, op=add-and-trim 1_178_008
  pqwy imperative lru with batch evict if over cap 1_126_974
            hashtable, with drop when cap exceeded 750_954
            hashtable, with new when exceeding cap 1_515_602
          tjr_lru with automatic evict if over cap 4_611_904
~~~



## Quick links

* [ocamldoc documentation](https://tomjridge.github.io/tjr_lru_cache/)
* <https://github.com/pqwy/lru> - the in-memory code now uses this LRU implementation
* <https://github.com/tomjridge/tjr_kv> - the tjr_kv key-value store library



## Dependencies

| Dependency            | Comment                 |
| --------------------- | ----------------------- |
| tjr_fs_shared         | Common library          |
| tjr_profile_with_core | For testing / profiling |
| minicli, containers   | For testing             |

