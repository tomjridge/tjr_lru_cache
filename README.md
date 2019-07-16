# Tjr_lru_cache

[TOC]

## Description

Tjr_lru_cache is an LRU cache, a component of tjr_kv and ImpFS. 

Tjr_kv is a key-value store in OCaml. ImpFS will hopefully be a filesystem for OCaml.

Tjr_kv architecture:





<img src="https://docs.google.com/drawings/d/e/2PACX-1vSnTmJGnVDyxnrBZ_VOVZ7T0O9etqZa-BDPu-EPH9ziiNjY375TMgO-ENB9UO4e-HT3qmtbJKvFOFl0/pub?w=453&amp;h=373">





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

