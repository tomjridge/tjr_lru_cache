(** 

Abbreviations: 
  - Im = In Memory
  - Lim = LRU (in memory)
  - mt = Multi-threaded

Main types:

{[
  type ('k,'v,'lru,'t) lru_factory = <
    empty :       
      max_size    : int -> 
      evict_count : int -> 
      'lru;

    make_ops : 
      with_state : ('lru,'t) with_state ->
      to_lower   : (('k,'v,'t) lru_msg -> (unit,'t)m) -> 
      ('k,'v,'t)mt_ops;

  >

  type ('k,'v,'t) lru_msg = 
    | Insert of 'k*'v*(unit -> (unit,'t)m)
    | Delete of 'k*(unit -> (unit,'t)m)
    | Find of 'k * ('v option -> (unit,'t)m)
    | Evictees of ('k * 'v entry) list

  type ('k,'v,'t) mt_ops = {
    mt_find          : 'k -> ('v option,'t) m; 
    mt_insert        : persist_mode -> 'k -> 'v -> (unit,'t) m;
    mt_delete        : persist_mode -> 'k -> (unit,'t) m;
    mt_sync_key      : 'k -> (unit,'t) m;
    mt_sync_all_keys : unit -> (unit,'t) m;
  }

]}

Examples in {!Lru_examples}.

*)
