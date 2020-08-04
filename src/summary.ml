(** 

Abbreviations: 
  - Im = In Memory
  - Lim = LRU (in memory)
  - mt = Multi-threaded

Main types:

{[
  type 'v lru_entry = 
    | Insert of { value: 'v; dirty:bool }
    | Delete of { dirty:bool }
    | Lower of 'v option

  type ('k,'v,'lru,'t) lru_factory = <
    empty :  lru_params -> 'lru;

    make_ops : 
      with_state : ('lru,'t) with_state ->
      to_lower   : (('k,'v,'t) lru_msg -> (unit,'t)m) -> 
      ('k,'v,'t)mt_ops;

  >

  type ('k,'v,'t) lru_msg = 
    | Insert   of 'k*'v*(unit -> (unit,'t)m)
    | Delete   of 'k*(unit -> (unit,'t)m)
    | Find     of 'k * ('v option -> (unit,'t)m)
    | Evictees of ('k,'v)kvop list

  type ('k,'v,'t) mt_ops = {
    mt_find          : 'k -> ('v option,'t) m; 
    mt_insert        : 'k -> 'v -> (unit,'t) m;
    mt_delete        : 'k -> (unit,'t) m;
    mt_sync_key      : 'k -> (unit,'t) m;
    mt_sync_all_keys : unit -> (unit,'t) m;
  }

]}

Examples in {!Lru_examples}.

*)
