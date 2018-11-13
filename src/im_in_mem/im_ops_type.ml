(** A record to package up the functions from {! Lru_in_mem}. *)
open Entry.Types
open Im_cache_state.Types

(** This type is what is returned by the [make_lru_in_mem]
    function. 

    NOTE that [sync_key] performs only the in-mem updates (ie clearing the dirty flag). If you want to flush to disk, you have to do something else (see {!Lru_multithreaded}).
*)
type ('k,'v,'t) lru_in_mem_ops = {
  find: 'k -> ('k,'v) cache_state -> 
    [ `In_cache of 'v entry
    | `Not_in_cache of
        vopt_from_lower:'v option ->
        cache_state:('k, 'v) cache_state ->
        'v option * 
        [ `Evictees of ('k * 'v entry) list option ] *
        [ `Cache_state of ('k, 'v) cache_state ] ];

  insert: 'k -> 'v -> ('k,'v) cache_state ->
    [ `Evictees of ('k * 'v entry) list option ] *
    [ `Cache_state of ('k, 'v) cache_state ];

  delete: 'k -> ('k, 'v) cache_state ->
    [ `Evictees of ('k * 'v entry) list option ] *
    [ `Cache_state of ('k, 'v) cache_state ];

  sync_key: 'k -> ('k, 'v) cache_state -> [ `Not_present | `Present of 'v entry * ('k, 'v) cache_state ]

}
