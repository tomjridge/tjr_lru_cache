(** Main interfaces *)

module Entry = struct

  module Internal = struct
    (** The cache maintains an internal clock. *)
    type time = int

    (** Entries are marked using a bool; true means "this is dirty". *)
    type dirty = bool
  end
  open Internal

  
  module Export = struct
    (** Cache map entries; values in the map are tagged with a last-accessed time and a dirty flag

Entries in the cache for key k:

- Insert v (dirty=true/false)
    {ul {li this occurs on insert}}
- Delete   (dirty=true/false)
- Lower vopt 
    {ul {li this occurs when we check the lower layer for a non-existing entry in cache}}
- (No entry)
    {ul {li for a key that hasn't been seen before}}

Additionally, each entry has a last-accessed time

 *)
    type 'v entry_type = 
      | Insert of { value: 'v; dirty:dirty }
      | Delete of { dirty:dirty }
      | Lower of 'v option

    type 'v entry = { entry_type: 'v entry_type; atime: time }
  end
  open Export
  
  (* fns on entries and entry_types ------------------------------------ *)

  let is_Lower = function Lower _ -> true | _ -> false 

  let mark_clean = function
    | Insert {value;dirty} -> Insert {value;dirty=false}
    | Delete {dirty} -> Delete {dirty=false}
    | Lower vopt -> Lower vopt

  let entry_type_is_dirty = function
    | Insert {value;dirty} -> dirty
    | Delete {dirty} -> dirty
    | Lower vopt -> false

end
include Entry.Export
open Entry.Internal




module Cache_state = struct

  (** We maintain a queue as a map from time to key that was accessed at
      that time. *)
  module Queue = Map_int


(** The [cache_state] consists of:

- [max_size]: the max number of entries in the cache

- [evict_count]: number of entries to evict when cache full; for
   [tjr_kv] performance is best when the [evict_count] is such that
   the evictees fit nicely in a block

- [current_time]: the current time (monotonically increasing); increased on
   each operation

- [cache_map]: the cache entries, a map from key to ['v value]

- [queue]: a map from time to key that was accessed at that time; only
   holds the latest time a key was accessed (earlier entries for key k
   are deleted when a new operation on k occurs).


NOTE the [queue] field allows to identify the least recently used without
walking the entire map.

NOTE the ['k_map] parameter is a map from k to v entry

*)
  type ('k,'v,'k_map) cache_state = {  
    max_size: int;
    evict_count: int; (* number to evict when cache full *)
    current_time: time;
    cache_map_ops: ('k,'v entry,'k_map) Tjr_map.map_ops;  
    cache_map:'k_map;
    queue: 'k Queue.t; 
    (** map from time to key that was accessed at that time *)
  }
end
include Cache_state





(** A record to package up the functions from {! Lru_in_mem}. *)

(** This type is what is returned by the [make_lru_in_mem]
    function. 

    NOTE that [sync_key] performs only the in-mem updates (ie clearing the dirty flag). If you want to flush to disk, you have to do something else (see {!Lru_multithreaded}).
*)
module Lru_in_mem_ops = struct
  type ('k,'v,'k_map,'t) lru_in_mem_ops = {
    find: 'k -> ('k,'v,'k_map) cache_state -> 
      [ `In_cache of 'v entry
      | `Not_in_cache of
          vopt_from_lower:'v option ->
          cache_state:('k, 'v,'k_map) cache_state ->
          'v option * 
          [ `Evictees of ('k * 'v entry) list option ] *
          [ `Cache_state of ('k, 'v, 'k_map) cache_state ] ];

    insert: 'k -> 'v -> ('k,'v,'k_map) cache_state ->
      [ `Evictees of ('k * 'v entry) list option ] *
      [ `Cache_state of ('k, 'v,'k_map) cache_state ];

    delete: 'k -> ('k, 'v,'k_map) cache_state ->
      [ `Evictees of ('k * 'v entry) list option ] *
      [ `Cache_state of ('k, 'v,'k_map) cache_state ];

    sync_key: 'k -> ('k, 'v,'k_map) cache_state -> [ `Not_present | `Present of 'v entry * ('k, 'v,'k_map) cache_state ]

  }
end
(* include Lru_in_mem_ops ; NOTE not included by default *)
