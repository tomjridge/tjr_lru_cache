(** Main lru-in-mem interfaces; usually don't open this *)

module Pvt_dirty = struct
  (** Entries are marked using a bool; true means "this is dirty". *)
  type dirty = bool
end
open Pvt_dirty

(** Cache map entries; values in the map are tagged with a
    last-accessed time and a dirty flag

Entries in the cache for key k:

- Insert v (dirty=true/false)
    {ul {li this occurs on insert}}
- Delete   (dirty=true/false)
- Lower vopt 
    {ul {li this occurs when we check the lower layer for a
    non-existing entry in cache; if we find a value, we insert Lower
    (Some v), else Lower None; in either case, there is no need to do
    anything further (ie the entry is not dirty) }}
- (No entry)
    {ul {li for a key that hasn't been seen before}}

Additionally, each entry has a last-accessed time

 *)
type 'v entry = 
  | Insert of { value: 'v; dirty:dirty }
  | Delete of { dirty:dirty }
  | Lower of 'v option

module Entry = struct
  let is_Lower = function Lower _ -> true | _ -> false 

  let mark_clean = function
    | Insert {value;dirty} -> Insert {value;dirty=false}
    | Delete {dirty} -> Delete {dirty=false}
    | Lower vopt -> Lower vopt

  let entry_is_dirty = function
    | Insert {value;dirty} -> dirty
    | Delete {dirty} -> dirty
    | Lower vopt -> false
end

(** The LRU in-memory cache state consists of:

- [max_size]: the max number of entries in the cache

- [evict_count]: number of entries to evict when cache full; for
   [tjr_kv] performance is best when the [evict_count] is such that
   the evictees fit nicely in a block

- [lru_state]: the lru state

*)
  type ('k,'v,'lru) lim_state = {  
    max_size: int;
    evict_count: int; (* number to evict when cache full *)
    lru_state:'lru;
    compare_lru:'lru -> 'lru -> int
  }



(** A record to package up the functions from {! Lru_in_mem}. *)

type ('a,'b) maybe_in_cache = In_cache of 'a | Not_in_cache of 'b

type ('k,'v,'lru) evictees_x_lim_state = {
  evictees: ('k * 'v entry) list option;
  lim_state: ('k,'v,'lru) lim_state
}

(** This type is what is returned by the [make_lru_in_mem]
    function. 

    NOTE that [sync_key] performs only the in-mem updates (ie clearing
    the dirty flag). If you want to flush to disk, you have to do
    something else (see {!Lru_multithreaded}).

    NOTE find returns a "maybe_in_cache", which consists of a function
    which tries to retrieve from below, and then on return updates the
    then-current lim_state (which may already have the entry...)

    NOTE the interface tries to favour pure state passing; find is the
    only routine which may need to call to disk

    NOTE a struct so that we can scope record fields as Lim_ops.\{ ... \}
*)
module Lim_ops = struct
  type ('k,'v,'lru) lim_ops = {
    find: 'k -> ('k,'v,'lru) lim_state -> 
      ('v entry, 
       vopt_from_lower:'v option ->
       lim_state:('k, 'v,'lru) lim_state ->
       'v option * 
       ('k,'v,'lru) evictees_x_lim_state ) maybe_in_cache;

    insert: 'k -> 'v -> ('k,'v,'lru) lim_state ->
      ('k,'v,'lru) evictees_x_lim_state;

    delete: 'k -> ('k, 'v,'lru) lim_state ->
      ('k,'v,'lru) evictees_x_lim_state;

    sync_key: 'k -> ('k, 'v,'lru) lim_state -> 
      ('v entry * ('k, 'v,'lru) lim_state, unit) maybe_in_cache

  }
end
include Lim_ops
