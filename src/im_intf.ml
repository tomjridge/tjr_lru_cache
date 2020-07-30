(** Main lru-in-mem interfaces; don't open (clashing fields) *)


(** {2 Cache entries} *)

module Entry = struct

  (** Cache map entries; values in the map are tagged with a
      last-accessed time (via underlying Lru impl) and a dirty flag

      Entries in the cache for key k:

      - (Insert v (dirty=true/false)), {i this occurs on insert}
      - (Delete   (dirty=true/false))
      - (Lower vopt), {i this occurs when we check the lower layer for a
        non-existing entry in cache; if we find a value, we insert Lower
        (Some v), else Lower None; in either case, there is no need to do
        anything further (ie the entry is not dirty) }
      - (No entry), {i for a key that hasn't been seen before}

  *)
  (* \$(PIPE2SH("""sed -n '/type[ ].*entry = /,/option/p' >GEN.entry.ml_""")) *)
  type 'v entry = 
    | Insert of { value: 'v; dirty:bool }
    | Delete of { dirty:bool }
    | Lower of 'v option


  (** Auxiliary functions for entries *)

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
include Entry


(** {2 Lru cache state (in memory)} *)

(** The LRU in-memory cache state consists of:

- [max_size]: the max number of entries in the cache

- [evict_count]: number of entries to evict when cache full; for
   [tjr_kv] performance is best when the [evict_count] is such that
   the evictees fit nicely in a block

- [lru_state]: the lru state from the underlying lru impl

*)
module Lru_state_im = struct
  (* \$(PIPE2SH("""sed -n '/type[ ].*lru_state_im = /,/}/p' >GEN.lru_state_im.ml_""")) *)
  type 'lru lru_state_im = {  
    max_size    : int;
    evict_count : int; (* number to evict when cache full *)
    lru_state   : 'lru;
    compare_lru : 'lru -> 'lru -> int
  }
end
include Lru_state_im


(** {2 Lru ops (in memory)} *)
module Lru_ops_im = struct

  (** A record to package up the functions from {!Lru_in_mem}. *)

  type ('a,'b) maybe_in_cache = In_cache of 'a | Not_in_cache of 'b

  (* \$(PIPE2SH("""sed -n '/type[ ].*evictees_x_state = /,/}/p' >GEN.evictees_x_state.ml_""")) *)
  type ('k,'v,'lru) evictees_x_state = {
    evictees     : ('k * 'v entry) list option;
    lru_state_im : 'lru lru_state_im
  }

  (** This type is what is returned by the [make_lru_in_mem] function.

      NOTE that [sync_key] performs only the in-mem updates (ie
     clearing the dirty flag). If you want to flush to disk, you have
     to do something else (see {!Lru_multithreaded}).

      NOTE find returns a "maybe_in_cache", which consists of a
     callback function which tries to retrieve from below, and then on
     return updates the then-current lim_state (which may already have
     the entry...)

      NOTE the interface tries to favour pure state passing; find is
     the only routine which may need to call to disk *)
  (* \$(PIPE2SH("""sed -n '/type[ ].*lru_ops_im = /,/}/p' >GEN.lru_ops_im.ml_""")) *)
  type ('k,'v,'lru) lru_ops_im = {
    find: 'k -> 'lru lru_state_im -> 
      ('v entry, 
       vopt_from_lower:'v option ->
       lru_state_im:'lru lru_state_im ->
       'v option * 
       ('k,'v,'lru) evictees_x_state ) maybe_in_cache;

    insert: 'k -> 'v -> 'lru lru_state_im ->
      ('k,'v,'lru) evictees_x_state;

    delete: 'k -> 'lru lru_state_im ->
      ('k,'v,'lru) evictees_x_state;

    sync_key: 'k -> 'lru lru_state_im -> 
      ('v entry * 'lru lru_state_im, unit) maybe_in_cache
  }

end
include Lru_ops_im

(*
module Lru_fc = struct
  type ('k,'v,'lru) lru_fc = (module Lru.F.S with type k='k and type v='v entry and type t='lru)
end
include Lru_fc
*)
