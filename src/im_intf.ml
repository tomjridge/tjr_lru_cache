(** Main lru-in-mem interfaces; don't open (clashing fields) *)


(** {2 Cache entries} *)

module Lru_entry = struct

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
  (* $(PIPE2SH("""sed -n '/type[ ].*entry = /,/option/p' >GEN.entry.ml_""")) *)
  type 'v lru_entry = 
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

  let dirty_entry_to_kvop k e = 
    match e with
    | Insert{value;_} -> Kvop.(Insert(k,value))
    | Delete _ -> Kvop.(Delete k)
    | Lower _ -> failwith "impossible"

end
include Lru_entry


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

  (** A record to package up the functions from find. *)


  (* \$(PIPE2SH("""sed -n '/type[ ].*evictees_x_state = /,/}/p' >GEN.evictees_x_state.ml_""")) *)
  type ('k,'v,'lru) evictees_x_state = {
    evictees     : ('k,'v)kvop list; (* NOTE may be empty *)
    lru_state_im : 'lru lru_state_im
  }

  type ('k,'v,'lru) apply_update_from_lower = 
    vopt_from_lower : 'v option ->
    lru_state_im    : 'lru lru_state_im ->
    'v option * ('k,'v,'lru) evictees_x_state
    
  
  type ('k,'v,'lru) find_result = 
    | In_cache of 'v lru_entry 
    | Not_in_cache 
 (* of ('k,'v,'lru) apply_update_from_lower *)
    (** Not_in_cache: we have to call out to disk; what we get back is
       a 'v option, and we have to then update the lru state (to, in
       turn, get a possibly different 'v option and the
       evictees_x_state; actually, since we are the only thread
       operating on 'k (other threads block), the 'v option returned
       is that received from lower) *)

  (** This type is what is returned by the [make_lru_in_mem] function.

      NOTE that [sync_key] performs only the in-mem updates (ie
     clearing the dirty flag). If you want to flush to disk, you have
     to do something else (see {!Lru_multithreaded}).

      NOTE find returns a "find_result", which consists of a
     callback function which tries to retrieve from below, and then on
     return updates the then-current lim_state (which may already have
     the entry...); this is implemented in the multithreaded code

      NOTE the interface tries to favour pure state passing; find is
     the only routine which may need to call to disk *)
  (* \$(PIPE2SH("""sed -n '/type[ ].*lru_ops_im = /,/}/p' >GEN.lru_ops_im.ml_""")) *)
  type ('k,'v,'lru) lru_ops_im = {
    find: 'k -> 'lru lru_state_im -> [`In of 'v option | `Not_in ]; 
    (* In None means there is a Delete in the map for key k; Not_in
       means we have to call lower layers *)

    insert: 'k -> 'v -> 'lru lru_state_im ->
      ('k,'v,'lru) evictees_x_state;

    (* k is not in the cache, and there are waiting threads *)
    update_from_lower: 'k -> 'v option -> 'lru lru_state_im ->
      ('k,'v,'lru) evictees_x_state;

    delete: 'k -> 'lru lru_state_im -> 
      ('k,'v,'lru) evictees_x_state;

    sync_key: 'k -> 'lru lru_state_im -> ( ('k,'v)kvop * 'lru lru_state_im ) option;
    (** If the key is present and the entry is dirty, we get a kvop
       and a state with that entry cleaned; otherwise we get
       None. NOTE in the multithreaded code, this must also call out
       to the lower layer. The state is changed if the entry is
       dirty. *)
      
    sync_all_keys: 'lru lru_state_im -> ('k,'v,'lru) evictees_x_state;  
    (** NOTE in multithreaded code, this must call lower before returning *)
  }

end
include Lru_ops_im

(* \$(PIPE2SH("""sed -n '/type[ ].*lru_params_im = /,/>/p' >GEN.lru_params_im.ml_""")) *)
type lru_params_im = <
  max_size:int;
  evict_count:int
>

(* \$(PIPE2SH("""sed -n '/type[ ].*lru_factory_im = /,/>/p' >GEN.lru_factory_im.ml_""")) *)
type ('k,'v,'lru) lru_factory_im = <
  empty : lru_params_im -> 'lru lru_state_im;
  lru_ops_im : ('k,'v,'lru) lru_ops_im
>
