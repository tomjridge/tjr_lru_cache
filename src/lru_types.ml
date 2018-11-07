(** The interface provided by the LRU; provides blocking/non-blocking
   operations, and persist now/persist later flags. *)

(* OLD These are the operations supported by the LRU.

NOTE this interface doesn't allow "transaction" operations (multiple
   ops, which commit atomically). This is sufficient for ImpFS - the
   kv store is pointwise syncable not transactional. However, since
   the lower level does support transactional operations, it seems
   strange to limit the functionality here.

NOTE all calls are blocking; for non-blocking calls, launch an async
   light-weight thread. *)
(*
type ('k,'v,'t) lru_ops = {
  find: 'k -> ('v option,'t) m; 
  insert: mode -> 'k -> 'v -> (unit,'t) m;
  delete: mode -> 'k -> (unit,'t) m;
  sync_key: 'k -> (unit,'t) m;
  sync_all_keys: unit -> (unit,'t) m;
}
*)



(* in-mem types ----------------------------------------------------- *)

include Persist_mode
include Lru_in_mem.Entry_type
include Lru_in_mem.Cache_state
include Lru_in_mem.Lru_in_mem_ops
include Lru_multithreaded.Lru_state
include Lru_multithreaded.Lru_ops
