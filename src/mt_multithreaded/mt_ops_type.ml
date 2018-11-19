open Tjr_monad.Types
open Persist_mode

(** The interface provided by the multithreaded LRU; provides
   blocking/non-blocking operations, and persist now/persist later
   flags.

These are the operations supported by the LRU.

NOTE this interface doesn't allow "transaction" operations (multiple
   ops, which commit atomically). This is sufficient for ImpFS - the
   kv store is pointwise syncable not transactional. However, since
   the lower level does support transactional operations, it seems
   strange to limit the functionality here.

NOTE all calls are blocking; for non-blocking calls, launch an async
   light-weight thread. *)

type ('k,'v,'t) mt_ops = {
  find: 'k -> ('v option,'t) m; 
  insert: mode -> 'k -> 'v -> (unit,'t) m;
  delete: mode -> 'k -> (unit,'t) m;
  sync_key: 'k -> (unit,'t) m;
  sync_all_keys: unit -> (unit,'t) m;
}

