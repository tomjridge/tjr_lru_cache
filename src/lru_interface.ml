(** The interface provided by the LRU; provides blocking/non-blocking
   operations, and persist now/persist later flags. *)

open Tjr_monad.Monad

(** A calling mode can be "later" (ie, perform immediately in mem
    and return; persist sometime later) or "now", in which case we
    can optionally supply a callback. A call can be blocking or
    non-blocking. For non-blocking, the calling thread is expected to
    launch an async promise that resolves without blocking the
    caller. Thus, we implement only blocking versions of calls. *)
type persist_mode = Persist_later | Persist_now

type mode = persist_mode

(** These are the operations supported by the LRU.

NOTE this interface doesn't allow "transaction" operations (multiple
   ops, which commit atomically). This is sufficient for ImpFS - the
   kv store is pointwise syncable not transactional. However, since
   the lower level does support transactional operations, it seems
   strange to limit the functionality here.

NOTE all calls are blocking; for non-blocking calls, launch an async
   light-weight thread. *)
type ('k,'v,'t) lru_ops = {
  find: 'k -> ('v option,'t) m; 
  insert: mode -> 'k -> 'v -> (unit,'t) m;
  delete: mode -> 'k -> (unit,'t) m;
  sync_key: 'k -> (unit,'t) m;
  sync_all_keys: unit -> (unit,'t) m;
}


