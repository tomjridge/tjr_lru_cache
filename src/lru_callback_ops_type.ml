(** The LRU operations, expressed using callbacks. This is somehow
   more primitive than the monadic map interface. This interface is
   converted into the typical monadic "syncable map" interface using
   events. *)

open Tjr_monad.Types
open Persist_mode


(** The interface *provided* by the LRU, with callbacks *)
type ('k,'v,'t) lru_callback_ops = {
  find: 'k -> ('v option -> (unit,'t)m) -> (unit,'t)m;
  insert: mode -> 'k -> 'v -> (unit -> (unit,'t)m) -> (unit,'t)m;
  delete: mode -> 'k -> (unit -> (unit,'t)m) -> (unit,'t)m;
  sync_key: 'k -> (unit -> (unit,'t)m) -> (unit,'t)m;
}
