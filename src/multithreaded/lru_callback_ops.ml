open Tjr_monad.Types
open Persist_mode

(* FIXME do we want eg find to take the callback to 'v option m? or add functionality to fulfil some promise, of type: 'a -> ('a,'t) m -> unit; or rather 'a -> ('a,'t) u -> (unit,'t) m; or can we just call async in the callback? *)

(** The interface *provided* by the LRU, with callbacks *)
type ('k,'v,'t) lru_callback_ops = {
  find: 'k -> ('v option -> (unit,'t)m) -> (unit,'t)m;
  insert: mode -> 'k -> 'v -> (unit -> (unit,'t)m) -> (unit,'t)m;
  delete: mode -> 'k -> (unit -> (unit,'t)m) -> (unit,'t)m;
  sync_key: 'k -> (unit -> (unit,'t)m) -> (unit,'t)m;
}
