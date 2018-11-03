(** The interface to the pcache (uncached KV store), on which we build
   the LRU. *)

open Tjr_monad.Monad

type ('k,'v) op = Insert of 'k * 'v | Delete of 'k
  
(** NOTE the following are all blocking *)
type ('k,'v,'t) uncached_ops = {
  find: 'k -> ('v option, 't) m;
  insert: 'k -> 'v -> (unit,'t) m;
  delete: 'k -> (unit,'t) m;
  handle_batched_ops: ('k,'v) op list -> (unit,'t) m;
}
