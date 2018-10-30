(** LRU with disk.

This extends the in-memory code to the monad.

For find: We use with_cache_state to execute the first step of
   find. We check if the find would succeed without going to disk. If
   so, we are done. Otherwise, we execute the lower call to find, and
   on return we alter the then-current cache_state (updates are
   serialized).

We have the problem that we may have a stale value for the result of
   the lower find. So we need to tag results returned by lower_find
   with a "time of read", so we don't replace a newer value with an
   older value. FIXME this requires modifying the in-mem code again.

In order to serialize the updates to the cache state, we add a
   "with_cache_state" call that blocks whilst updates are made to the
   cache state.

*)

(** TODO:

- add "current lower time" tag to lower_find in in-mem code

- update in-mem find to return a continuation that again takes the
   cache state

- update the find code to take account of the "current lower time"
   that is returned by lower_find

- implement sync_key and sync_all_keys for in-mem interface; these
   need to update the cache state as well of course (to mark all
   entries clean)

- evictees may arise from the in-mem operation; however, no-one is
   waiting on these; so we just call the pcache to write evictees to
   disk

- for LRU persist-now mode: we use "handle_ops_in_pcache(ops)" from
   the LRU; this is implemented by placing the ops on the pcache
   queue, together with a callback; the pcache thread activates the
   callback when the ops have been persisted. Why not just lock the
   pcache and execute the write? It may be that we want to prioritize
   non-evictee updates in the pcache

- for in-mem, when we return with cache_state and evictees, we aim to
   unblock the cache state ASAP; but can we do this without ensuring
   that the evictees are on-disk? Yes (the evictees can be written to
   disk in a delayed fashion), but we must be careful that the
   evictees are taken into account when accessing the lower layer. The
   implementation of this is somewhat complicated. So a first version
   should just block until the evictees are known to be on disk.

*)



(* pcache interface ------------------------------------------------- *)

open Tjr_monad.Monad
open In_mem_cache

type ('k,'v,'t) pcache_ops = {
  find: 'k -> ('v option,'t) m;
  
  insert_now: 'k -> 'v -> (unit,'t) m;  (* use sync_key implementation *)
  delete_now: 'k -> (unit,'t) m;  (* ditto *)

  sync_key: 'k -> 'v entry_type -> (unit,'t) m;
  sync_keys: ('k * 'v entry) list -> (unit,'t) m;  (* can use flush_evictees implementation *)

  flush_evictees: ('k * 'v entry) list -> (unit, 't) m;  
  (** blocks; make sure these operations are serialized to pcache *)
}

let in_mem_cache_ops = In_mem_cache.make_cached_map()

let _ = in_mem_cache_ops
