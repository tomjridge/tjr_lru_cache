(** Main mt (multithreaded lru) types *)
open Im_intf

module Persist_mode = struct
  (** A calling mode can be "later" (ie, perform immediately in mem
      and return; persist sometime later) or "now", in which case we
      can optionally supply a callback. A call can be blocking or
      non-blocking. For non-blocking, the calling thread is expected to
      launch an async promise that resolves without blocking the
      caller. Thus, we implement only blocking versions of calls. *)
  type persist_mode = Persist_later | Persist_now

  type mode = persist_mode
end
include Persist_mode



(** The LRU operations, expressed using callbacks. This is somehow
   more primitive than the monadic map interface. This interface is
   converted into the typical monadic "syncable map" interface using
   events. *)
module Mt_callback_ops = struct
  (** The interface *provided* by the LRU, with callbacks *)
  type ('k,'v,'t) mt_callback_ops = {
    find: 'k -> ('v option -> (unit,'t)m) -> (unit,'t)m;
    insert: mode -> 'k -> 'v -> (unit -> (unit,'t)m) -> (unit,'t)m;
    delete: mode -> 'k -> (unit -> (unit,'t)m) -> (unit,'t)m;
    sync_key: 'k -> (unit -> (unit,'t)m) -> (unit,'t)m;
  }
end
(* NOTE don't include so we don't clash with other map-like ops *)



module Mt_ops = struct
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
end
(* NOTE don't include *)




module Threading_types = struct
  (* type for the async operation FIXME move to Tjr_monad *)

  (** The async operation completes with unit almost immediately; the
      argument may be a long-running computation; it is scheduled for
      execution.

      Because even creating an 'a m in lwt schedules computation, we
      shield the argument to async to avoid computation.  *)
  type 't async = (unit -> (unit,'t) m) -> (unit,'t) m 

  (** We want to store "blocked threads" in a map, indexed by key; we use
     a polymap; the threads are of type 'v -> ('a,'t) m; the 'a return
     type can just be unit, since a thread can launch some other thread
     to eventually return 'a *)
  type ('v,'t) blocked_thread = 'v option -> (unit,'t)m

  (* NOTE enqueue_to_lower needs to execute immediately; so we keep part
     of the msg queue in the lru state; FIXME probably inefficient *)
  (* FIXME rather than have an explicit list of blocked threads, perhaps
     we can have a "suspend on" operation, and a "wakeup_all" operation,
     like lwt's task and bind *)

  (* open Msg_type *)
end
include Threading_types



module Mt_state_type = struct

(* old !!!
- a queue of messages (most recent first) to the lower level

The lower "disk" layer is defined elsewhere. We just put messages on the list [to_lower], so these messages are the API.

NOTE Access to the lower layer is serialized. Messages are added to
   [to_lower] in order. There is another thread which takes messages
   from [to_lower] (after taking the lru lock) and calls the lower
   layer.

NOTE Access to the [lru_state] is serialized via [with_lru].  
*)


(** The [lru_state] consists of:

- the cache state

- a map from k to a wait list (of threads that are waiting for a find
   operation to complete at the disk layer)

*)
  type ('k,'v,'k_map,'t_map,'t) lru_state = { 
    cache_state: ('k,'v,'k_map) cache_state; 
    blocked_threads: 't_map;
    blocked_threads_ops:('k,('v,'t)blocked_thread list,'t_map)Tjr_map.map_ops
  }
  
  let mt_initial_state ~max_size ~evict_count ~compare_k = 
    let cache_state = Im_cache_state.mk_initial_cache ~max_size ~evict_count ~compare_k in
    let blocked_threads_ops : ('k,('v,'t)blocked_thread list,(_,_,unit)Tjr_map.map)Tjr_map.map_ops = 
      Tjr_map.make_map_ops compare_k in
    {
      cache_state; 
      blocked_threads=blocked_threads_ops.empty;
      blocked_threads_ops;
    }

  let _ :
max_size:int ->
evict_count:int ->
compare_k:('k -> 'k -> int) ->
('k, 'v, ('k, 'v entry, unit) Tjr_map.map,
 ('k, ('v, 't) blocked_thread list, unit) Tjr_map.map, 't)
lru_state
= mt_initial_state

  type ('msg,'k,'v,'k_map,'t_map,'t) with_lru_ops = {
    with_lru: 
      'a. 
        (lru:('k,'v,'k_map,'t_map,'t)lru_state -> 
         set_lru:(('k,'v,'k_map,'t_map,'t)lru_state -> (unit,'t)m)
         -> ('a,'t)m)
      -> ('a,'t)m
  }

end
include Mt_state_type



module Msg_type = struct

  (** The type of messages that we send to the lower level *)
  type ('k,'v,'t) msg = 
      Insert of 'k*'v*(unit -> (unit,'t)m)
    | Delete of 'k*(unit -> (unit,'t)m)
    | Find of 'k * ('v option -> (unit,'t)m)
    | Evictees of ('k * 'v entry) list
end
include Msg_type