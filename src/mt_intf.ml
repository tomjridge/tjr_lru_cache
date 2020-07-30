(** Main mt (multithreaded lru) types; don't open outside this lib *)


(** NOTE the main interfaces are in module {!Mt_callback_ops} and {!Mt_ops}. *)

open Im_intf

(** A calling mode can be "later" (ie, perform immediately in mem
    and return; persist sometime later) or "now", in which case we
    can optionally supply a callback. A call can be blocking or
    non-blocking. For non-blocking, the calling thread is expected to
    launch an async promise that resolves without blocking the
    caller. Thus, we implement only blocking versions of calls. *)
type persist_mode = Persist_later | Persist_now


(** The LRU operations, expressed using callbacks. This is somehow
   more primitive than the monadic map interface. This interface is
   converted into the typical monadic "syncable map" interface using
   events. *)
module Mt_callback_ops = struct
  (** The interface *provided* by the LRU, with callbacks *)
  type ('k,'v,'t) mt_callback_ops = {
    find     : 'k -> ('v option -> (unit,'t)m) -> (unit,'t)m;
    insert   : persist_mode -> 'k -> 'v -> (unit -> (unit,'t)m) -> (unit,'t)m;
    delete   : persist_mode -> 'k -> (unit -> (unit,'t)m) -> (unit,'t)m;
    sync_key : 'k -> (unit -> (unit,'t)m) -> (unit,'t)m;
  }
end
(* NOTE don't include so we don't clash with other map-like ops *)

type ('k,'v,'t) mt_callback_ops = ('k,'v,'t) Mt_callback_ops.mt_callback_ops


(** The main LRU operations, with a non-callback interface. *)
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
  (* $(PIPE2SH("""sed -n '/type[ ].*mt_ops = /,/}/p' >GEN.mt_ops.ml_""")) *)
  type ('k,'v,'t) mt_ops = {
    mt_find          : 'k -> ('v option,'t) m; 
    mt_insert        : persist_mode -> 'k -> 'v -> (unit,'t) m;
    mt_delete        : persist_mode -> 'k -> (unit,'t) m;
    mt_sync_key      : 'k -> (unit,'t) m;
    mt_sync_all_keys : unit -> (unit,'t) m;
  }
end
include Mt_ops


(* type for the async operation FIXME move to Tjr_monad *)

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

module Mt_state = struct

(** The state consists of:

- the cache state

- a map from k to a wait list (of threads that are waiting for a find
   operation to complete at the disk layer)

NOTE t_map is "thread map"

*)
  type ('k,'v,'lru,'t_map,'t) mt_state = { 
    lim_state           : 'lru lru_state_im; 
    blocked_threads     : 't_map;
    blocked_threads_ops : ('k,('v,'t)blocked_thread list,'t_map)Tjr_map.map_ops
  }
  
  let mt_initial_state ~(initial_lim_state:'lru lru_state_im) ~k_cmp = 
    let lim_state = initial_lim_state in
    let blocked_threads_ops : ('k,('v,'t)blocked_thread list,(_,_,unit)Tjr_map.map)Tjr_map.map_ops = 
      Tjr_map.unsafe_make_map_ops k_cmp in
    {
      lim_state; 
      blocked_threads=blocked_threads_ops.empty;
      blocked_threads_ops;
    }

end
include Mt_state


module Lru_msg = struct
  (** The type of messages that we send to the lower level. NOTE that
     the callbacks are needed to implement the "persist now" mode,
     i.e., to inform the caller when the operation has committed to
     disk. *)
  (* $(PIPE2SH("""sed -n '/type[ ].*lru_msg = /,/list/p' >GEN.lru_msg.ml_""")) *)
  type ('k,'v,'t) lru_msg = 
    | Insert of 'k*'v*(unit -> (unit,'t)m)
    | Delete of 'k*(unit -> (unit,'t)m)
    | Find of 'k * ('v option -> (unit,'t)m)
    | Evictees of ('k * 'v entry) list
end
include Lru_msg


module Lru_factory = struct
  
  (** This is the eventual interface provided by [Make]. NOTE this
     type is unreadable in odoc *)
  (* $(PIPE2SH("""sed -n '/type[ ].*lru_factory = /,/^ *>/p' >GEN.lru_factory.ml_""")) *)
  type ('k,'v,'lru,'t) lru_factory = <
    empty :       
      max_size    : int -> 
      evict_count : int -> 
      'lru;

    make_ops : 
      with_state : ('lru,'t) with_state ->
      to_lower   : (('k,'v,'t) lru_msg -> (unit,'t)m) -> 
      ('k,'v,'t)mt_ops;

  >
end
include Lru_factory
