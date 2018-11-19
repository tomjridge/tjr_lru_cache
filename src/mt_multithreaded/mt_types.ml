open Tjr_monad.Types
open Im_types

include Mt_callback_ops_type
include Mt_ops_type


(* type for the async operation FIXME move to Tjr_monad *)

(** The async operation completes with unit almost immediately; the
   argument may be a long-running computation; it is scheduled for
   execution.

    Because even creating an 'a m in lwt schedules computation, we
   shield the argument to async to avoid computation.  *)
type 't async = (unit -> (unit,'t) m) -> (unit,'t) m 

(* we want to store "blocked threads" in a map, indexed by key; we use
   a polymap; the threads are of type 'v -> ('a,'t) m; the 'a return
   type can just be unit, since a thread can launch some other thread
   to eventually return 'a *)

type ('v,'t) blocked_thread = 'v option -> (unit,'t)m

(* NOTE enqueue_to_lower needs to execute immediately; so we keep part
   of the msg queue in the lru state; FIXME probably inefficient *)
(* FIXME rather than have an explicit list of blocked threads, perhaps
   we can have a "suspend on" operation, and a "wakeup_all" operation,
   like lwt's task and bind *)

open Msg_type

(** The [lru_state] consists of:

- the cache state

- a map from k to a wait list (of threads that are waiting for a find
   operation to complete at the disk layer)

- a queue of messages (most recent first) to the lower level

The lower "disk" layer is defined elsewhere. We just put messages on the list [to_lower], so these messages are the API.

NOTE Access to the lower layer is serialized. Messages are added to
   [to_lower] in order. There is another thread which takes messages
   from [to_lower] (after taking the lru lock) and calls the lower
   layer.

NOTE Access to the [lru_state] is serialized via [with_lru].  
*)
module Mt_state_type = struct
  type ('k,'v,'t) lru_state = { 
    cache_state: ('k,'v) cache_state; 
    blocked_threads: ('k,('v,'t) blocked_thread list) Tjr_polymap.t;
    to_lower: ('k,'v,'t) msg list; (** NOTE in reverse order  *)
  }

  let mt_initial_state ~max_size ~evict_count ~compare_k = {
    cache_state=Im_cache_state.mk_initial_cache ~max_size ~evict_count ~compare_k; 
    blocked_threads=Tjr_polymap.empty compare_k;
    to_lower=[]
  }

end
include Mt_state_type


type ('msg,'k,'v,'t) with_lru_ops = {
  with_lru: 
    'a. 
      (lru:('k,'v,'t)lru_state -> 
       set_lru:(('k,'v,'t)lru_state -> (unit,'t)m)
       -> ('a,'t)m)
    -> ('a,'t)m
}
