(** A multithreaded / concurrent-safe implementation of the LRU
   interface. This extends the in-memory explicit-state-passing code
   to the monad. *)

(* FIME/TODO

FIXME It may be that we want to prioritize non-evictee updates in the
pcache over evictee writes. But this introduces much complication into
the pcache.

- evictees may arise from the in-mem operation; however, no-one is
  waiting on these; so we just call the pcache to write evictees to
  disk; if we do not have an active pcache thread, this may block the
  pcache; is there a strategy to sync the evictees that doesn't block
  the pcache? could maintain an in-mem evictees map that is gradually
  pushed to disk whilst other requests are served;
    - on the other hand, we expect that the pcache can flush evictees
      to disk in 1 block write


- for in-mem, when we return with cache_state and evictees, we aim to
   unblock the cache state ASAP; but can we do this without ensuring
  that the evictees are on-disk? Yes (the evictees can be written to
  disk in a delayed fashion), but we must be careful that the evictees
  are taken into account when accessing the lower layer. The
  implementation of this is somewhat complicated. So a first version
  should just block until the evictees are known to be on disk.


- FIXME in order to handle evictees quickly, pcache should pre-allocate
   blocks (so we don't need toupdate the free map); perhaps each block
  should then be written with a next pointer if possible (even if not
  full)

*)


open Tjr_monad.Monad
open Lru_in_mem


(* lru state -------------------------------------------------------- *)

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


(** The [lru_state] consists of:

- the cache state

- a map from k to a wait list (of threads that are waiting for a find
   operation to complete at the disk layer)

- a queue of messages (most recent first) to the lower land

The lower "disk" layer is defined in {!module: Pcache_interface}.

NOTE Access to the lower layer is serialized. Messages are added to
   [to_lower] in order. There is another thread which takes messages
   from [to_lower] (after taking the lru lock) and calls the lower
   layer.

NOTE Access to the [lru_state] is serialized via [with_lru].  
*)
type ('msg,'k,'v,'t) lru_state = { 
  cache_state: ('k,'v) cache_state; 
  blocked_threads: ('k,('v,'t) blocked_thread list) Poly_map.t;
  to_lower: 'msg list; (** NOTE in reverse order  *)
}

(** NOTE the following, which essentially adds waiting threads to a
    wait queue, and notifies them when an event occurs, is a
   re-implementation of a concurrency staple (eg Lwt tasks); the
   implementation here is explicit because we may want to verify the
   implementation using the local state [blocked_threads] (rather than
   some generic version provided by the monad *)
let add_callback_on_key ~callback ~k ~lru = 
  match Poly_map.find_opt k lru.blocked_threads with
  | Some xs -> 
    (* just add the callback to the list *)
    let xs = callback::xs in
    let blocked_threads = Poly_map.add k xs lru.blocked_threads in
    { lru with blocked_threads },`Lower_call_in_progress
  | None -> 
    let blocked_threads = Poly_map.add k [callback] lru.blocked_threads in
    (* we want to issue a `Find k call to lower, with a callback that
       unblocks the waiting threads *)
    { lru with blocked_threads },`Need_to_call_lower

(** NOTE this also removes all waiters on key k *)
let process_callbacks_for_key ~monad_ops ~async ~k ~vopt_from_lower ~lru =
  let ( >>= ) = monad_ops.bind in 
  let return = monad_ops.return in
  let blocked_threads = Poly_map.find_opt k lru.blocked_threads in
  assert(blocked_threads <> None);                    
  let blocked_threads = blocked_threads |> Tjr_prelude.dest_Some in
  let rec loop bs = 
    match bs with
    | [] -> return ()
    | b::bs -> async(b vopt_from_lower) >>= fun () -> loop bs
  in
  loop blocked_threads >>= fun () -> 
  let blocked_threads = Poly_map.remove k lru.blocked_threads in
  return {lru with blocked_threads}


(** NOTE [with_lru f] executes whilst locking lru state; it should be
   possible to execute the monadic [f] quickly, without yielding to
   other threads. 

For find: 
- We access the [lru_state] (exlusively/serially).
- We check if the find would succeed without going to disk. If so, we
  are done.
- Otherwise, we check the wait list for the given key. If this is
  non-existent, we create it, execute the lower call to find, and on
  return we alter the then-current cache state (updates are
  serialized), and notify all pending threads on the wait list, and
  delete the wait list.


For [Persist_later] insert and delete, we access the [lru_state],
modify the cache state. If we have evictees, we need to flush these to
lower. The problem is that we may not want to hold the [lru_state]
during this eviction. At the moment, we hold the lock.

For [Persist_now] insert and delete, we do as above. If we have
evictees, we can add the op to the evictees list (so, the size of the
evictees should be 1 less than the amount that can safely fit in one
block FIXME) and then call the lower layer.

For sync_key (and sync_keys) we simply take the entry in the cache (if
any) and mark it clean and flush to lower.

*)
let make_lru_ops ~monad_ops ~with_lru ~async =
  let ( >>= ) = monad_ops.bind in 
  let return = monad_ops.return in
  let process_callbacks_for_key = process_callbacks_for_key ~monad_ops ~async in
  let in_mem_ops = make_lru_in_mem() in
  let find k (callback:'v option -> (unit,'t) m) = 
    with_lru (fun ~lru ~set_lru -> 
        (* check if k is already in the cache *)
        in_mem_ops.find k lru.cache_state |> function
        | `In_cache e -> 
          let vopt = 
            match e.entry_type with
            | Insert i -> (Some i.value)
            | Delete _ -> None
            | Lower vopt -> vopt
          in
          callback vopt
        | `Not_in_cache kk -> (
            (* now we have to call to the lower level; first we check
               that a call isn't already in process... *)
            add_callback_on_key ~k ~callback ~lru |> fun (lru,lower_call_status) -> 
            match lower_call_status with
            | `Lower_call_in_progress -> set_lru lru  (* and return *)
            | `Need_to_call_lower -> 
              (* we want to issue a `Find k call to lower, with a
                 callback that unblocks the waiting threads *)
              let callback = fun vopt_from_lower ->                   
                with_lru (fun ~lru ~set_lru -> 
                    (* first wake up sleeping threads *)
                    process_callbacks_for_key ~k ~vopt_from_lower ~lru >>= fun lru -> 
                    kk ~vopt_from_lower ~cache_state:lru.cache_state 
                    |> fun (vopt,`Evictees es, `Cache_state cache_state) -> 
                    (* have to handle evictees by flushing to lower *)
                    match es with 
                    | None -> set_lru {lru with cache_state}
                    | Some es -> 
                      set_lru {lru with cache_state; 
                                        to_lower=(`Evictees es)::lru.to_lower })
              in
              set_lru { lru with to_lower=`Find(k,callback)::lru.to_lower }))
  in

  ()


