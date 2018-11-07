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
open Persist_mode
open Lru_in_mem

(* type for the async operation FIXME move to Tjr_monad *)

(** The async operation completes with unit almost immediately; the
   argument may be a long-running computation; it is scheduled for
   execution.

    Because even creating an 'a m in lwt schedules computation, we
   shield the argument to async to avoid computation.  *)
type 't async = (unit -> (unit,'t) m) -> (unit,'t) m 


(* lru_ops type ----------------------------------------------------- *)


module Lru_callback_ops = struct
(* FIXME do we want eg find to take the callback to 'v option m? or add functionality to fulfil some promise, of type: 'a -> ('a,'t) m -> unit; or rather 'a -> ('a,'t) u -> (unit,'t) m; or can we just call async in the callback? *)
type ('k,'v,'t) lru_callback_ops = {
  find: 'k -> ('v option -> (unit,'t)m) -> (unit,'t)m;
  insert: mode -> 'k -> 'v -> (unit -> (unit,'t)m) -> (unit,'t)m;
  delete: mode -> 'k -> (unit -> (unit,'t)m) -> (unit,'t)m;
  sync_key: 'k -> (unit -> (unit,'t)m) -> (unit,'t)m;
}
end
include Lru_callback_ops  


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

- a queue of messages (most recent first) to the lower level

The lower "disk" layer is defined elsewhere. We just put messages on the list [to_lower], so these messages are the API.

NOTE Access to the lower layer is serialized. Messages are added to
   [to_lower] in order. There is another thread which takes messages
   from [to_lower] (after taking the lru lock) and calls the lower
   layer.

NOTE Access to the [lru_state] is serialized via [with_lru].  
*)
module Lru_state = struct
type ('msg,'k,'v,'t) lru_state = { 
  cache_state: ('k,'v) cache_state; 
  blocked_threads: ('k,('v,'t) blocked_thread list) Tjr_polymap.t;
  to_lower: 'msg list; (** NOTE in reverse order  *)
}
end
include Lru_state


(** NOTE the following, which essentially adds waiting threads to a
    wait queue, and notifies them when an event occurs, is a
   re-implementation of a concurrency staple (eg Lwt tasks); the
   implementation here is explicit because we may want to verify the
   implementation using the local state [blocked_threads] (rather than
   some generic version provided by the monad) *)
let add_callback_on_key ~callback ~k ~lru = 
  match Tjr_polymap.find_opt k lru.blocked_threads with
  | Some xs -> 
    (* just add the callback to the list *)
    let xs = callback::xs in
    let blocked_threads = Tjr_polymap.add k xs lru.blocked_threads in
    { lru with blocked_threads },`Lower_call_in_progress
  | None -> 
    let blocked_threads = Tjr_polymap.add k [callback] lru.blocked_threads in
    (* we want to issue a `Find k call to lower, with a callback that
       unblocks the waiting threads *)
    { lru with blocked_threads },`Need_to_call_lower

(** NOTE this also removes all waiters on key k *)
let process_callbacks_for_key ~monad_ops ~(async:'t async) ~k ~vopt_from_lower ~lru =
  let ( >>= ) = monad_ops.bind in 
  let return = monad_ops.return in
  let blocked_threads = Tjr_polymap.find_opt k lru.blocked_threads in
  assert(blocked_threads <> None);                    
  let blocked_threads = blocked_threads |> Tjr_prelude.dest_Some in
  let rec loop bs = 
    match bs with
    | [] -> return ()
    | b::bs -> async(fun () -> b vopt_from_lower) >>= fun () -> loop bs
  in
  loop blocked_threads >>= fun () -> 
  let blocked_threads = Tjr_polymap.remove k lru.blocked_threads in
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

type ('msg,'k,'v,'t) with_lru_ops = {
  with_lru: 
    'a. 
      (lru:('msg,'k,'v,'t)lru_state -> 
       set_lru:(('msg,'k,'v,'t)lru_state -> (unit,'t)m)
       -> ('a,'t)m)
    -> ('a,'t)m
}

let make_lru_ops' ~monad_ops ~with_lru_ops ~(async:'t async) =
  let ( >>= ) = monad_ops.bind in 
  let return = monad_ops.return in
  let with_lru = with_lru_ops.with_lru in
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
          async (fun () -> callback vopt)
        (* FIXME note the callback should not do computation on
           receiving an argument... it must wait for the world state
           *)
        | `Not_in_cache kk -> (
            (* now we have to call to the lower level; first we check
               that a call isn't already in process... *)
            add_callback_on_key ~k ~callback ~lru |> fun (lru,lower_call_status) -> 
            match lower_call_status with
            | `Lower_call_in_progress -> set_lru lru  (* and return *)
            | `Need_to_call_lower -> 
              (* we want to issue a `Find k call to lower, with a
                 callback that unblocks the waiting threads *)
              let callback vopt_from_lower : (unit,'t)m =                    
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

  let insert mode k v (callback: unit -> (unit,'t)m) =
    with_lru (fun ~lru ~set_lru -> 
        in_mem_ops.insert k v lru.cache_state 
        |> function (`Evictees es, `Cache_state cache_state) ->
          (* update lru with evictees *)
          let to_lower = 
            match es with 
            | None -> lru.to_lower
            | Some es -> (`Evictees es)::lru.to_lower
          in
          (* handle mode=persist_now, cache_state *)
          let cache_state,to_lower = 
            match mode with
            | Persist_now -> (
                in_mem_ops.sync_key k cache_state |> function
                | `Not_present -> failwith "impossible"
                | `Present (e,c) -> (
                    assert(entry_type_is_dirty e.entry_type);
                    (c,(`Insert(k,v,callback)::to_lower)))
                (* FIXME order or evictees vs insert? *))
            | Persist_later -> (cache_state,to_lower)
          in
          set_lru {lru with cache_state; to_lower}) >>= fun () ->
    (if mode=Persist_later then async(callback) else return ())
  in
  
  let delete mode k callback = 
    with_lru (fun ~lru ~set_lru -> 
        in_mem_ops.delete k lru.cache_state 
        |> function (`Evictees es, `Cache_state cache_state) ->
          (* update lru with evictees *)
          let to_lower = 
            match es with 
            | None -> lru.to_lower
            | Some es -> (`Evictees es)::lru.to_lower
          in
          (* handle mode=persist_now, cache_state *)
          let cache_state,to_lower = 
            match mode with
            | Persist_now -> (
                in_mem_ops.sync_key k cache_state |> function
                | `Not_present -> failwith "impossible"
                | `Present (e,c) -> (
                    assert(entry_type_is_dirty e.entry_type);
                    (c,(`Delete(k,callback)::to_lower)))
                (* FIXME order or evictees vs insert? *))
            | Persist_later -> (cache_state,to_lower)
          in
          set_lru {lru with cache_state; to_lower}) >>= fun () ->
    (if mode=Persist_later then async(callback) else return ())
  in

  let sync_key k callback = 
    with_lru (fun ~lru ~set_lru -> 
        in_mem_ops.sync_key k lru.cache_state |> function
          | `Not_present -> async (callback)
          | `Present (e,cache_state) ->             
            match entry_type_is_dirty e.entry_type with
            | false -> 
              (* just return *)
              async (callback)
            | true -> 
              let to_lower =
                match e.entry_type with
                | Insert{value;dirty} ->
                  (`Insert(k,value,callback)::lru.to_lower)
                | Delete{dirty} -> 
                  (`Delete(k,callback)::lru.to_lower)
                | _ -> lru.to_lower
              in
              set_lru {lru with cache_state; to_lower}

      )
  in
  fun kk -> kk ~find ~insert ~delete ~sync_key
  


(* export ----------------------------------------------------------- *)

(* open Lru_interface *)

let make_lru_callback_ops ~monad_ops ~with_lru_ops ~async =
  make_lru_ops' ~monad_ops ~with_lru_ops ~async @@ fun ~find ~insert ~delete ~sync_key -> 
  let open Lru_callback_ops in
  {find;insert;delete;sync_key}



(* implement non-callback interface --------------------------------- *)

(** We assume that there is a way to "fulfill" an 'a m with an 'a  *)
type 't event_ops = {
  create: 'a. unit -> ('a,'t)m;
  signal: 'a. ('a,'t) m -> 'a -> (unit,'t)m;
}

module Lru_ops = struct

(** The interface provided by the LRU; provides blocking/non-blocking
   operations, and persist now/persist later flags. 

These are the operations supported by the LRU.

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

end

let make_lru_ops ~monad_ops ~event_ops ~callback_ops =
  let ( >>= ) = monad_ops.bind in 
  let return = monad_ops.return in
  let {find;insert;delete;sync_key} = callback_ops in
  let find k = 
    let e = event_ops.create() in
    (find k (fun v -> event_ops.signal e v)) >>= fun () -> e
  in
  let insert mode k v =
    let e = event_ops.create() in
    (insert mode k v (fun () -> event_ops.signal e ())) >>= fun () -> e
  in
  let delete mode k = 
    let e = event_ops.create() in
    (delete mode k (fun () -> event_ops.signal e ())) >>= fun () -> e
  in
  let sync_key k =
    let e = event_ops.create() in
    (sync_key k (fun () -> event_ops.signal e ())) >>= fun () -> e
  in
  let sync_all_keys () =
    failwith "FIXME TODO not implemented yet"
  in
  let open Lru_ops in
  {find;insert;delete;sync_key;sync_all_keys}
  
