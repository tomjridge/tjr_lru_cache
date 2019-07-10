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

open Im_intf
open Lru_in_mem
open Mt_intf


(* profiling -------------------------------------------------------- *)

let profiler = ref Tjr_profile.dummy_profiler
               |> Global.register ~name:"Multithreaded_lru profiler"


(* let _ = Printf.printf "Warning, profiling enabled. Your code may run slow. At: \n%s\n%!" __LOC__ *)
(* let _ = assert(Printf.printf "Assertions enabled. Good!\n%!"; true) *)


module Internal2 = struct
  (* lru state -------------------------------------------------------- *)


  (** NOTE the following, which essentially adds waiting threads to a
      wait queue, and notifies them when an event occurs, is a
      re-implementation of a concurrency staple (eg Lwt tasks); the
      implementation here is explicit because we may want to verify the
      implementation using the local state [blocked_threads] (rather than
      some generic version provided by the monad) *)
  let add_callback_on_key ~callback ~k ~lru = 
    let m = lru.blocked_threads_ops in
    match m.find_opt k lru.blocked_threads with
    | Some xs -> 
      (* just add the callback to the list *)
      let xs = callback::xs in
      let blocked_threads = m.add k xs lru.blocked_threads in
      { lru with blocked_threads },`Lower_call_in_progress
    | None -> 
      let blocked_threads = m.add k [callback] lru.blocked_threads in
      (* we want to issue a `Find k call to lower, with a callback that
         unblocks the waiting threads *)
      { lru with blocked_threads },`Need_to_call_lower

  (** NOTE this also removes all waiters on key k *)
  let run_callbacks_for_key ~monad_ops ~(async:'t async) ~k ~vopt_from_lower ~lru =
    let ( >>= ) = monad_ops.bind in 
    let return = monad_ops.return in
    let m = lru.blocked_threads_ops in
    let blocked_threads = m.find_opt k lru.blocked_threads in
    assert(blocked_threads <> None);                    
    let blocked_threads = blocked_threads |> dest_Some in
    let rec loop bs = 
      match bs with
      | [] -> return ()
      | b::bs -> async(fun () -> b vopt_from_lower) >>= fun () -> loop bs
    in
    loop blocked_threads >>= fun () -> 
    let blocked_threads = m.remove k lru.blocked_threads in
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

  let make_lru_ops' ~monad_ops ~(async:'t async) ~with_lru_ops ~to_lower  =
    let mark = !profiler.mark in
    let ( >>= ) = monad_ops.bind in 
    let return = monad_ops.return in
    let maybe_to_lower = function
      | None -> return ()
      | Some msg -> to_lower msg
    in
    let evictees_to_msg_option = function
      | None -> None
      | Some [] -> failwith __LOC__  (* assume evictees are never [] *)
      | Some es -> Some (Evictees es)
    in
    let maybe_evictees_to_lower es = maybe_to_lower (evictees_to_msg_option es) in
    let with_lru = with_lru_ops.with_lru in
    let run_callbacks_for_key = run_callbacks_for_key ~monad_ops ~async in
    let in_mem_ops = make_lru_in_mem() in
    let find k (callback:'v option -> (unit,'t) m) = 
      with_lru (fun ~lru ~set_lru -> 
          (* check if k is already in the cache *)
          in_mem_ops.find k lru.cache_state |> function
          | `In_cache e -> (
              let vopt = 
                match e.entry_type with
                | Insert i -> (Some i.value)
                | Delete _ -> None
                | Lower vopt -> vopt
              in
              async (fun () -> callback vopt))
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
                      run_callbacks_for_key ~k ~vopt_from_lower ~lru >>= fun lru -> 
                      kk ~vopt_from_lower ~cache_state:lru.cache_state 
                      |> fun (vopt,`Evictees es, `Cache_state cache_state) -> 
                      (* have to handle evictees by flushing to lower *)
                      maybe_evictees_to_lower es >>= fun () ->
                      set_lru {lru with cache_state })
                in
                to_lower (Find(k,callback))))
    in

    (* let mark = get_mark ~name:"mt_insert"  in  *)

    let insert mode k v (callback: unit -> (unit,'t)m) =
      with_lru (fun ~lru ~set_lru -> 
          assert(mark "ab";true);
          in_mem_ops.insert k v lru.cache_state 
          |> function (`Evictees es, `Cache_state cache_state) ->
            assert(mark "bc";true);        
            (match es with
             | None -> return ()
             | Some es -> to_lower (Evictees es)) >>= fun () ->
            (* handle mode=persist_now, cache_state *)
            assert(mark "cd";true);        
            let cache_state,to_lower = 
              match mode with
              | Persist_now -> (
                  in_mem_ops.sync_key k cache_state |> function
                  | `Not_present -> failwith "impossible"
                  | `Present (e,c) -> (
                      assert(Entry.entry_type_is_dirty e.entry_type);
                      (c,Some(Insert(k,v,callback)))))
              (* FIXME order or evictees vs insert? *)
              | Persist_later -> (cache_state,None)
            in
            maybe_to_lower to_lower >>= fun () ->
            assert(mark "de";true);        
            set_lru {lru with cache_state}) >>= fun () ->
      assert(mark "ef";true);        
      (if mode=Persist_later then async(callback) else return ())
    in

    let delete mode k callback = 
      with_lru (fun ~lru ~set_lru -> 
          in_mem_ops.delete k lru.cache_state 
          |> function (`Evictees es, `Cache_state cache_state) ->
            (* update lru with evictees *)
            maybe_evictees_to_lower es >>= fun () ->
            (* handle mode=persist_now, cache_state *)
            let cache_state,to_lower = 
              match mode with
              | Persist_now -> (
                  in_mem_ops.sync_key k cache_state |> function
                  | `Not_present -> failwith "impossible"
                  | `Present (e,c) -> (
                      assert(Entry.entry_type_is_dirty e.entry_type);
                      (c,Some(Delete(k,callback))))
                  (* FIXME order or evictees vs insert? *))
              | Persist_later -> (cache_state,None)
            in
            maybe_to_lower to_lower >>= fun () ->
            set_lru {lru with cache_state}) >>= fun () ->
      (if mode=Persist_later then async(callback) else return ())
    in

    let sync_key k callback = 
      with_lru (fun ~lru ~set_lru -> 
          in_mem_ops.sync_key k lru.cache_state |> function
          | `Not_present -> async (callback)
          | `Present (e,cache_state) ->             
            match Entry.entry_type_is_dirty e.entry_type with
            | false -> 
              (* just return *)
              async (callback)
            | true -> 
              let to_lower =
                match e.entry_type with
                | Insert{value;dirty} -> Some(Insert(k,value,callback))
                | Delete{dirty} -> Some(Delete(k,callback))
                | _ -> None
              in
              maybe_to_lower to_lower >>= fun () ->
              set_lru {lru with cache_state})
    in
    fun kk -> kk ~find ~insert ~delete ~sync_key



  (* export ----------------------------------------------------------- *)

  (* open Lru_interface *)

  (** Construct the LRU callback-oriented interface *)
  let make_lru_callback_ops ~monad_ops ~async ~with_lru_ops ~to_lower =
    make_lru_ops' ~monad_ops ~async ~with_lru_ops ~to_lower @@ fun ~find ~insert ~delete ~sync_key -> 
    let open Mt_callback_ops in
    {find;insert;delete;sync_key}



  (* implement non-callback interface --------------------------------- *)

  (** We now implement the standard (non-callback) interface, using events *)

  (** We assume that there is a way to "fulfill" an 'a m with an 'a  *)
  type 't event_ops = 't Tjr_monad.Event.event_ops 


  let make_lru_ops ~monad_ops ~event_ops ~(callback_ops:('k,'v,'t)Mt_callback_ops.mt_callback_ops) =
    let open Tjr_monad.Event in
    let ( >>= ) = monad_ops.bind in 
    let return = monad_ops.return in
    let Mt_callback_ops.{find;insert;delete;sync_key} = callback_ops in
    let {ev_create=create;ev_signal=signal;ev_wait=wait} = event_ops in
    let find k = 
      create() >>= fun e ->
      (find k (fun v -> signal e v)) >>= fun () -> wait e
    in
    let insert mode k v =
      create() >>= fun e ->
      (insert mode k v (fun () -> signal e ())) >>= fun () -> wait e
    in
    let delete mode k = 
      create() >>= fun e ->
      (delete mode k (fun () -> signal e ())) >>= fun () -> wait e
    in
    let sync_key k =
      create() >>= fun e -> 
      (sync_key k (fun () -> signal e ())) >>= fun () -> wait e
    in
    let sync_all_keys () =
      failwith "FIXME TODO not implemented yet"
    in
    let open Mt_ops in
    {find;insert;delete;sync_key;sync_all_keys}

end

let make_lru_callback_ops = Internal2.make_lru_callback_ops

let make_lru_ops : monad_ops:'t monad_ops ->
event_ops:'t Event.event_ops ->
callback_ops:('k, 'v, 't) Mt_callback_ops.mt_callback_ops ->
('k, 'v, 't) Mt_ops.mt_ops
= Internal2.make_lru_ops
