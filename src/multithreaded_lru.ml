(** Non-functorial multithreaded / concurrent-safe implementation of the LRU
   interface. This extends the in-memory explicit-state-passing code
   to the monad. 

Compared to the in-mem version:

- we use the monad
- we allow multiple threads to call the interface methods
- for long-running operations (eg find, when the key is not in the i-m
  cache), we maintain a queue of blocked threads, make a single call
  to the lower layers, and when this returns we release the threads
- we have explicit messages to the lower layer
- the api includes "persist mode" (now or later) to indicate whether
  insert/delete operations should persist immediately or not

Given that the likely target is Lwt, this is probably overkill.

*)

(* FIXME/TODO

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


- for in-mem, when we return with lim_state and evictees, we aim to
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
open Mt_intf

module type S = sig
  type t
  val monad_ops : t monad_ops
  val async     : t async
  val event_ops : t event_ops
end

module Make(S:S) = struct
  module S=S
  open S

  let ( >>= ) = monad_ops.bind 
  let return = monad_ops.return

  (** {2 Util} *)

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
  let run_callbacks_for_key ~k ~vopt_from_lower ~lru =
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



  (** {2 Callback interface} *)

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
  let make_callback_ops (type k v lru)
      ~(lru_ops_im:(k,v,lru)lru_ops_im)
      ~with_lru
      ~(to_lower:(k, v, t) lru_msg -> (unit, t) m)
    =
    let open (struct

      let maybe_to_lower = function
        | None -> return ()
        | Some msg -> to_lower msg
      
      let evictees_to_msg_option = function
        | None -> None
        | Some [] -> failwith __LOC__  (* assume evictees are never [] *)
        | Some es -> Some (Evictees es)
      
      let maybe_evictees_to_lower es = maybe_to_lower (evictees_to_msg_option es)

      let with_lru = with_lru.with_state

      (* let run_callbacks_for_key = run_callbacks_for_key ~monad_ops ~async in *)
      let find k (callback:'v option -> (unit,'t) m) = 
        with_lru (fun ~state:lru ~set_state:set_lru -> 
            (* check if k is already in the cache *)
            lru_ops_im.find k lru.lim_state |> function
            | In_cache e -> (
                let vopt = 
                  match e with
                  | Insert i -> (Some i.value)
                  | Delete _ -> None
                  | Lower vopt -> vopt
                in
                async (fun () -> callback vopt))
            (* FIXME note the callback should not do computation on
               receiving an argument... it must wait for the world state
            *)
            | Not_in_cache kk -> (
                (* now we have to call to the lower level; first we check
                   that a call isn't already in process... *)
                add_callback_on_key ~k ~callback ~lru |> fun (lru,lower_call_status) -> 
                match lower_call_status with
                | `Lower_call_in_progress -> set_lru lru  (* and return *)
                | `Need_to_call_lower -> 
                  (* we want to issue a `Find k call to lower, with a
                     callback that unblocks the waiting threads *)
                  let callback vopt_from_lower : (unit,'t)m =                    
                    with_lru (fun ~state:lru ~set_state:set_lru -> 
                        (* first wake up sleeping threads *)
                        run_callbacks_for_key ~k ~vopt_from_lower ~lru >>= fun lru -> 
                        kk ~vopt_from_lower ~lru_state_im:lru.lim_state 
                        |> fun (vopt,exc) -> 
                        (* have to handle evictees by flushing to lower *)
                        maybe_evictees_to_lower exc.evictees >>= fun () ->
                        set_lru {lru with lim_state=exc.lru_state_im })
                  in
                  to_lower (Find(k,callback))))      

      let insert mode k v (callback: unit -> (unit,'t)m) =
        with_lru (fun ~state:lru ~set_state:set_lru -> 
            (* mark "ab"; *)
            lru_ops_im.insert k v lru.lim_state |> fun exc ->
            (* mark "bc"; *)
            (match exc.evictees with
             | None -> return ()
             | Some es -> to_lower (Evictees es)) >>= fun () ->
            (* handle mode=persist_now, lim_state *)
            (* mark "cd"; *)
            let lim_state,to_lower = 
              match mode with
              | Persist_now -> (
                  lru_ops_im.sync_key k exc.lru_state_im |> function
                  | Not_in_cache () -> failwith "impossible"
                  | In_cache (e,c) -> (
                      assert(Entry.entry_is_dirty e);
                      (c,Some(Insert(k,v,callback)))))
              (* FIXME order or evictees vs insert? *)
              | Persist_later -> (exc.lru_state_im,None)
            in
            maybe_to_lower to_lower >>= fun () ->
            (* mark "de"; *)
            set_lru {lru with lim_state}) 
        >>= fun () ->
        (* mark "ef"; *)
        (* FIXME check the following *)
        (if mode=Persist_later then async(callback) else return ())

      let delete mode k callback = 
        with_lru (fun ~state:lru ~set_state:set_lru -> 
            lru_ops_im.delete k lru.lim_state 
            |> function exc ->
              (* update lru with evictees *)
              maybe_evictees_to_lower exc.evictees >>= fun () ->
              (* handle mode=persist_now, lim_state *)
              let lim_state,to_lower = 
                match mode with
                | Persist_now -> (
                    lru_ops_im.sync_key k exc.lru_state_im |> function
                    | Not_in_cache () -> failwith "impossible"
                    | In_cache (e,c) -> (
                        assert(Entry.entry_is_dirty e);
                        (c,Some(Delete(k,callback))))
                    (* FIXME order or evictees vs insert? *))
                | Persist_later -> (exc.lru_state_im,None)
              in
              maybe_to_lower to_lower >>= fun () ->
              set_lru {lru with lim_state})
        >>= fun () ->
        (if mode=Persist_later then async(callback) else return ())

      let sync_key k callback = 
        with_lru (fun ~state:lru ~set_state:set_lru -> 
            lru_ops_im.sync_key k lru.lim_state |> function
            | Not_in_cache () -> async (callback)
            | In_cache (e,lim_state) ->             
              match Entry.entry_is_dirty e with
              | false -> 
                (* just return *)
                async (callback)
              | true -> 
                let to_lower =
                  match e with
                  | Insert{value;dirty} -> Some(Insert(k,value,callback))
                  | Delete{dirty} -> Some(Delete(k,callback))
                  | _ -> None
                in
                maybe_to_lower to_lower >>= fun () ->
                set_lru {lru with lim_state})
    end)
    in
    Mt_callback_ops.{find;insert;delete;sync_key}



  (** {2 Event-based interface}

      We now implement the standard (non-callback) interface, using events.
  *)


  let make_lru_ops ~(callback_ops:('k,'v,'t)Mt_callback_ops.mt_callback_ops) =
    let Mt_callback_ops.{find;insert;delete;sync_key} = callback_ops in
    let {ev_create=create;ev_signal=signal;ev_wait=wait} = event_ops in
    let mt_find k = 
      create() >>= fun e ->
      (find k (fun v -> signal e v)) >>= fun () -> wait e
    in
    let mt_insert mode k v =
      create() >>= fun e ->
      (insert mode k v (fun () -> signal e ())) >>= fun () -> wait e
    in
    let mt_delete mode k = 
      create() >>= fun e ->
      (delete mode k (fun () -> signal e ())) >>= fun () -> wait e
    in
    let mt_sync_key k =
      create() >>= fun e -> 
      (sync_key k (fun () -> signal e ())) >>= fun () -> wait e
    in
    let mt_sync_all_keys () =
      failwith "FIXME TODO not implemented yet"
    in
    let open Mt_ops in
    {mt_find;mt_insert;mt_delete;mt_sync_key;mt_sync_all_keys}

  let (_ : callback_ops:('k, 'v, t) mt_callback_ops -> ('k, 'v, t) mt_ops) =
    make_lru_ops
    

  (** {2 Join event-based interface to callback interface} *)

  let make ~lru_ops_im ~with_lru ~to_lower = 
    make_callback_ops ~lru_ops_im ~with_lru ~to_lower |> fun callback_ops -> 
    make_lru_ops ~callback_ops
    
end


module Examples = struct

  module type T = sig
    type k
    type v
    type lru
    type t = lwt
    val lru_factory : (k,v,lru,t) lru_factory
  end

  module Int_int : T with type k=int and type v=int = struct
    type k = int
    type v = int
    type t = lwt 

    module I = Lru_in_mem.Examples.Int_int
                 
    let empty ~max_size ~evict_count = 
      let s = I.make_init_lru_state_im ~max_size ~evict_count in
      mt_initial_state ~initial_lim_state:s ~k_cmp:Int_.compare
    
    module M = Make(struct include With_lwt type t = lwt end)

    type t_map = (k, (v, t) blocked_thread list, unit) Tjr_map.map

    type lru = (k, v, I.lru, t_map, t) mt_state

    let make_ops ~with_state ~to_lower = 
      M.make ~lru_ops_im:I.lru_ops_im ~with_lru:with_state ~to_lower
        
    let lru_factory : _ lru_factory = object
      method empty=empty
      method make_ops=make_ops
    end

    let _ = lru_factory

  end
end
