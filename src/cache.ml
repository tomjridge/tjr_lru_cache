(** An LRU cache on top of a map. *)


(* TODO

- ensure concurrency is handled correctly
- implement leaf_stream, insert_many

*)

(*

* Sync behaviour -------------------------------------------------------

Our main use for this module is as a frontend to an uncached KV store
(see tjr_kv). 

There we target the following API: FIXME perhaps move the api here?

type blocking_mode = Continue | Block

type persistent_mode = Transient | Persistent of blocking_mode

type mode = persistent_mode

type ('k,'v,'t) pmap_ops = {
  find: 'k -> ('v option,'t) m;
  insert: mode -> 'k -> 'v -> (unit,'t) m;
  delete: mode -> 'k -> (unit,'t) m;
  sync_key: blocking_mode -> 'k -> (unit,'t) m;
  sync_all_keys: blocking_mode -> unit -> (unit,'t) m;
}

This API is expected to be used by many threads. In particular, for [sync_key]:

Concurrent actions:

- thread A: insert_T(k,v); sync_key(k)
- thread B: insert_T(k,v'); sync_key(k)

Then the insert of v' may be synced by thread A's sync_key.


Similarly, for [sync_all_keys]:

- thread A: insert_T(k,v); sync_all_keys
- thread B: insert_T(k,v')

Thread A's call to sync_all_keys may result in (k,v') rather than (k,v).


* Non-blocking design --------------------------------------------------

We want sync operations to be non-blocking.

- For the thread that issues the sync, blocking_mode=Continue means that we don't wait for the sync to complete

- Transient operations from other threads should not block while a sync is taking place.

- A concurrent sync_all_keys operation (perhaps occuring halfway through an existing sync) should avoid re-syncing keys that have already been synced.


The design (for tjr_kv) has a single active thread servicing the pcache. If this thread is solely responsible for sync_all_keys, then transient operations will block (till the thread completes and can service another message from the LRU cache).

Thus, the question is how to service the sync_all_keys operation in the pcache.

FIXME another issue: how to suspend a thread in the LRU till we receive notification of the sync completion. For LWT we can just pass some mbox var. For the API perhaps we have a callback function which updates the system state.

NOTE LMDB has "There can be multiple simultaneously active read-only transactions but only one that can write. Once a single read-write transaction is opened, all further attempts to begin one will block until the first one is committed or aborted."

* Consumed API ---------------------------------------------------------

The LRU provides a "syncable/blocking" map API.

FIXME terminology: perhaps persistent_mode should be sync_mode? Or should we separate the sync operation from the durability and blocking requirement?

FIXME sync is a bad word because it implies symmetrical operation, whereas a sync is really an operation at the API which forces changes downwards (flush has this connotation).

We build it on top of an API which exposes map-like operations together with a sync operation. So the LRU is really just an LRU cache of a syncable map. 

A non-blocking operation can put a msg on the queue and return immediately. A blocking operation has to listen for the reply before returning. There is a question of whether to implement a reply queue or use some simpler mechanism (callback or mbox var). Probably best to use a simple callback of type () -> 'a m


* Marking entries clean ------------------------------------------------

On a sync_all_keys call, we can dispatch to the lower layer, and mark all entries clean at that point, without waiting for the return. We only need to wait for the return if the sync_all_keys call is blocking (but we can still mark the entries clean... the blocking behaviour is a notification thing, but marking entries clean is just to record in memory that these entries no longer need to be flushed).


*)

(* we want to be able to take eg a map_ops and produce a cached
   version *)

open Tjr_monad.Monad
open Tjr_btree
(* open Base_types *)
open Map_ops

(** The cache maintains an internal clock. *)
type time = int

(** Dirty blocks are marked using a bool. *)
type dirty = bool

(** We maintain a queue as a map from time to key that was accessed at
   that time. *)
module Queue = Map_int

(* following needs polymorphic map - 
   batteries? no, only Pervasives.compare
   extlib? yes, allows parameterization by compare 

FIXME can also use first class modules to produce polymorphic ops
*)

(** Polymorphic map module. *)
module Pmap = struct 
  type ('k,'v) t = (*ExtLib.*) ('k,'v)PMap.t
  let map: ('v -> 'u) -> ('k,'v) t -> ('k,'u) t = PMap.map
  (* FIXME bindings and cardinal should be in ExtLib; also others *)
  let bindings: ('k,'v) t -> ('k * 'v) list = (fun m -> 
      let bs = ref [] in
      PMap.iter (fun k v -> bs:=(k,v)::!bs) m;
      List.rev !bs)
  let empty: ('k -> 'k -> int) -> ('k,'v) t = PMap.create
  let cardinal: ('k,'v) t -> int = (fun m ->
      let x = ref 0 in
      PMap.iter (fun k v -> x:=!x+1) m;
      !x)
  let iter: ('k -> 'v -> unit) -> ('k,'v) t -> unit = PMap.iter
  let find: 'k -> ('k,'v) t -> 'v = PMap.find
  let remove: 'k -> ('k,'v) t -> ('k,'v) t = PMap.remove
  let add: 'k -> 'v -> ('k,'v) t -> ('k,'v) t = PMap.add
end


(** The [cache_state] consists of:

- [max_size]: the max number of entries in the cache
- [evict_count]: number of entries to evict when cache full
- [current]: the current time (monotonically increasing)
- [map]: the cached part of the map (from key to value option, with a time and a dirty flag
- [queue]: a map from time to key that was accessed at that time; only holds the latest time a key was accessed (earlier entries for key k are deleted when a new operation on k occurs). 

The [map] field, value option and dirty flag: [None] indicates known 
not to be present at lower (if
[dirty=false]), or has been deleted (if [dirty=true]); [Some v] with
[dirty=true] indicates that this needs to be flushed to lower map. 

The [queue] field allows to identify the least recently used without walking the entire map.

*)
type ('k,'v) cache_state = {  
  max_size: int;
  evict_count: int; (* number to evict when cache full *)
  current: time;
  map: ('k,'v option*time*dirty) Pmap.t;  
  queue: 'k Queue.t; (* map from time to key that was accessed at that time *)
}

(** The [cache_ops] are a monadic reference to the cache_state. *)
type ('k,'v,'t) cache_ops = ( ('k,'v)cache_state,'t) Tjr_monad.Mref_plus.mref


(** For testing, we typically need to normalize wrt. time *)
let normalize c =
  (* we need to map times to times *)
  let t_map = ref Map_int.empty in
  let time = ref 0 in
  let queue = ref Queue.empty in
  Queue.iter
    (fun t k -> 
       let t' = (!time) in
       t_map:=Map_int.add t t' (!t_map);
       queue:=Queue.add t' k (!queue);
       time:=!time+1;
       ())
    c.queue;
  {c with
   current=(!time);
   map=Pmap.map (fun (v,t,d) -> (v,Map_int.find t (!t_map),d)) c.map;
   queue=(!queue) }

let then_ f x = (if x=0 then f () else x)

(* FIXME a bit horrible! *)
(** For testing we need to compare two caches. *)
let compare c1 c2 =
  Test.test(fun _ -> assert (c1.max_size = c2.max_size));
  (Pervasives.compare c1.current c2.current) |> then_
    (fun () -> Pervasives.compare 
        (c1.map |> Pmap.bindings)
        (c2.map |> Pmap.bindings)) |> then_
    (fun () -> Pervasives.compare
        (c1.queue |> Map_int.bindings)
        (c2.queue |> Map_int.bindings))

(** Construct the initial cache using some relatively small values for
   [max_size] etc. *)
let mk_initial_cache ~compare_k = {
  max_size=8;
  evict_count=4;
  current=0;
  map=((Pmap.empty compare_k):('k,'v)Pmap.t);
  queue=Queue.empty
}


(* the cache never has more than max_size elts; the queue never has
     more than max_size elts 

     all k in map are in the queue; iff

     map and queue agree on timings

*)
(** Cache wellformedness, check various invariants. *)
let wf c =
  Test.test @@ fun () -> 
  assert (Pmap.cardinal c.map <= c.max_size);
  assert (Queue.cardinal c.queue <= c.max_size);
  assert (Pmap.cardinal c.map = Queue.cardinal c.queue);
  Pmap.iter (fun k (v,t,d) -> assert(Queue.find t c.queue = k)) c.map;
  ()


(** An exception, for quick abort. FIXME remove *)
exception E_

(* FIXME correctness of following not clear *)

(* FIXME document the following more *)
(* flush evictees, assuming already removed from c; map_ops is the ops
   of the underlying layer; exposed so can call when we want to flush
   the entire cache *)
(* FIXME the following targets only insert and delete in the lower
   map; but we want to target insert_many probably *)
(** We [sync_evictees] by calling the relevant operations on the lower map. *)
let sync_evictees ~monad_ops ~map_ops =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  dest_map_ops map_ops @@ fun ~find ~insert ~delete ~insert_many ->
  let rec loop es =
    match es with
    | [] -> return ()
    | e::es -> (
        let (k,(vopt,time,dirty)) = e in
        match vopt with 
        | None -> (
            match dirty with
            | false -> loop es
            | true -> (
                (* need to delete *)
                (* FIXME and mark clean? *)
                delete k >>= (fun () -> loop es) ))
        | Some v -> (
            match dirty with
            | false -> loop es
            | true -> (
                (* write out and continue *)
                (* FIXME and mark clean? *)
                insert k v >>= (fun () -> loop es) )))
  in
  fun evictees -> loop evictees


open Tjr_monad.Mref_plus

let mark_entry_clean' k map =
  let dirty = false in
  try 
    Pmap.find k map |> fun (v,t,dirty_) ->
    Pmap.add k (v,t,dirty) map |> fun map ->
    map
  with Not_found -> map

let mark_entry_clean ~cache_ops = 
  fun k -> 
    cache_ops.with_ref (fun c -> 
        let c' = mark_entry_clean' k c in
        (),c')
      
let mark_all_clean' map =
  let dirty = false in
  Pmap.map (fun (v,t,d) -> (v,t,dirty)) map


(** If we flush all entries (on a sync), we can the mark cache as clean. *)
let mark_all_clean ~cache_ops = 
  cache_ops.with_ref (fun c -> 
      let c' = mark_all_clean' c in
      (),c')

(*
(** [sync_all_keys] to the lower map. *)
let sync_all_keys ~monad_ops ~map_ops ~cache_ops =
  let ( >>= ) = monad_ops.bind in
  (* FIXME we want to do this in such a way that we do not block
     subsequent operations on the in-mem map *)
  cache_ops.with_ref (fun c ->
      sync_evictees ~monad_ops ~map_ops (Pmap.bindings c) >>= fun () ->
      mark_all_clean ~monad_ops ~cache_ops


(** Sync a particular key/value to the lower map. *)
let sync_key ~monad_ops ~map_ops ~cache_ops =
  let ( >>= ) = monad_ops.bind in
  fun k -> 
    cache_ops.with_ref (fun pmap ->
    try 
      sync_evictees ~monad_ops ~map_ops [(k,Pmap.find k pmap)] >>= fun () ->
      (* FIXME need to mark entry as clean; but this needs to be done atomically with the sync *)
      (),mark_

(** Construct the cached map on top of an existing map. The [evict_hook] is for testing and can be ignored. *)
let make_cached_map ~monad_ops ~map_ops ~cache_ops =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  dest_map_ops map_ops @@ fun ~find ~insert ~delete ~insert_many ->
  let evict_hook : (unit -> unit) ref = ref (fun () -> ()) in
  (* update time on each put *)
  let put_cache c = 
    let c = {c with current=c.current+1} in 
    cache_ops.set c 
  in

  let maybe_flush_evictees c = (
    let card = Pmap.cardinal c.map in
    match (card > c.max_size) with (* FIXME inefficient *)
    | false -> put_cache c
    | true -> (
        !evict_hook ();
        (* how many to evict? *)
        let n = c.evict_count in
        (* for non-dirty, we just remove from map; for dirty we
           must flush to lower *)
        let count = ref 0 in
        let evictees = ref [] in
        let queue = ref c.queue in  
        let map = ref c.map in
        begin 
          try (
            Queue.iter 
              (fun time k -> 
                 queue:=Queue.remove time !queue;
                 evictees:=(k,Pmap.find k c.map)::!evictees;
                 map:=Pmap.remove k !map;
                 count:=!count +1;
                 if !count >= n then raise E_ else ())
              c.queue) 
          with E_ -> ()
        end;
        (* now we have evictees, new queue, and new map *)
        (sync_evictees ~monad_ops ~map_ops !evictees) >>= fun () ->
        let c' = {c with map=(!map); queue=(!queue)} in
        put_cache c' ))
  in

  let find k = (
    cache_ops.get () >>= fun c ->
    (* try to find in cache *)
    try (
      let (v,time,dirty) = Pmap.find k c.map in          
      (* update time *)
      let time' = c.current in
      let c = {c with map=(Pmap.add k (v,time',dirty) c.map) } in
      (* remove entry from queue *)
      let c = {c with queue=(Queue.remove time c.queue) } in
      (* add new entry *)
      let c = {c with queue=(Queue.add time' k c.queue) } in
      (* update cache *)
      put_cache c >>= (fun () -> return v))
    with Not_found -> (
        (* retrieve from lower level *)
        find k >>= fun v -> 
        (* update cache *)
        let time = c.current in
        let dirty = false in
        let c = {c with map=(Pmap.add k (v,time,dirty) c.map) } in
        (* update queue *)
        let c = {c with queue=(Queue.add time k c.queue) } in
        maybe_flush_evictees c >>= (fun () -> return v)))
  in

  let insert k v = (
    cache_ops.get () >>= fun c -> 
    try (
      let (v_,time,dirty) = Pmap.find k c.map in          
      (* update time *)
      let time' = c.current in
      let dirty' = true in
      let c = {c with map=(Pmap.add k (Some v,time',dirty') c.map) } in
      (* remove entry from queue *)
      let c = {c with queue=(Queue.remove time c.queue) } in
      (* add new entry *)
      let c = {c with queue=(Queue.add time' k c.queue) } in
      (* update cache *)
      put_cache c)
    with Not_found -> (
        (* update cache *)
        let time = c.current in
        let dirty = true in
        let c = {c with map=(Pmap.add k (Some v,time,dirty) c.map) } in
        (* update queue *)
        let c = {c with queue=(Queue.add time k c.queue) } in
        maybe_flush_evictees c))
  in

  (* TODO make insert_many more efficient *)
  let insert_many k v kvs = insert k v >>= (fun () -> return kvs) in

  let delete k = (
    cache_ops.get () >>= fun c -> 
    try (
      let (v_,time,dirty) = Pmap.find k c.map in          
      (* update time *)
      let time' = c.current in
      let dirty' = true in
      let c = {c with map=(Pmap.add k (None,time',dirty') c.map) } in
      (* remove entry from queue *)
      let c = {c with queue=(Queue.remove time c.queue) } in
      (* add new entry *)
      let c = {c with queue=(Queue.add time' k c.queue) } in
      maybe_flush_evictees c)
    with Not_found -> (
        let time = c.current in
        let dirty = true in
        let c = {c with map=(Pmap.add k (None,time,dirty) c.map)} in
        (* add new entry to queue *)
        let c = {c with queue=(Queue.add time k c.queue) } in
        (* update cache *)            
        maybe_flush_evictees c))
  in
  let get_leaf_stream () = failwith "FIXME" in
  let cached_map_ops = 
    mk_map_ops ~find ~insert ~insert_many ~delete 
  in
  fun kk -> kk ~cached_map_ops ~evict_hook
*)
