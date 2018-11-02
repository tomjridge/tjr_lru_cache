(** A single-threaded in-memory LRU cache, as a prelude to the real
   thing.

We assume all operations complete quickly from the cache. Operations
   that need to go to disk are modelled using explicit continuation
   passing.

We allow values to be tagged with extra info (this will be used to
   record whether the value needs to be flushed to the lower
   level). ?FIXME

The difficulty is how to extend this to deal with disk-backed
   eviction.

NOTE that the operations occur not in a monad - instead, explicit
   state passing is used, and we return the possible evictees; this is
   a prelude to the real version where we need to do something with
   the evictees

*)

(** FIXME tests are enabled; disable for production *)
let test f = f ()

(** The cache maintains an internal clock. *)
type time = int

(** Entries are marked using a bool; true means "this is dirty". *)
type dirty = bool

module Map_int = Tjr_fs_shared.Map_int

(** We maintain a queue as a map from time to key that was accessed at
   that time. *)
module Queue = Map_int

(** Cache map entries; values in the map are tagged with a last-accessed time and a dirty flag

Entries in the cache for key k:

- Insert v (dirty=true/false)
    {ul {li this occurs on insert}}
- Delete   (dirty=true/false)
- Lower vopt 
    {ul {li this occurs when we check the lower layer for a non-existing entry in cache}}
- (No entry)
    {ul {li for a key that hasn't been seen before}}

Additionally, each entry has a last-accessed time

 *)

type 'v entry_type = 
  | Insert of { value: 'v; dirty:dirty }
  | Delete of { dirty:dirty }
  | Lower of 'v option

let is_Lower = function Lower _ -> true | _ -> false 

type 'v entry = { entry_type: 'v entry_type; atime: time }


(* cache state ------------------------------------------------------ *)

(** The [cache_state] consists of:

- [max_size]: the max number of entries in the cache

- [evict_count]: number of entries to evict when cache full; for
   [tjr_kv] performance is best when the [evict_count] is such that
   the evictees fit nicely in a block

- [current_time]: the current time (monotonically increasing); increased on
   each operation

- [cache_map]: the cache entries, a map from key to ['v value]

- [queue]: a map from time to key that was accessed at that time; only
   holds the latest time a key was accessed (earlier entries for key k
   are deleted when a new operation on k occurs).


NOTE the [queue] field allows to identify the least recently used without
walking the entire map.

*)

type ('k,'v) cache_state = {  
  max_size: int;
  evict_count: int; (* number to evict when cache full *)
  current_time: time;
  cache_map: ('k,'v entry) Poly_map.t;  
  queue: 'k Queue.t; (** map from time to key that was accessed at that time *)
}



(* ops type --------------------------------------------------------- *)

module Lru_in_mem_ops = struct

  (** This type is what is returned by the [make_lru_in_mem] function *)
  type ('k,'v,'t) lru_in_mem_ops = {
    find: 'k -> ('k,'v) cache_state -> 
      [ `In_cache of 'v entry
      | `Not_in_cache of
           vopt_from_lower:'v option ->
           cache_state:('k, 'v) cache_state ->
           'v option * 
           [ `Evictees of ('k * 'v entry) list option ] *
           [ `Cache_state of ('k, 'v) cache_state ] ];

    insert: 'k -> 'v -> ('k,'v) cache_state ->
      [ `Evictees of ('k * 'v entry) list option ] *
      [ `Cache_state of ('k, 'v) cache_state ];

    delete: 'k -> ('k, 'v) cache_state ->
      [ `Evictees of ('k * 'v entry) list option ] *
      [ `Cache_state of ('k, 'v) cache_state ]
  }
end



(*
(* res -------------------------------------------------------------- *)

(** Each operation produces a value (possibly unit), an optional list of evictees, and an updated cache state *)
type ('a,'k,'v) res = { ret_val: 'a; es: ('k*'v entry) list option; c:('k,'v)cache_state }
*)



(* cache state operations ------------------------------------------- *)

(* We need to ensure the map and queue are coordinated *)

let then_ f x = (if x=0 then f () else x)

let compare c1 c2 =
  test(fun _ -> assert (c1.max_size = c2.max_size));
  (Pervasives.compare c1.current_time c2.current_time) |> then_
    (fun () -> Pervasives.compare 
        (c1.cache_map |> Poly_map.bindings)
        (c2.cache_map |> Poly_map.bindings)) |> then_
    (fun () -> Pervasives.compare
        (c1.queue |> Map_int.bindings)
        (c2.queue |> Map_int.bindings))


(** Cache wellformedness, check various invariants. 

- the cache never has more than max_size elts 
- the queue never has more than max_size elts 
- all k in map are in the queue; iff
- map and queue agree on timings

*)
let wf c =
  test @@ fun () -> 
  assert (
    (* Printf.printf "wf: %d %d\n" (Poly_map.cardinal c.cache_map) c.max_size; *)
    Poly_map.cardinal c.cache_map <= c.max_size);
  assert (Queue.cardinal c.queue <= c.max_size);
  assert (Poly_map.cardinal c.cache_map = Queue.cardinal c.queue);
  Poly_map.iter (fun k entry -> assert(Queue.find entry.atime c.queue = k)) c.cache_map;
  ()
  
(** For testing, we typically need to normalize wrt. time *)
let normalize c =
  (* we need to map times to times *)
  let t_map = ref Queue.empty in
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
   current_time=(!time);
   cache_map=Poly_map.map (fun entry -> {entry with atime = Map_int.find entry.atime (!t_map)}) c.cache_map;
   queue=(!queue) }




(** Construct the initial cache using some relatively small values for
   [max_size] etc. *)
let mk_initial_cache ~compare_k = {
  max_size=4;
  evict_count=2;
  current_time=0;
  cache_map=((Poly_map.empty compare_k):('k,'v)Poly_map.t);
  queue=Queue.empty
}


(* find_in_cache, get_evictees  --------------------------------- *)


(* NOTE evictees are removed from the map, so no need to mark clean *)

let tick c = { c with current_time=c.current_time+1}

(** Attempt to locate key in cache. *)
let find_in_cache ~update_time (k:'k) (c:('k,'v)cache_state) = 
  let c = tick c in
  try 
    let e = Poly_map.find k c.cache_map in          
    (* update time *)
    let c = 
      if update_time then
        {c with 
         cache_map=Poly_map.add k {e with atime=c.current_time} c.cache_map;
         queue=c.queue |> Queue.remove e.atime |> Queue.add c.current_time k}
      else 
        c
    in
    (Some e,c)
  with Not_found -> 
    (None,c)


let _ = find_in_cache



(** An exception, for quick abort. FIXME remove *)
exception E_

(** Returns None if no evictees need to be flushed, or Some(evictees)
   otherwise *)
let get_evictees (c:('k,'v)cache_state) = 
  let card = Poly_map.cardinal c.cache_map in
  match card > c.max_size with (* FIXME inefficient *)
  | false -> (None,c)
  | true -> 
    (* how many to evict? *)
    let n = c.evict_count in
    (* for non-dirty, we just remove from map; for dirty we
       must flush to lower *)
    let count = ref 0 in
    let evictees = ref [] in
    let queue = ref c.queue in  
    let cache_map = ref c.cache_map in
    begin 
      try 
        Queue.iter 
          (fun time k -> 
             queue:=Queue.remove time !queue;
             evictees:=(k,Poly_map.find k c.cache_map)::!evictees;
             cache_map:=Poly_map.remove k !cache_map;
             count:=!count +1;
             if !count >= n then raise E_ else ())
          c.queue
      with E_ -> ()
    end;
    (* now we have evictees, new queue, and new map *)
    let c = {c with cache_map=(!cache_map); queue=(!queue)} in
    (Some (!evictees),c)
    
let _ = get_evictees


(** Construct the cached map on top of an existing map.

NOTE the idea for [find] is that we execute a quick step to handle the
   case that there is an entry in the cache. If there isn't, we make a
   slow call to the lower layer, and when the result arrives we make
   another update to the cache state with the then current state of
   the cache. To avoid the risk of stale results being returned from
   lower, we have to tag lower results with some kind of
   monotonically-increasing index.


*)
let make_cached_map () =

  (* NOTE that if find pulls an entry into the cache, we may have to
       get rid of evictees; so a read causes a write; but this should be
       relatively infrequent *)
  let find (k:'k) c = 
    find_in_cache ~update_time:true k c |> fun (v,c) -> 
    match v with
    | None -> 
      `Not_in_cache (fun ~vopt_from_lower ~cache_state ->          
          let c = cache_state in
          let c = tick c in
          (* need to update map before returning *)
          (* FIXME concurrency concerns: v may be stale if we get from lower, but cache updated in meantime *)
          let new_entry = Lower vopt_from_lower in
          { c with
            cache_map=Poly_map.add k 
                {entry_type=new_entry; atime=c.current_time} 
                c.cache_map;
            queue=c.queue |> Queue.add c.current_time k } |> fun c ->
          (* also may need to evict *)
          get_evictees c |> fun (es,c) ->
          (vopt_from_lower,`Evictees es, `Cache_state c))
    | Some v -> 
      `In_cache v
  in

  let _ = find in


  (* FIXME TODO we need to also handle the cases where we flush to lower immediately *)

  (* NOTE entry_type is not Lower *)
  let perform k entry_type c = 
    assert(not (is_Lower entry_type));
    let c = tick c in
    let e = 
      try 
        Some (Poly_map.find k c.cache_map)
      with Not_found -> None 
    in
    let e' = {entry_type; atime=c.current_time } in
    (* new entry in cache_map *)
    let c = {c with cache_map=(Poly_map.add k e' c.cache_map) } in
    (* maybe remove existing entry from queue *)
    let c = 
      match e with
      | None -> c
      | Some e -> 
        {c with queue=(Queue.remove e.atime c.queue) } 
    in
    (* add new entry *)
    let c = {c with queue=(Queue.add e'.atime k c.queue) } in
    get_evictees c |> fun (es,c) -> 
    (`Evictees es, `Cache_state c)
  in

  let insert k v c = perform k (Insert {value=v; dirty=true }) c in

  (* TODO make insert_many more efficient *)
  (* let insert_many k v kvs c = insert k v c |> fun c -> (kvs,c) in *)

  let delete k c = perform k (Delete {dirty=true}) c in
  fun kk -> kk ~find ~insert ~delete



let _ = make_cached_map


include Lru_in_mem_ops

let make_lru_in_mem () = 
  make_cached_map () @@ fun ~find ~insert ~delete -> 
  let open Lru_in_mem_ops in
  { find; insert; delete }
