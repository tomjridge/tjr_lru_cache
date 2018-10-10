(** A single-threaded in-memory LRU cache, as a prelude to the real
   thing.

We assume all operations complete quickly. We allow values to be
   tagged with extra info (this will be used to record whether the
   value needs to be flushed to the lower level).  

The difficulty is how to extend this to deal with disk-backed eviction.

*)

(** The cache maintains an internal clock. *)
type time = int

(** Entries are marked using a bool; true means "this is dirty". *)
type dirty = bool

module Map_int = Tjr_fs_shared.Map_int

(** We maintain a queue as a map from time to key that was accessed at
   that time. *)
module Queue = Map_int

(** Cache map entries; values in the map are tagged with a last-accessed time and a dirty flag

Description of the value option and dirty flag: [None] indicates known 
not to be present at lower (if
[dirty=false]), or has been deleted (if [dirty=true]); [Some v] with
[dirty=true] indicates that this needs to be flushed to lower map. 

 *)

type 'v op = Insert of 'v | Delete 

type 'v entry = { value: 'v op; dirty:dirty; time:time }



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

(* cache state operations ------------------------------------------- *)

(* We need to ensure the map and queue are coordinated *)


(** Construct the initial cache using some relatively small values for
   [max_size] etc. *)
let mk_initial_cache ~compare_k = {
  max_size=8;
  evict_count=4;
  current_time=0;
  cache_map=((Poly_map.empty compare_k):('k,'v)Poly_map.t);
  queue=Queue.empty
}


(** An exception, for quick abort. FIXME remove *)
exception E_


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
         cache_map=Poly_map.add k {e with time=c.current_time} c.cache_map;
         queue=c.queue |> Queue.remove e.time |> Queue.add c.current_time k}
      else 
        c
    in
    (Some e,c)
  with Not_found -> 
    (None,c)


let _ = find_in_cache



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
    let c' = {c with cache_map=(!cache_map); queue=(!queue)} in
    (Some (!evictees),c)
    
let _ = get_evictees


(** Each operation produces a value (possibly unit), an optional list of evictees, and an updated cache state *)
type ('a,'k,'v) res = { ret_val: 'a; es: ('k*'v entry) list option; c:('k,'v)cache_state }

(** Construct the cached map on top of an existing map. *)
(* FIXME we need to do something with the evictees *)
let make_cached_map ~lower_find =

  (* NOTE that if find pulls an entry into the cache, we may have to
       get rid of evictees; so a read causes a write; but this should be
       relatively infrequent *)
  let find (k:'k) c : ('v op,'k,'v) res = 
    find_in_cache ~update_time:true k c |> fun (v,c) -> 
    match v with
    | None -> 
      lower_find k c |> fun (v,c) -> 
      let c = tick c in
      (* need to update map before returning *)
      (* FIXME concurrency concerns: v may be stale *)
      (v,{c with
          cache_map=Poly_map.add k {value=v; time=c.current_time; dirty=false} c.cache_map;
          queue=c.queue |> Queue.add c.current_time k}) |> fun (v,c) ->
      (* also may need to evict *)
      get_evictees c |> fun (es,c) ->
      { ret_val=v; es; c}
    | Some e -> {ret_val=e.value;es=None;c}
  in

  let _ = find in

  let perform k op c = 
    let c = tick c in
    let e = 
      try 
        Some (Poly_map.find k c.cache_map)
      with Not_found -> None 
    in
    let e' = { value=op; time=c.current_time; dirty=true } in
    (* new entry in cache_map *)
    let c = {c with cache_map=(Poly_map.add k e' c.cache_map) } in
    (* maybe remove existing entry from queue *)
    let c = 
      match e with
      | None -> c
      | Some e -> 
        {c with queue=(Queue.remove e.time c.queue) } 
    in
    (* add new entry *)
    let c = {c with queue=(Queue.add e'.time k c.queue) } in
    get_evictees c |> fun (es,c) -> 
    { ret_val=(); es; c }
  in

  let insert k v c = perform k (Insert v) c in

  (* TODO make insert_many more efficient *)
  (* let insert_many k v kvs c = insert k v c |> fun c -> (kvs,c) in *)

  let delete k c = perform k Delete c in
  fun kk -> kk ~find ~insert ~delete





(* FIXME Test *)
