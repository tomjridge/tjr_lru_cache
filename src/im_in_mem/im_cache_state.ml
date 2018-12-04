(** The in-memory cache state *)


open Entry.Types


(* FIXME duplicated *)
(** FIXME tests are enabled; disable for production *)
let test f = f ()


module Types = struct


  module Map_int = Tjr_map.Map_int

  (** We maintain a queue as a map from time to key that was accessed at
      that time. *)
  module Queue = Map_int


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
    cache_map: ('k,'v entry) Tjr_polymap.t;  
    queue: 'k Queue.t; 
    (** map from time to key that was accessed at that time *)
  }
end
include Types


(* cache state operations ------------------------------------------- *)

(* We need to ensure the map and queue are coordinated *)

let then_ f x = (if x=0 then f () else x)

let compare c1 c2 =
  test(fun _ -> assert (c1.max_size = c2.max_size));
  (Pervasives.compare c1.current_time c2.current_time) |> then_
    (fun () -> Pervasives.compare 
        (c1.cache_map |> Tjr_polymap.bindings)
        (c2.cache_map |> Tjr_polymap.bindings)) |> then_
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
    (* Printf.printf "wf: %d %d\n" (Tjr_polymap.cardinal c.cache_map) c.max_size; *)
    Tjr_polymap.cardinal c.cache_map <= c.max_size);
  assert (Queue.cardinal c.queue <= c.max_size);
  assert (Tjr_polymap.cardinal c.cache_map = Queue.cardinal c.queue);
  Tjr_polymap.iter (fun k entry -> assert(Queue.find entry.atime c.queue = k)) c.cache_map;
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
   cache_map=Tjr_polymap.map (fun entry -> {entry with atime = Map_int.find entry.atime (!t_map)}) c.cache_map;
   queue=(!queue) }




(** Construct the initial cache using some relatively small values for
   [max_size] etc. *)
let mk_initial_cache ~max_size ~evict_count ~compare_k = {
  max_size;
  evict_count;
  current_time=0;
  cache_map=((Tjr_polymap.empty compare_k):('k,'v)Tjr_polymap.t);
  queue=Queue.empty
}


(*
let mk_initial_cache_4_2 ~compare_k = {
  max_size=4;
  evict_count=2;
  current_time=0;
  cache_map=((Tjr_polymap.empty compare_k):('k,'v)Tjr_polymap.t);
  queue=Queue.empty
}
*)
