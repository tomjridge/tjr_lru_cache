(** The in-memory cache state *)


open Im_intf


(* FIXME duplicated *)
(** FIXME tests are enabled; disable for production *)
let test f = f ()

(* module Poly_map = Tjr_map.With_pervasives_compare (\* FIXME placeholder *\) *)



(* cache state operations ------------------------------------------- *)

(* We need to ensure the map and queue are coordinated *)

let then_ f x = (if x=0 then f () else x)

let compare c1 c2 =
  test(fun _ -> assert (c1.max_size = c2.max_size));
  (Pervasives.compare c1.current_time c2.current_time) |> then_
    (fun () -> Pervasives.compare 
        (c1.cache_map |> c1.cache_map_ops.bindings)
        (c2.cache_map |> c2.cache_map_ops.bindings)) |> then_
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
    c.cache_map_ops.cardinal c.cache_map <= c.max_size);
  assert (Queue.cardinal c.queue <= c.max_size);
  assert (c.cache_map_ops.cardinal c.cache_map = Queue.cardinal c.queue);
  c.cache_map_ops.iter (fun k entry -> assert(Queue.find entry.atime c.queue = k)) c.cache_map;
  true
  
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
   cache_map=c.cache_map_ops.map (fun entry -> {entry with atime = Map_int.find entry.atime (!t_map)}) c.cache_map;
   queue=(!queue) }




(** Construct the initial cache using some relatively small values for
   [max_size] etc. *)
let mk_initial_cache ~max_size ~evict_count ~cache_map_ops = {
  max_size;
  evict_count;
  current_time=0;
  cache_map_ops;
  cache_map=cache_map_ops.empty;
  queue=Queue.empty
}


(*
let mk_initial_cache_4_2 ~compare_k = {
  max_size=4;
  evict_count=2;
  current_time=0;
  cache_map=((Poly_map.empty compare_k):('k,'v)Poly_map.t);
  queue=Queue.empty
}
*)
