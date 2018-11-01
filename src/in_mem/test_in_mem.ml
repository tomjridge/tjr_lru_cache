(* testing in mem cache ---------------------------------------- *)

(* open Tjr_btree *)
(* open Base_types *)
open In_mem_cache

module Cache = In_mem_cache

(* we test just cache behaviour, not linked with btree *)

(* in memory *)

(* note that the use of time means that we need to normalize timings
   (and current time) in order to exhaust state space *)

type key = int
type value = int
type op = Find of key | Insert of key * value | Delete of key


module Test_state = struct 

  
  (* the spec state is the combined view of the cache and the base map ? *)
  type spec_state = int Map_int.t

  type t = {
    spec: spec_state;
    cache: (key,value)cache_state;
    base_map: int Map_int.t;
  }

  let then_ f x = (if x=0 then f () else x)

  (* FIXME v inefficient *)
  let compare s1 s2 = 
    (Pervasives.compare 
       (s1.spec |> Map_int.bindings) (s2.spec |> Map_int.bindings)) |> then_
      (fun () -> 
         Cache.compare s1.cache s2.cache) |> then_
      (fun () -> 
         Map_int.compare Pervasives.compare s1.base_map s2.base_map)

end

open Test_state

let init_cache = Cache.mk_initial_cache ~compare_k:Tjr_btree.Base_types.Int_.compare |> Cache.normalize

let init_base_map = Map_int.empty

let init_spec = Map_int.empty

let initial_state = { spec=init_spec; cache=init_cache; base_map=init_base_map }


(* base uncached map ------------------------------------------------ *)

(* just return the value option *)
let base_find_opt k = fun t -> 
  Map_int.find_opt k t
  

let (find,insert,delete) = 
  Cache.make_cached_map () @@
  fun ~find ~insert ~delete -> (find,insert,delete)

(* we modify find so that it utilises the base map *)

let _ = find

let _ = insert

(* for all operations, we need to work out what to do with the
   evictees; here we simply flush to lower FIXME might be best to
   phrase this all in a monad *)

let merge_evictees_with_base_map (es:(key*value entry)list option) m =
  match es with
  | None -> m
  | Some es -> 
    (* Printf.printf "Merging evictees..."; *)
    Tjr_list.with_each_elt
      ~list:es
      ~step:(fun ~state (k,e) ->
          match e.entry_type with
          | Insert { value; dirty=true } -> Map_int.add k value state
          | Insert { value; dirty=false } -> state 
          | Delete { dirty=true } -> Map_int.remove k state
          | Delete { dirty=false } -> state
          | Lower vopt -> state)
      ~init:m

let find k t = 
  find k t.cache |> function
  | `In_cache e -> (
      match e.entry_type with
      | Insert {value; dirty } -> (Some value,t)
      | Delete _ -> (None,t)
      | Lower vopt -> (vopt,t))
  | `Not_in_cache kk -> 
    let vopt_from_lower = Map_int.find_opt k t.base_map in
    kk ~vopt_from_lower ~cache_state:t.cache |> fun (vopt,`Evictees es, `Cache_state cache) ->
    (* need to do something with evictees, and return the vopt and
       updated test state *)
    (vopt, { t with cache=cache; base_map=merge_evictees_with_base_map es t.base_map })


let insert,delete =
  let update t = fun (`Evictees es, `Cache_state cache) ->
    { t with cache; base_map=merge_evictees_with_base_map es t.base_map }
  in
  let insert k v t = insert k v t.cache |> update t in
  let delete k t = delete k t.cache |> update t in
  insert,delete

let _ : key -> t -> value option * t = find
let _ : key -> value -> t -> t = insert
let _ : key -> t -> t = delete


(* exhaustive testing ----------------------------------------------- *)

(* we use module Exhaustive *)

open Tjr_exhaustive_testing

let step t op =
  begin
    match op with
    | Find k -> find k t |> fun (_,t') -> t'
    | Insert (k,v) -> 
      insert k v t
      |> (fun t' -> {t' with spec=Map_int.add k v t'.spec})
    | Delete k ->
      delete k t
      |> (fun t'-> {t' with spec=Map_int.remove k t'.spec})
  end                   
  |> (fun x -> [{ x with cache=Cache.normalize x.cache}])

(* cache invariants:

   the model corresponds to the store plus the cache

   if an entry in the cache is not dirty, then the same entry exists in store

   the cache map:none then if dirty then k exists in store, otherwise
   doesn't exist in store

   the key involved in a step is at the head of hte queue (unless
   delete?); the queue is like the queue at the previous step, but k
   may have bene deleted; evicted elements may also have disappeared
   (?) this looks like we need to do some more work here

   FIXME these are not currently checked

   we already check internal invariants of course
*)

let check_state x = (wf x.cache) (* FIXME TODO *)
let check_step x op y = () (* FIXME TODO *)

let test_ops = { step; check_state; check_step }


(* sets of states --------------------------------------------------- *)

module Ord = struct 
  type t = Test_state.t
  let compare (x:t) (y:t) = Test_state.compare x y
end

module Set_ops = Tjr_set.Make(Ord)

let set_ops = Set_ops.set_ops


(* running exhaustive tests ---------------------------------------- *)

(* let range = BatList.(range 1 `To 5) *)


let test range =
  let ops =
    range 
    |> List.map (fun k -> [Find k;Insert(k,2*k); Delete k])
    |> List.concat
  in
  Printf.printf "%s: " __MODULE__;
  test ~set_ops ~test_ops ~ops ~init_states:[initial_state];
  print_string "\n\n";



(* old ============================================================ *)

(* let _ = main () *)

(* with range 1..5 takes a while:

todo: 14534; done: 1729999 ..........
todo: 11166; done: 1739999 .
*)


(* manual interactive testing ---------------------------------------- *)

(*

(* to test, we need to track the operations *)

let initial_cache = Cache_.initial_cache

open Cache_

(* insert 8 elts *)

let s0 = ref { cache=initial_cache; store=ST.initial_state }

let run = Sem.run_ref s0

let _ = 
  for x = 1 to 8 do
    run (Cache_.insert x x)
  done

(* have a look at the state *)
let _ = !s0 

let _ = Map.bindings (!s0).cache.map

let _ = run (Cache_.insert 9 9)


let _ = Map.bindings (!s0).cache.map
let _ = Queue.bindings (!s0).cache.queue
let _ = Map_int.bindings (!s0).store.map

let _ = run (Cache_.insert 3 3)

let _ = run (Cache_.insert 11 11)
let _ = run (Cache_.find 1|> bind (fun _ -> return ()))

let _ = run (Cache_.insert 12 12)


*)


