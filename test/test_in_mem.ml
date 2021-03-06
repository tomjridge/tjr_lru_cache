(* FIXME resurrect this

(** Tests for the in-mem cache  *)

open Lru_in_mem

type key = int
type value = int

module Op = struct
  type op = Find of key | Insert of key * value | Delete of key
end

let compare_ints : int -> int -> int = Stdlib.compare

(*
let k_cmp = compare_ints

open Tjr_map
let make_map_ops () = Tjr_map.make_map_ops k_cmp
*)

let ops,initial_state = Lru_in_mem.Examples.Int_int.(ops,initial_state)

module Test_state = struct 
  (* the spec state is the combined view of the cache and the base map ? *)
  type spec_state = int Map_int.t


  (* type kve_map = (key,value entry,unit)map *)
  (* let k_ve_map_ops : (key,value entry,kve_map)map_ops = make_map_ops () *)

  type t = {
    spec: spec_state;
    cache: (key,value,Int_int.lru)lim_state;
    base_map: int Map_int.t;
  }

  let then_ f x = (if x=0 then f () else x)

  (* FIXME v inefficient *)
  let compare s1 s2 = 
    (Stdlib.compare 
       (s1.spec |> Map_int.bindings) (s2.spec |> Map_int.bindings)) |> then_
      (fun () -> 
        Stdlib.compare 
          (s1.cache.max_size,s1.cache.evict_count)
          (s2.cache.max_size,s2.cache.evict_count)) |> then_          
      (fun () -> 
        s1.cache.compare_lru s1.cache.lru_state s2.cache.lru_state) |> then_
      (fun () -> 
         Map_int.compare compare_ints s1.base_map s2.base_map)

end

open Test_state
    
let init_cache = initial_state ~max_size:4 ~evict_count:2 

let _ : (value, value, Int_int.lru) lim_state = init_cache

let init_base_map = Map_int.empty

let init_spec = Map_int.empty

let initial_state = { spec=init_spec; cache=init_cache; base_map=init_base_map }


(* base uncached map ------------------------------------------------ *)

(* just return the value option *)
let base_find_opt k = fun t -> 
  Map_int.find_opt k t
  

let Im_intf.{find; insert; delete; _ } = ops


(* we modify find so that it utilises the base map *)

let _ = find

let _ = insert

(* for all operations, we need to work out what to do with the
   evictees; here we simply flush to lower FIXME might be best to
   phrase this all in a monad *)

let merge_evictees_with_base_map (es:(key*value entry)list option) (m:int Map_int.t) =
  match es with
  | None -> m
  | Some es -> 
    (* Printf.printf "Merging evictees..."; *)
    (es,m) |> 
    iter_break (function
        | ([],m) -> Break m
        | (k,e)::es,m -> 
          match e with
          | Insert { value; dirty=true } -> Cont (es,Map_int.add k value m)
          | Insert { value; dirty=false } -> Cont (es,m) 
          | Delete { dirty=true } -> Cont(es,Map_int.remove k m)
          | Delete { dirty=false } -> Cont(es,m)
          | Lower vopt -> Cont(es,m))



let find k t = 
  find k t.cache |> function
  | In_cache e -> (
      match e with
      | Insert {value; dirty } -> (Some value,t)
      | Delete _ -> (None,t)
      | Lower vopt -> (vopt,t))
  | Not_in_cache kk -> 
    let vopt_from_lower = Map_int.find_opt k t.base_map in
    kk ~vopt_from_lower ~lim_state:t.cache |> fun (vopt,exc) ->
    (* need to do something with evictees, and return the vopt and
       updated test state *)
    (vopt, { t with cache=exc.lim_state; 
                    base_map=merge_evictees_with_base_map exc.evictees t.base_map })


let insert,delete =
  let update t = fun exc ->
    { t with cache=exc.lim_state; 
             base_map=merge_evictees_with_base_map exc.evictees t.base_map }
  in
  let insert k v t = insert k v t.cache |> update t in
  let delete k t = delete k t.cache |> update t in
  insert,delete

let _ : key -> t -> value option * t = find
let _ : key -> value -> t -> t = insert
let _ : key -> t -> t = delete


(* exhaustive testing ----------------------------------------------- *)

(* we use module Exhaustive *)

open Exhaustive_testing

let step t op =
  let open Op in
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
  |> fun x -> [x]
  (* |> (fun x -> [{ x with cache=Im_cache_state.normalize x.cache}]) *)


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

let check_state x = () (* assert_(Im_cache_state.wf x.cache) (* FIXME TODO *) *)
let check_step x op y = () (* FIXME TODO *)

let cmp = Test_state.compare
(* let test_ops = { step; check_state; check_step } *)


(* running exhaustive tests ---------------------------------------- *)

let test range =
  let ops =
    range 
    |> List.map (fun k -> Op.[Find k;Insert(k,2*k); Delete k])
    |> List.concat
  in
  Printf.printf "%s: " __MODULE__;
  test ~cmp ~step ~check_state ~check_step ~ops ~init_states:[initial_state];
  print_string "\n\n";


*)
