(* open Lru *)
open Tjr_profile_with_core

let prof = make_string_profiler ()

module W = struct type t = int let weight: t->int = fun _ -> 1 end

(** {2 Functional} *)

module Lru_impl = Lru.F.Make
    (struct type t = int let compare = Pervasives.compare end)
    (W)


let _ = 
  let module A = struct
    let cap = 10

    let lru = ref (Lru_impl.empty cap)

    let _ = 
      prof.mark "func";
      for i = 1 to (int_of_float 1e6) do
        lru := (Lru_impl.add i (2*i) (!lru))
      done;
      prof.mark "func'";
  end
  in
  ()


(** {2 Imperative} *)


module Htype = struct
  type t = int
  let equal x y = (x:int)=y
  let hash x = Hashtbl.hash x
end

module M_impl = Lru.M.Make(Htype)(W)

let _ = 
  let module A = struct
    module M = M_impl
    let cap = 10

    let lru = M.create cap

    let _ = 
      prof.mark "imp";
      for i = 1 to (int_of_float 1e6) do
        M.add i (2*i) lru
      done;
      prof.mark "imp'";
  end
  in
  ()
    


(** {2 With hashtbl, just drop each time cap reached} *)

let _ = 
  let module A = struct
    let cap = 10
      
    (* type t = (int,int) Hashtbl.t * int *)

    let ht = Hashtbl.create cap
    let ht_count = ref 0

    module H = Hashtbl

    let _ = 
      prof.mark "ht";
      for i = 1 to (int_of_float 1e6) do
        H.add ht i (2*i);
        ht_count:=!ht_count +1;
        (if !ht_count > cap then
           H.clear ht; ht_count:=0)
      done;
      prof.mark "ht'";
    
  end
  in
  ()



(** {2 tjr_lru timing} *)

let _ =
  let module A = struct
    open Tjr_lru_cache
    open Im_intf

    let cap = 10
      
    let lru_ops = make_lru_in_mem ()
    let cache_map_ops=Tjr_map.make_map_ops Pervasives.compare
    let cache_state = ref { max_size=cap; evict_count=1; current_time=0; cache_map_ops; 
                            cache_map=cache_map_ops.empty;
                            queue=Queue.empty }

    let l = lru_ops

    let _ = 
      prof.mark "tjr";
      for i = 1 to (int_of_float 1e6) do
        l.insert i (2*i) !cache_state |> fun (_,`Cache_state c) -> 
        cache_state:=c
      done;
      prof.mark "tjr'";
    
  end
  in
  ()
  



let _ = prof.print_summary();

