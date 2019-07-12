module W = struct type t = int let weight: t->int = fun _ -> 1 end

let cap = 100000

let _ = Printf.printf "%s, capacity is %d\n" __MODULE__ cap

let measure_execution_time_and_print s f = 
  Tjr_profile.measure_execution_time_and_print (CCString.pad 40 s) f

(** {2 Functional} *)

module Lru_impl = Lru.F.Make
    (struct type t = int let compare = Pervasives.compare end)
    (W)


let _ = 
  let module A = struct

    let lru = ref (Lru_impl.empty cap)

    let _ = 
      measure_execution_time_and_print "pqwy functional lru, 1e6 inserts" @@ fun () -> 
      for i = 1 to (int_of_float 1e6) do
        lru := (Lru_impl.add i (2*i) (!lru) |> Lru_impl.trim)
      done
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

    let lru = M.create cap

    let _ = 
      measure_execution_time_and_print "pqwy imperative lru, 1e6 inserts" @@ fun () -> 
      for i = 1 to (int_of_float 1e6) do
        M.add i (2*i) lru;
        M.trim lru
      done
  end
  in
  ()
    


(** {2 With hashtbl, just drop each time cap reached} *)

let _ = 
  let module A = struct
      
    (* type t = (int,int) Hashtbl.t * int *)

    let ht = Hashtbl.create cap
    let ht_count = ref 0

    module H = Hashtbl

    let _ = 
      measure_execution_time_and_print "hashtable, with drop, 1e6 inserts" @@ fun () -> 
      for i = 1 to (int_of_float 1e6) do
        H.add ht i (2*i);
        ht_count:=!ht_count +1;
        (if !ht_count > cap then
           H.clear ht; ht_count:=0)
      done
  end
  in
  ()

let _ = 
  let module A = struct
    let ht = ref @@ Hashtbl.create cap
    let ht_count = ref 0

    module H = Hashtbl

    let _ = 
      measure_execution_time_and_print "hashtable, with new, 1e6 inserts" @@ fun () -> 
      for i = 1 to (int_of_float 1e6) do
        H.add !ht i (2*i);
        ht_count:=!ht_count +1;
        (if !ht_count > cap then
           ht:=H.create cap; ht_count:=0)
      done
  end
  in
  ()




(** {2 tjr_lru timing} *)

let _ =
  if cap > 10000 then () else
  let module A = struct
    open Tjr_lru_cache
    open Im_intf

    let lru_ops = make_lru_in_mem ()
    let cache_map_ops=Tjr_map.make_map_ops Pervasives.compare

    (* FIXME cap and evict count effect on performance??? *)
    let cache_state = ref { max_size=cap; evict_count=cap/2; current_time=0; cache_map_ops; 
                            cache_map=cache_map_ops.empty;
                            queue=Queue.empty }

    let l = lru_ops

    let _ = 
      measure_execution_time_and_print "tjr_lru, 1e6 inserts" @@ fun () -> 
      for i = 1 to (int_of_float 1e6) do
        l.insert i (2*i) !cache_state |> fun (_,`Cache_state c) -> 
        cache_state:=c
      done
  end
  in
  ()
