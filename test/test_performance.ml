open Minicli

let count,cap,evict_count = 
  let module A = struct
    let argc, args = CLI.init ()                        
                       
    let _ = 
      if argc = 1 then (
        Printf.printf {|usage: %s -count <ops> -cap <capacity> [-evict <count>]|} Sys.argv.(0);
        exit 1
      )

    let int_of_string s = 
      float_of_string_opt s |> function
      | None -> int_of_string s
      | Some f -> int_of_float f

    (** Command line expects 1 argument, the capacity of the LRU *)
    let cap = CLI.get_string ["-cap"] args |> int_of_string 
    let evict_count = 
      CLI.get_string_opt ["-evict"] args
      |> function None -> 1
                | Some s -> int_of_string s

    let count = CLI.get_string ["-count"] args |> int_of_string

    let _ = Printf.printf "%s, count is %d\n" __MODULE__ count
    let _ = Printf.printf "%s, capacity is %d\n" __MODULE__ cap
    let _ = Printf.printf "%s, evict count is %d\n" __MODULE__ evict_count
    let _ = CLI.finalize ()
  end
  in
  A.(count,cap,evict_count)


module W = struct type t = int let weight: t->int = fun _ -> 1 end

let measure_execution_time_and_print s f = 
  Tjr_profile.measure_execution_time_and_print (CCString.pad 40 s) f


(** {2 Functional} *)

module Lru_impl = Lru.F.Make
    (struct type t = int let compare = Stdlib.compare end)
    (W)


let _ = 
  let module A = struct

    let lru = ref (Lru_impl.empty cap)

    let _ = 
      measure_execution_time_and_print "pqwy functional lru" @@ fun () -> 
      for i = 1 to count do
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
      measure_execution_time_and_print "pqwy imperative lru" @@ fun () -> 
      for i = 1 to count do
        M.add i (2*i) lru;
        M.trim lru
      done
  end
  in
  ()


(** {2 Imperative with bulk eviction} *)


module M_impl' = Lru.M.Make(Htype)(W)

let _ = 
  let module A = struct
    module M = M_impl'

    let lru = M.create cap

    let _ = 
      measure_execution_time_and_print "pqwy imperative lru with batch evict" @@ fun () -> 
      for i = 1 to count do
        M.add i (2*i) lru;
        (match M.size lru > cap with
           | true -> (for _i = 1 to evict_count do M.drop_lru lru done)
           | false -> ())
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
      measure_execution_time_and_print "hashtable, with drop" @@ fun () -> 
      for i = 1 to count do
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
      measure_execution_time_and_print "hashtable, with new" @@ fun () -> 
      for i = 1 to count do
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

    let lru_ops = Lru_in_mem.Int_int.ops
    (* let cache_map_ops=Tjr_map.make_map_ops Stdlib.compare *)

    (* FIXME cap and evict count effect on performance??? *)
    let cache_state = ref @@ Lru_in_mem.Int_int.initial_state ~max_size:cap ~evict_count

    let l = lru_ops

    let _ = 
      measure_execution_time_and_print "tjr_lru" @@ fun () -> 
      for i = 1 to count do
        l.insert i (2*i) !cache_state |> fun exc -> 
        cache_state:=exc.lim_state
      done
  end
  in
  ()
