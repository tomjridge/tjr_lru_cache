(** Performance testing code. 

Three main parameters:

- count, the number of add operations to execute
- capacity, the cache capacity
- evict, the number of entries to evict when capacity is reached

*)

module Make() = struct

  open Minicli

  (** {2 Command line argument processing} *)

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

      let _ = Printf.printf "count is %#d (%s)\n" count __FILE__
      let _ = Printf.printf "capacity is %#d (%s)\n" cap __FILE__
      let _ = Printf.printf "evict count is %#d (%s)\n" evict_count __FILE__
      let _ = CLI.finalize ()
    end
    in
    A.(count,cap,evict_count)


  module W = struct type t = int let weight: t->int = fun _ -> 1 end

  let measure_execution_time_and_print s f = 
    Tjr_profile.measure_execution_time_and_print (CCString.pad 50 s) f


  let _ = Printf.printf "Following timings are in nanoseconds\n%!"

  (** {2 Functional LRU implementation from pqwy} *)

  module Lru_impl = Lru.F.Make
      (struct type t = int let compare = Stdlib.compare end)
      (W)


  let _ = 
    let module A = struct

      let lru = ref (Lru_impl.empty cap)

      let _ = 
        measure_execution_time_and_print "pqwy functional lru, op=add-and-trim" @@ fun () -> 
        for i = 1 to count do
          lru := (Lru_impl.add i (2*i) (!lru) |> Lru_impl.trim)
        done
    end
    in
    ()


  (** {2 Imperative LRU implementation from pqwy} *)


  module Htype = struct
    type t = int
    let equal x y = (x:int)=y
    let hash x = Hashtbl.hash x
  end

  module M_impl = Lru.M.Make(Htype)(W)

  let _ = 
    let module A = struct
      module M = M_impl

      (* FIXME the following creates a cache with full capacity rather
         than empty, which is what the other tests do *)
      let lru = M.create cap

      let _ = 
        measure_execution_time_and_print "pqwy imperative lru, op=add-and-trim" @@ fun () -> 
        for i = 1 to count do
          M.add i (2*i) lru;
          M.trim lru
        done
    end
    in
    ()


  (** {2 Imperative (pqwy) with bulk eviction on exceeding cap} *)


  module M_impl' = Lru.M.Make(Htype)(W)

  let _ = 
    let module A = struct
      module M = M_impl'

      let lru = M.create cap

      let _ = 
        measure_execution_time_and_print "pqwy imperative lru with batch evict if over cap" @@ fun () -> 
        for i = 1 to count do
          M.add i (2*i) lru;
          (match M.size lru > cap with
           | true -> (for _i = 1 to evict_count do M.drop_lru lru done)
           | false -> ())
        done
    end
    in
    ()




  (** {2 With hashtbl, just dropping entire cache each time cap reached} *)

  let _ = 
    let module A = struct

      (* type t = (int,int) Hashtbl.t * int *)

      let ht = Hashtbl.create cap
      let ht_count = ref 0

      module H = Hashtbl

      let _ = 
        measure_execution_time_and_print "hashtable, with drop when cap exceeded" @@ fun () -> 
        for i = 1 to count do
          H.add ht i (2*i);
          incr ht_count;
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
        measure_execution_time_and_print "hashtable, with new when exceeding cap" @@ fun () -> 
        for i = 1 to count do
          H.add !ht i (2*i);
          incr ht_count;
          (if !ht_count > cap then
             ht:=H.create cap; ht_count:=0)
        done
    end
    in
    ()




  (** {2 tjr_lru timing} *)

  (** NOTE this test only runs if cap <= 10k *)

  let _ =
    if cap > 10_000 then () else
      let module A = struct
        (* open Tjr_lru_cache *)
        open Im_intf

        let lru_ops = Lru_in_mem.Int_int.ops
        (* let cache_map_ops=Tjr_map.make_map_ops Stdlib.compare *)

        (* FIXME cap and evict count effect on performance??? *)
        let cache_state = ref @@ Lru_in_mem.Int_int.initial_state ~max_size:cap ~evict_count

        let l = lru_ops

        let _ = 
          measure_execution_time_and_print "tjr_lru with automatic evict if over cap" @@ fun () -> 
          for i = 1 to count do
            l.insert i (2*i) !cache_state |> fun exc -> 
            cache_state:=exc.lim_state
          done
      end
      in
      ()

end
