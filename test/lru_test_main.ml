(* open Tjr_lru_cache *)

let (--) = List_.from_upto

let main () = 
  match Array.to_list Sys.argv |> List.tl with
  | [l;h] -> 
      let [l;h] = List.map int_of_string [l;h] in
      ()
      (* Test_in_mem.test (l--h)  *)
  | _ -> let module X = Test_performance.Make() in ()
[@@warning "-8"]

let _ = main ()
