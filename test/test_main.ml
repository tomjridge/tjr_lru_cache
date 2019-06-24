(* open Tjr_lru_cache *)

let from_to l h : int list = List_.from_to l h

let (--) = from_to

let main () = 
  match Array.to_list Sys.argv |> List.tl with
  | [l;h] -> 
      let [l;h] = List.map int_of_string [l;h] in
      Test_in_mem.test (l--h) 
  | _ -> failwith __LOC__
[@@warning "-8"]

let _ = main ()
