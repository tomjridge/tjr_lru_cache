open Tjr_lru_cache

let from_to l h : int list = Tjr_list.from_to l h

let (--) = from_to

(*
let _ = 
  (* FIXME add printing as an exit hook *)
  Pervasives.at_exit (
    fun () -> 
      print_endline (__LOC__ ^ ": running exit hooks");
      Test.run_exit_hooks ());
*)

let main () = 
  match Array.to_list Sys.argv |> List.tl with
  | [l;h] -> 
      let [l;h] = List.map int_of_string [l;h] in
      Test_cache.test (l--h) 
  | _ -> failwith __LOC__
[@@warning "-8"]

let _ = main ()
