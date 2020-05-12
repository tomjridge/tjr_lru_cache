(** A multithreaded LRU cache (part of ImpFS; safe to open)

NOTE Mt_ops may have field names that clash with those elsewhere

 *)

(* NOTE tjr_lib has a simpler version of LRU, not multithreaded; we
   need a bit more here *)

(** 

Abbreviations: 
  - Im = In Memory
  - Lim = LRU In Mem
  - mt = MultiThreaded
  - fc = First-Class (module!); 
*)


(** NOTE hidden doc for private modules *)

(**/**)

(** Private modules *)
module Pvt = struct

  (** {2 Single-threaded version} *)

  module Im_intf = Im_intf

  module Lru_in_mem = Lru_in_mem
  (* open Lru_in_mem *)

  module Mt_intf = Mt_intf

  module Multithreaded_lru = Multithreaded_lru

  module Test_performance = Test_performance
end
(**/**)


include Im_intf.Entry_type

type persist_mode = Mt_intf.persist_mode = Persist_later | Persist_now

include Mt_intf.Mt_ops

include Mt_intf.Lru_msg_type

include Mt_intf.Lru_factory


module Make_1 = Make_1

module Make_2 = Make_2

module Make_3 = Make_3

module Make = Make_3


