(** A multithreaded LRU cache (part of ImpFS; safe to open) *)

(* NOTE tjr_lib has a simpler version of LRU, not multithreaded; we
   need a bit more here *)

include Summary


(** NOTE hidden doc for private modules *)

(**/**)

(** Private modules *)
module Pvt = struct

  (** {2 Single-threaded version} *)

  module Im_intf = Im_intf

  module Lru_in_mem = Lru_in_mem

  module Test_performance = Test_performance
end
(**/**)
module Mt_intf = Mt_intf

type persist_mode = Mt_intf.persist_mode = Persist_later | Persist_now

module Lru_msg = Mt_intf.Lru_msg

module Lru_factory = Mt_intf.Lru_factory

module Mt_ops = Mt_intf.Mt_ops

include Mt_ops (* prefix mt so no clashes *)

module Multithreaded_lru = Multithreaded_lru

module Lru_examples = Multithreaded_lru.Examples

