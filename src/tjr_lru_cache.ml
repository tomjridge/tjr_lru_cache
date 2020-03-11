(** A multithreaded LRU cache (part of ImpFS; don't open)

NOTE probably best not to open this module, because msg_type and entry
have constructors like Insert etc that clash with others defined
elsewhere

 *)

(** 

Abbreviations: 
  - Im = In Memory
  - Lim = LRU In Mem
  - mt = MultiThreaded
  - fc = First-Class (module!); 
*)


(** {2 Single-threaded version} *)

module Im_intf = Im_intf

module Lru_in_mem = Lru_in_mem
(* open Lru_in_mem *)


(** {2 Multi-threaded version} *)

(** {3 Interfaces} *)

module Mt_intf = Mt_intf

(** {3 Persistence mode} *)

(** NOTE hidden doc for include Persist_mode, now or later FIXME change to `Now or `Later *)

(**/**)
include Mt_intf.Persist_mode
(**/**)


(** {3 The resulting multithreaded operations} *)

include Mt_intf.Mt_ops


(** {3 Non-functorial mt impl} *)

module Multithreaded_lru = Multithreaded_lru


(** {3 Construct the multithreaded LRU (functor version)} *)

module Make = Make

(** {2 Performance testing} *)

module Test_performance = Test_performance
