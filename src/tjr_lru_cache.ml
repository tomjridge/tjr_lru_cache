(** A multithreaded LRU cache (part of ImpFS). *)

(** 

- Abbreviations: Im = In Memory; Lim = LRU In Mem; mt = MultiThreaded; fc = First-Class (module!); 

*)



(** {2 In-memory version} *)

(** NOTE: No multithreading *)

module Im_intf = Im_intf

module Lim_state = Im_intf.Lim_state

module Lru_in_mem_ops = Im_intf.Lru_in_mem_ops

module Lru_in_mem = Lru_in_mem
open Lru_in_mem

let make_lru_in_mem_ops,make_init_lim_state = Lru_in_mem.(make_lru_in_mem_ops,make_init_lim_state)



(** Common instances *)

module Lim_int_int = Lru_in_mem.Int_int


(** {2 Multi-threaded version} *)

(** {3 Interfaces} *)

module Mt_intf = Mt_intf

(** {3 Persistence mode} *)

include Mt_intf.Persist_mode

(** {3 The resulting multithreaded operations} *)

include Mt_intf.Mt_ops

module Multithreaded_lru = Multithreaded_lru

(** {3 Construct the multithreaded LRU} *)

(** NOTE see also {!Lru_in_mem.Make_lru_fc} *)
let make_multithreaded_lru
  ~(lru_fc:('k,'v,'lru)lru_fc)
  ~(monad_ops:'t monad_ops)
  ~async
  ~event_ops
  ~with_lru
  ~to_lower
  =
  let open Multithreaded_lru in
  let lim_ops = make_lru_in_mem_ops lru_fc in
  let callback_ops = make_lru_callback_ops
    ~lim_ops
    ~monad_ops
    ~async
    ~with_lru
    ~to_lower
  in
  make_lru_ops ~monad_ops ~event_ops ~callback_ops
