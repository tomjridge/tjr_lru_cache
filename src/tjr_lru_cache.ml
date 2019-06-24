(** A multithreaded LRU cache (part of ImpFS). *)

(** {2 In-memory version} *)

(** NOTE: No multithreading *)

module Im_intf = Im_intf

include Im_intf.Lru_in_mem_ops

include Lru_in_mem


(** {2 Multi-threaded version} *)

(** {3 Interfaces} *)

module Mt_intf = Mt_intf

(** {3 Persistence mode} *)

include Mt_intf.Persist_mode

(** {3 The resulting multithreaded operations} *)

include Mt_intf.Mt_ops

(** {3 Construct the multithreaded LRU} *)

include Multithreaded_lru
