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

(** NOTE {!Pvt_in_mem} module hidden *)

(**/**)

(** {2 In-memory version} *)
module Pvt_in_mem = struct

  (** NOTE: No multithreading *)

  module Im_intf = Im_intf
  (* open Im_intf *)

  (* module Lim_state = Im_intf.Lim_state *)

  (* module Lru_in_mem_ops = Im_intf.Lru_in_mem_ops *)

  module Lru_in_mem = Lru_in_mem
  (* open Lru_in_mem *)

  (* let make_lru_in_mem_ops,make_init_lim_state = Lru_in_mem.(make_lru_in_mem_ops,make_init_lim_state) *)



  module Lim_int_int = Lru_in_mem.Int_int
  (** Common instance *)
end
open Pvt_in_mem
open Pvt_in_mem.Lru_in_mem
(**/**)


(** {2 Multi-threaded version} *)

(** {3 Interfaces} *)

module Mt_intf = Mt_intf

(** {3 Persistence mode} *)

include Mt_intf.Persist_mode

(** {3 The resulting multithreaded operations} *)

include Mt_intf.Mt_ops


(**/**)

module Multithreaded_lru = Multithreaded_lru


(** {3 Construct the multithreaded LRU} *)


(** NOTE see also {!Lru_in_mem.Make_lru_fc}; FIXME should also return
   an initial state to avoid remembering how to to construct this later *)
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
(**/**)



(** {3 Construct the multithreaded LRU (functor version)} *)

module type S = sig
  type k
  val compare: k -> k -> int

  type v
    
  type t
  val monad_ops: t monad_ops
  val async: (unit -> (unit,t)m) -> (unit,t)m
  val event_ops: t Event.event_ops
end

(** NOTE Msg_type (part of external interface), Entry (part of msg) and Mt_state_type included to improve usability, but hidden in doc *)

(**/**)
type 'v entry = 'v Pvt_in_mem.Im_intf.entry
include Mt_intf.Msg_type
include Mt_intf.Mt_state_type
(**/**)

module type T = sig
  type k
  type v
  type lru
  type t
  type t_map
          
  (* open Im_intf *)
  (* open Mt_intf.Mt_state_type *)
  (* open Mt_intf.Threading_types *)

  (** messages are flushed to the lower level, see make function below *)
  type nonrec msg = (k,v,t)msg

  type nonrec mt_state 
    = (k, v, lru,t_map, t) mt_state
(* FIXME why can't we expand this alias?    = {  lim_state: (k,v,lru) lim_state; 
      blocked_threads: t_map;
      blocked_threads_ops:(k,(v,t)blocked_thread list,t_map)Tjr_map.map_ops
    } *)

  val init_state :
    max_size:int ->
    evict_count:int ->
    mt_state

  type with_lru = (mt_state,t)with_lru_ops

  val make_multithreaded_lru :
    with_lru:with_lru ->
    to_lower:(msg -> (unit, t) Tjr_monad.m) ->
    (k, v, t) mt_ops
end

(** NOTE we expect the result of this Make to be bound to a module "Lru" or "Lru_" *)
module Make(S:S) : T with type k = S.k and type v = S.v and type t = S.t = struct
  include S
  type msg = (k,v,t) Mt_intf.Msg_type.msg
  module Internal = struct type k = S.k type v = S.v let compare = S.compare end
  module Lru_fc = Lru_in_mem.Make_lru_fc(Internal)
  type lru = Lru_fc.lru
  let lru_fc = Lru_fc.lru_fc    

  type t_map (* FIXME should be fixed by the initial state *)
    = (k, (v, t) Mt_intf.Threading_types.blocked_thread list, unit) Tjr_map.map

  type mt_state = (k, v, lru,t_map, t) Mt_intf.Mt_state_type.mt_state

  let init_state ~max_size ~evict_count = 
    let initial_lim_state = Lru_in_mem.make_init_lim_state ~max_size ~evict_count ~lru_fc in
    let compare_k = S.compare in
    Mt_intf.Mt_state_type.mt_initial_state ~initial_lim_state ~compare_k
     
  type with_lru = (mt_state,t)Mt_intf.Mt_state_type.with_lru_ops

  let make_multithreaded_lru ~with_lru ~to_lower = 
    make_multithreaded_lru ~lru_fc ~monad_ops ~async ~event_ops ~with_lru ~to_lower 
end
