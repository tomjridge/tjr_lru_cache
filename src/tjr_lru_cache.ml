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

      
module type T = sig
  type k
  type v
  type lru
  type t
  type t_map
  type msg = (k,v,t) Mt_intf.Msg_type.msg
          
  (* FIXME 'c should be fixed as t *)
  type mt_state = (k, v, lru,t_map, t) Mt_intf.Mt_state_type.mt_state

  val make_multithreaded_lru :
    ( (max_size:int -> evict_count:int -> mt_state), 
    with_lru:(mt_state, t) Mt_intf.Mt_state_type.with_lru_ops ->
    to_lower:(msg -> (unit, t) m) ->
    (k, v, t) mt_ops)initial_state_and_ops
end

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
     
  let make_multithreaded_lru = 
    let initial_state ~max_size ~evict_count = 
      let initial_lim_state = Lru_in_mem.make_init_lim_state ~max_size ~evict_count ~lru_fc in
      let compare_k = S.compare in
      Mt_intf.Mt_state_type.mt_initial_state ~initial_lim_state ~compare_k
    in
    let ops ~with_lru ~to_lower = make_multithreaded_lru ~lru_fc ~monad_ops ~async ~event_ops ~with_lru ~to_lower in
    { initial_state; ops }
end
