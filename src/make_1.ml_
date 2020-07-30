(** Construct the multithreaded lru (functor version) *)

open Mt_intf
(* open Im_intf.Entry_type *)
open Mt_intf.Lru_msg_type
open Mt_intf.Mt_state_type


(** Inputs: k,v,t, async and event_ops *)
module type S = sig
  type k
  val compare: k -> k -> int

  type v
    
  type t
  val monad_ops: t monad_ops
  val async: (unit -> (unit,t)m) -> (unit,t)m
  val event_ops: t event_ops
end


module type T = sig
  type k
  type v
  type lru'
  type t
  type t_map
          
  (** messages are flushed to the lower level, see make function below *)
  type nonrec lru_msg = (k,v,t)lru_msg

  type nonrec mt_state 
    = (k, v, lru',t_map, t) mt_state

  val init_state :
    max_size:int ->
    evict_count:int ->
    mt_state

  val make_multithreaded_lru :
    with_lru:(mt_state,t)with_state ->
    to_lower:(lru_msg -> (unit, t) Tjr_monad.m) ->
    (k, v, t) mt_ops
end

(** NOTE we expect the result of this Make to be bound to a module "Lru" or "Lru_" *)
module Make(S:S) : T with type k = S.k and type v = S.v and type t = S.t = struct
  include S
  type lru_msg = (k,v,t) Mt_intf.Lru_msg_type.lru_msg
  module Internal = struct type k = S.k type v = S.v let compare = S.compare end
  module Lru_fc = Lru_in_mem.Make_lru_fc(Internal)
  type lru' = Lru_fc.lru
  let lru_fc = Lru_fc.lru_fc    

  type t_map (* FIXME should be fixed by the initial state *)
    = (k, (v, t) Mt_intf.Threading_types.blocked_thread list, unit) Tjr_map.map

  type mt_state = (k, v, lru',t_map, t) Mt_intf.Mt_state_type.mt_state

  let init_state ~max_size ~evict_count = 
    let initial_lim_state = Lru_in_mem.make_init_lim_state ~max_size ~evict_count ~lru_fc in
    let compare_k = S.compare in
    Mt_intf.Mt_state_type.mt_initial_state ~initial_lim_state ~compare_k
     
  let make_multithreaded_lru ~with_lru ~to_lower = 
    Multithreaded_lru.make_multithreaded_lru ~lru_fc ~monad_ops ~async ~event_ops ~with_lru ~to_lower 

  

end



