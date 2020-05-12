(** Construct the multithreaded lru; like [Make_1], but targetting the lru
   factory *)

open Mt_intf.Lru_factory

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
  type t
  type lru

  val lru_factory : max_size:int -> evict_count:int -> (k, v, lru, t) lru_factory          
end

module Make(S:S) : T with type k = S.k and type v = S.v and type t = S.t = struct
  include S
  module M1 = Make_1.Make(S)
  open M1

  type lru = mt_state

  let lru_factory ~max_size ~evict_count : (_,_,_,_) Mt_intf.Lru_factory.lru_factory = 
    object (self)
      method max_size=max_size
      method evict_count=evict_count
      method empty : lru = init_state ~max_size ~evict_count
      method make = fun ~with_lru ~to_lower -> 
        make_multithreaded_lru ~with_lru ~to_lower
      method make_with_ref = fun ~to_lower -> 
        let lru_ref = ref self#empty in
        let with_lru = Tjr_monad.with_imperative_ref ~monad_ops lru_ref in
        let ops = self#make ~with_lru ~to_lower in
        object
          method ops = ops
          method lru_ref = lru_ref
          method with_lru = with_lru
        end      
    end

  let _ : max_size:int -> evict_count:int -> (k, v, lru, t) lru_factory = lru_factory  

end

