(** Construct the multithreaded LRU; like [Make_2], but specialized to Lwt *)


open Mt_intf.Lru_factory

(** Inputs: k,v,t, async and event_ops *)
module type S = sig
  type k
  val k_cmp: k -> k -> int

  type v
    
  type t = lwt
end


module type T = sig
  type k
  type v
  type t = lwt
  type lru

  val lru_factory : max_size:int -> evict_count:int -> (k, v, lru, t) lru_factory          
end

module Make(S:S) : T with type k = S.k and type v = S.v and type t = S.t = struct
  include S
  module S1 = struct
    include S
    let monad_ops = Std_types.monad_ops
    let async = Std_types.async
    let event_ops = Std_types.event_ops
    let compare = k_cmp
  end
  module M2 = Make_2.Make(S1)
  (* open M2 *)

  type lru = M2.lru

  let lru_factory ~max_size ~evict_count = 
    M2.lru_factory ~max_size ~evict_count

end


