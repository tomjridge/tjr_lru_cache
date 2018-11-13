(** The type of messages that we send to the lower level *)

open Tjr_monad.Types
open Entry

type ('k,'v,'t) msg = 
  Insert of 'k*'v*(unit -> (unit,'t)m)
  | Delete of 'k*(unit -> (unit,'t)m)
  | Find of 'k * ('v option -> (unit,'t)m)
  | Evictees of ('k * 'v entry) list
