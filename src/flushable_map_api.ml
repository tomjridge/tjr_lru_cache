(** The LRU provides a flushable map API. To support blocking/non-blocking modes, we can optionally provide a callback function. *)

open Tjr_monad.Monad

module Types = struct
  (** A calling mode can be "later" (ie, perform immediately in mem
     and return; persist sometime later) or now, in which case we can
     optionally supply a callback. A call can be blocking or
     non-blocking. For non-blocking, the calling thread is expected to
     launch an async promise that resolves without blocking the
     caller. *)

  type persist_mode = Persist_later | Persist_now

  type mode = persist_mode

  (* FIXME this interface doesn't allow "transaction" operations
     (multiple ops, which commit atomically). This is sufficient for
     ImpFS - the kv store is pointwise syncable not
     transactional. However, since the lower level does support
     transactional operations, it seems strange to limit the
     functionality here. *)
  type ('k,'v,'t) flushable_map_ops = {
    find: 'k -> ('v option,'t) m;
    insert: mode -> 'k -> 'v -> (unit,'t) m;
    delete: mode -> 'k -> (unit,'t) m;
    sync_key: 'k -> (unit,'t) m;
    sync_all_keys: unit -> (unit,'t) m;
  }
end

include Types

(** We also need to put operations on a queue, to be processed by the pcache. *)
module Concrete_representation = struct

  (* FIXME do we need to distinguish between raw and with_callback?
     can't we just provide a dummy callback which does nothing?
     Probably yes. *)

  (* NOTE in the following, when the lower impl is sure the data is on
     disk we call the continuation *)

  (* find is always called in blocking mode, so there needs to be some way to return to the caller *)
  type ('k,'v,'t) find_raw = 
    | Find : 'k * ('v option -> ('a,'t) m) -> ('k,'v,'t) find_raw
  and ('k,'v) non_find_raw' =
    | Insert of 'k * 'v 
    | Delete of 'k 
    | Sync_key of 'k 
    | Sync_all_keys 
  and ('k,'v,'t) non_find_raw = ('k,'v) non_find_raw' * (unit -> (unit,'t) m)
  and ('k,'v,'a,'t) flushable_map_ops' =
    | Find_raw of ('k,'v,'t) find_raw 
    | Non_find_raw of ('k,'v,'t) non_find_raw
    | Batch of ('k,'v) non_find_raw' list * (unit -> (unit,'t) m)

  (* NOTE a problem with this approach is that the 'a is different for
     every caller; this is why we use GADTs to eliminate the 'a tyvar
     *)
end
