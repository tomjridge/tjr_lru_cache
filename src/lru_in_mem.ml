(** A single-threaded in-memory LRU cache, as a prelude to the real
   thing.

We assume all operations complete quickly from the cache. Operations
   that need to go to disk are modelled using explicit continuation
   passing.

We allow values to be tagged with extra info (this will be used to
   record whether the value needs to be flushed to the lower
   level). ?FIXME

The difficulty is how to extend this to deal with disk-backed
   eviction.

NOTE that the operations occur not in a monad - instead, explicit
   state passing is used, and we return the possible evictees; this is
   a prelude to the real version where we need to do something with
   the evictees

*)

open Im_intf


type ('k,'v,'lru) lru_fc = (module Lru.F.S with type k='k and type v='v entry and type t='lru)

module Make_lru_fc(S:sig
    type k 
    type v
    val compare: k -> k -> int
  end) 
  : (sig 
    type lru 
    val lru_fc:(S.k,S.v,lru)lru_fc 
  end)
= struct
  open S
  module W = struct type t = v entry let weight: t->int = fun _ -> 1 end    

  module Lru' = Lru.F.Make(struct type t = k let compare: k -> k -> int = compare end)(W)

  type lru = Lru'.t

  let lru_fc = (module Lru' : Lru.F.S with type k=k and type v=v entry and type t=lru) 
end

module Internal(S:sig
    type k
    type v
    type lru
    val lru_fc : (k,v,lru)lru_fc
  end) = struct

  open S

  (* NOTE evictees are removed from the map, so no need to mark clean *)

  (* module W = struct type t = v entry let weight: t->int = fun _ -> 1 end *)

  module L = (val lru_fc)

  type lru = L.t

  (* type cs = (k,v,lru) lim_state *)
  
  (** Attempt to locate key in cache. FIXME inline *)
  let find_in_cache (k:k) (c:(k,v,lru)lim_state) = 
    L.find k c.lru_state |> function
    | None -> None,c
    | Some e -> (Some e,c)


  let _ = find_in_cache



  (** An exception, for quick abort. FIXME remove *)
  exception E_


  (** Construct the cached map, using existing map find operation.

      NOTE the idea for [find] is that we execute a quick step to handle the
      case that there is an entry in the cache. If there isn't, we make a
      slow call to the lower layer, and when the result arrives we make
      another update to the cache state with the then current state of
      the cache. To avoid the risk of stale results being returned from
      lower, we have to tag lower results with some kind of
      monotonically-increasing index.
  *)
  let make_cached_map () =

    (* Returns None if no evictees need to be flushed, or Some(evictees)
        otherwise *)
    let get_evictees (c:(k,v,lru)lim_state) : (k, v, lru) evictees_x_lim_state = 
      (* mark "ab"; *)
      let card = L.size c.lru_state in
      (* mark "bc"; *)
      match card > c.max_size with (* FIXME inefficient *)
      | false -> {evictees=None; lim_state=c}
      | true -> 
        (* how many to evict? *)
        let n = c.evict_count in
        (* for non-dirty, we just remove from map; for dirty we
           must flush to lower *)        
        let evictees,lru_state = 
          ([],c.lru_state,n) |> iter_break (fun (es,s,n) -> 
              match n <= 0 with
              | true -> Break (es,s)
              | false -> 
                L.pop_lru s |> function
                | None -> Break (es,s)
                | Some ((k,e),s) -> Cont((k,e)::es,s,(n-1)))
        in
        (* now we have evictees, new queue, and new map *)
        let c = {c with lru_state} in
        (* mark "cd"; *)
        {evictees=(Some evictees);lim_state=c}
    in


    (* NOTE that if find pulls an entry into the cache, we may have to
         get rid of evictees; so a read causes a write; but this should be
         relatively infrequent *)
    let find (k:k) c = 
      find_in_cache k c |> fun (v,c) -> 
      match v with
      | None -> 
        Not_in_cache (fun ~vopt_from_lower ~lim_state:(c:(k,v,lru)lim_state) ->
          (* need to update map before returning *)
          (* FIXME concurrency concerns: v may be stale if we get from
             lower, but cache updated in meantime *)
          let new_entry = Lower vopt_from_lower in
          let c = { c with lru_state=L.add k new_entry c.lru_state } in
          (* also may need to evict *)
          get_evictees c |> fun exc ->
          (vopt_from_lower, exc))
      | Some v -> In_cache v
    in

    let _ = find in


    (* FIXME TODO we need to also handle the cases where we flush to
       lower immediately *)

    
    (* [perform] is to factor out the insert/delete commonality NOTE
       entry_type is not Lower *)
    let perform k entry c = 
      assert(not (Entry.is_Lower entry));
      let c = {c with lru_state=(L.add k entry c.lru_state) } in
      get_evictees c
    in

    let insert k v c = perform k (Insert {value=v; dirty=true }) c in

    (* TODO make insert_many more efficient *)
    (* let insert_many k v kvs c = insert k v c |> fun c -> (kvs,c) in *)

    let delete k c = perform k (Delete {dirty=true}) c in

    (* NOTE for in_cache case, this returns the old (possibly dirty)
       entry; if indeed the entry is dirty we can flush to lower *)
    let sync_key k (c:(k,v,lru)lim_state) = 
      (* FIXME do we want to change the time? *)
      (* if present, change dirty bit and return entry type; NOTE the
         entry must be flushed to lower somehow - this only deals with
         the local change *)
      L.find k c.lru_state |> function
      | None -> Not_in_cache ()
      | Some e -> 
        let e' = Entry.mark_clean e in
        L.add k e' c.lru_state |> fun lru_state ->
        (* NOTE this returns the old entry *)
        In_cache(e,{c with lru_state})
    in

    let _ = sync_key in

    (find,insert,delete,sync_key)



  let _ : 
unit ->
(k ->
 (k, v, lru) lim_state ->
 (L.v,
  vopt_from_lower:v option ->
  lim_state:(k, v, lru) lim_state ->
  v option * (k, v, lru) evictees_x_lim_state)
 maybe_in_cache) *
(k -> v -> ('a, 'b, lru) lim_state -> (k, v, lru) evictees_x_lim_state) *
(k -> ('c, 'd, lru) lim_state -> (k, v, lru) evictees_x_lim_state) *
(k ->
 (k, v, lru) lim_state ->
 (L.v * ('e, 'f, lru) lim_state, unit) maybe_in_cache)
    = make_cached_map



  (* export as record ------------------------------------------------- *)


  (** Package up the operations in a record *)
  let make_lru_in_mem () = 
    make_cached_map () |> fun (find,insert,delete,sync_key) ->
    Lru_in_mem_ops.{ find; insert; delete; sync_key }

  let make_init_lim_state ~max_size ~evict_count  = {
    max_size; evict_count; lru_state = L.empty max_size;

    (* FIXME following v inefficient *)
    compare_lru=(fun l1 l2 -> Pervasives.compare (L.to_list l1) (L.to_list l2))
  }

  let _ : unit -> (k, v, lru) Lru_in_mem_ops.lru_in_mem_ops = make_lru_in_mem
end

let make_lru_in_mem_ops (type k v lru) lru_fc = 
  let module S = struct
    type nonrec k=k
    type nonrec v=v
    type nonrec lru=lru
    let lru_fc = lru_fc
  end
  in
  let module I = Internal(S) in
  I.make_lru_in_mem ()

let _ : ('a, 'b, 'c) lru_fc -> ('a, 'b, 'c) Lru_in_mem_ops.lru_in_mem_ops 
  = make_lru_in_mem_ops

let make_init_lim_state (type k v lru) ~max_size ~evict_count ~(lru_fc:(k,v,lru)lru_fc) = 
  let module L = (val lru_fc) in
  ({ max_size; evict_count; lru_state = L.empty max_size; 
     compare_lru=(fun l1 l2 -> Pervasives.compare (L.to_list l1) (L.to_list l2))
   } : (k,v,lru)lim_state)

let _ : max_size:int ->
evict_count:int -> lru_fc:('a, 'b, 'c) lru_fc -> ('a, 'b, 'c) lim_state
= make_init_lim_state
  

(** Don't open this directly; use [Int_int.ops] etc *)
module Int_int = struct
  type k = int
  type v = int

  module Internal = struct
    type nonrec k = k
    type nonrec v = v
    let  compare : k -> k -> int = Pervasives.compare
  end
  include Make_lru_fc(Internal)


  let ops = make_lru_in_mem_ops lru_fc
  let _ = ops

  let initial_state ~max_size ~evict_count = make_init_lim_state ~max_size ~evict_count ~lru_fc
  let _ = initial_state
end

(* FIXME other common instances *)  


(*
  module Internal = struct

    module W = struct type t = v entry let weight: t->int = fun _ -> 1 end    

    module Lru' = Lru.F.Make(struct type t = k let compare: k -> k -> int = Pervasives.compare end)(W)

    type lru = Lru'.t

    let lru_fc = (module Lru' : Lru.F.S with type k=k and type v=v entry and type t=lru) 

  end

  type lru = Internal.lru

  let lru_fc : (k,v,lru) lru_fc = Internal.lru_fc
*)
