(** A single-threaded in-memory LRU cache, as a prelude to the real
   thing.

We assume all operations complete quickly from the cache. Operations
   that need to go to disk are modelled using explicit continuation
   passing.

The difficulty is how to extend this to deal with disk-backed
   eviction.

NOTE that the operations occur not in a monad - instead, explicit
   state passing is used, and we return the possible evictees; this is
   a prelude to the real version where we need to do something with
   the evictees

*)

open Im_intf

module type S = sig
  type k
  type v
  val k_cmp : k -> k -> int
end

module type T = sig
  module S:S
  open S
  type lru

  val lru_factory_im : (k, v, lru) lru_factory_im
end


(** With full sig *)
module Make_v1(S:S) = struct
  module S=S
  open S

  (** Construct the LRU *)

  module W = struct type t = v lru_entry let weight: t->int = fun _ -> 1 end    

  module L = Lru.F.Make(struct type t = k let compare: k -> k -> int = k_cmp end)(W)
      
  type lru = L.t

  (* NOTE evictees are removed from the map, so no need to mark clean *)
               
  (** Lift L.find to lru_state_im. FIXME inline *)
  let find_in_cache (k:k) (c:lru lru_state_im) : v lru_entry option = 
    L.find k c.lru_state 

  let _ = find_in_cache



(*
  (** Type abbrev for find *)
  type maybe_in_cache_f = 
    (W.t,
     vopt_from_lower:v option ->
     lru_state_im:lru lru_state_im ->
     v option * (k, v, lru) Tjr_lru_cache__.Im_intf.evictees_x_state)
      Tjr_lru_cache__.Im_intf.maybe_in_cache
*)

  (** Construct the cached map, using existing map find operation.

      NOTE the idea for [find] is that we execute a quick step to handle the
      case that there is an entry in the cache. If there isn't, we make a
      slow call to the lower layer, and when the result arrives we make
      another update to the cache state with the then current state of
      the cache. To avoid the risk of stale results being returned from
      lower, we have to tag lower results with some kind of
      monotonically-increasing index.
  *)
  module Make_cached_map = struct

    (* Returns None if no evictees need to be flushed, or Some(evictees)
        otherwise *)
    let get_evictees (c:lru lru_state_im) : (k, v, lru) evictees_x_state = 
      (* mark "ab"; *)
      let card = L.size c.lru_state in
      (* mark "bc"; *)
      match card > c.max_size with (* FIXME inefficient *)
      | false -> {evictees=[]; lru_state_im=c}
      | true -> 
        (* how many to evict? *)
        let n = c.evict_count in
        (* for non-dirty, we just remove from map; for dirty we
           must flush to lower *)        
        let evictees,lru_state = 
          ([],c.lru_state,n) |> iter_k (fun ~k:kont (es,s,n) -> 
              match n <= 0 with
              | true -> (es,s)
              | false -> 
                L.pop_lru s |> function
                | None -> (es,s)
                | Some ((k,e),s) -> 
                  (* NOTE only add the dirty evictees *)
                  match entry_is_dirty e with
                  | true -> kont ( (dirty_entry_to_kvop k e) ::es,s,n-1)
                  | false -> kont (es,s,n-1))
        in
        (* now we have evictees, new queue, and new map *)
        let c = {c with lru_state} in
        (* mark "cd"; *)
        {evictees;lru_state_im=c}


    (* NOTE that if find pulls an entry into the cache, we may have to
         get rid of evictees; so a read causes a write; but this should be
         relatively infrequent *)
    let find (k:k) c = 
      find_in_cache k c |> function
      | None -> `Not_in
      | Some(Insert {value;_}) -> `In (Some value)
      | Some(Delete _) -> `In None
      | Some(Lower vopt) -> `In vopt

(*
 |> function
      | None -> 
        Not_in_cache (fun ~vopt_from_lower ~lru_state_im:(c:lru lru_state_im) ->
            (* need to update map before returning *)
            (* FIXME concurrency concerns: v may be stale if we get from
               lower, but cache updated in meantime *)
            let new_entry = Lower vopt_from_lower in
            let c = { c with lru_state=L.add k new_entry c.lru_state } in
            (* also may need to evict *)
            get_evictees c |> fun exc ->
            (vopt_from_lower, exc))
      | Some v -> In_cache v
*)

    let _ = find


    (* FIXME TODO we need to also handle the cases where we flush to
       lower immediately *)

    
    (* [perform] is to factor out the insert/delete commonality NOTE
       entry_type is not Lower *)
    let perform k entry c = 
      (* assert(not (Lru_entry.is_Lower entry)); *)
      let c = {c with lru_state=(L.add k entry c.lru_state) } in
      get_evictees c

    let _ = perform
      
    let insert k v c = perform k (Insert {value=v; dirty=true }) c

    let _ = insert

    (* TODO make insert_many more efficient *)
    (* let insert_many k v kvs c = insert k v c |> fun c -> (kvs,c) in *)

    let delete k c = perform k (Delete {dirty=true}) c

    let update_from_lower k vopt lru = 
      perform k (Lower vopt) lru
        
    (* NOTE for in_cache case, this returns the old (possibly dirty)
       entry; if indeed the entry is dirty we can flush to lower *)
    let sync_key k (c:lru lru_state_im) : (('k,'v)kvop * lru lru_state_im) option = 
      (* FIXME do we want to change the time? *)
      (* if present, change dirty bit and return entry type; NOTE the
         entry must be flushed to lower somehow - this only deals with
         the local change *)
      match find_in_cache k c with
      | None -> None
      | Some e -> 
        match entry_is_dirty e with
        | false -> None
        | true -> 
          let e' = Lru_entry.mark_clean e in
          let lru_state = L.add k e' c.lru_state in
          Some(dirty_entry_to_kvop k e,{c with lru_state})

    let sync_all_keys (s:lru lru_state_im) : _ evictees_x_state = 
      (L.to_list s.lru_state,L.empty s.max_size,[]) |> iter_k (fun ~k:kont (kvs,lru,es) -> 
          (* kvs are to process; lru is the new clean lru; es are the evictees *)
          match kvs with
          | [] -> 
            {evictees=es;
             lru_state_im={s with lru_state=lru}}
          | (k,v)::kvs -> 
            let lru = L.add k (mark_clean v) lru in
            let es = 
              match entry_is_dirty v with
              | true -> (dirty_entry_to_kvop k v)::es
              | false -> es
            in
            kont (kvs,lru,es))
  end


  (** Package up the operations in a record *)
  let lru_ops_im = 
    let open Make_cached_map in     
    Lru_ops_im.{ find; insert; delete; update_from_lower; sync_key; sync_all_keys }

  let _ : (k, v, lru) lru_ops_im = lru_ops_im

  let empty (params:lru_params_im) = 
    let max_size = params#max_size in
    {
      max_size=params#max_size; 
      evict_count=params#evict_count; 
      lru_state = L.empty max_size;
      compare_lru=(fun l1 l2 -> Stdlib.compare (L.to_list l1) (L.to_list l2))
      (* FIXME compare_lru v inefficient; only used for debug? *)
    }

  let lru_factory_im : _ lru_factory_im = object
    method empty=empty
    method lru_ops_im=lru_ops_im
  end

end

module Make_v2(S:S) : T with module S=S = struct
  include Make_v1(S)
end

module Make = Make_v2

module Examples = struct
  (** Don't open this directly; use [Int_int.ops] etc *)
  module Int_int = struct
    module S = struct
      type k = int
      type v = int
      let k_cmp = Int_.compare
    end
    module M = Make(S)
    type lru = M.lru
    let lru_factory_im = M.lru_factory_im
  end
end
(* FIXME other common instances *)  
