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


module Profiler = Make_profiler()
open Profiler

module Internal(S:sig
    type k
    type v
    val compare: k -> k -> int
  end) = struct

  open S

  (* NOTE evictees are removed from the map, so no need to mark clean *)

  module W = struct type t = v entry let weight: t->int = fun _ -> 1 end

  module L = Lru.F.Make
      (struct type t = k let compare = compare end)
      (W)

  
  (** Attempt to locate key in cache. *)
  let find_in_cache (k:'k) (c:('k,'v,'lru)cache_state) = 
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
    let get_evictees (c:('k,'v,'lru)cache_state) = 
      mark "ab";
      let card = L.size c.lru_state in
      mark "bc";
      match card > c.max_size with (* FIXME inefficient *)
      | false -> (None,c)
      | true -> 
        (* how many to evict? *)
        let n = c.evict_count in
        (* for non-dirty, we just remove from map; for dirty we
           must flush to lower *)        
        let evictees,lru_state = 
          ([],c.lru_state,n) |> List_.iter_break (fun (es,s,n) -> 
              match n <= 0 with
              | true -> `Break (es,s)
              | false -> 
                L.pop_lru s |> function
                | None -> `Break (es,s)
                | Some ((k,e),s) -> `Continue(e::es,s,(n-1)))
        in
        (* now we have evictees, new queue, and new map *)
        let c = {c with lru_state} in
        mark "cd";
        (Some evictees,c)
    in


    (* NOTE that if find pulls an entry into the cache, we may have to
         get rid of evictees; so a read causes a write; but this should be
         relatively infrequent *)
    let find (k:'k) c = 
      find_in_cache k c |> fun (v,c) -> 
      match v with
      | None -> 
        Not_in_cache (fun ~vopt_from_lower ~cache_state ->          
            let c = cache_state in
            let c = tick c in
            (* need to update map before returning *)
            (* FIXME concurrency concerns: v may be stale if we get from
               lower, but cache updated in meantime *)
            let new_entry = Lower vopt_from_lower in
            { c with
              cache_map=m.add k 
                  {entry_type=new_entry; atime=c.current_time} 
                  c.cache_map;
              queue=c.queue |> Queue.add c.current_time k } |> fun c ->
            (* also may need to evict *)
            get_evictees c |> fun (es,c) ->
            (vopt_from_lower,`Evictees es, `Cache_state c))
      | Some v -> 
        `In_cache v
    in

    let _ = find in


    (* FIXME TODO we need to also handle the cases where we flush to
       lower immediately *)

    (* NOTE entry_type is not Lower *)
    let perform k entry_type c = 
      mark "_";
      assert(not (Entry.is_Lower entry_type));
      let m = c.cache_map_ops in
      let c = tick c in
      let e = 
        try 
          Some (m.find k c.cache_map)
        with Not_found -> None 
      in
      mark "bc";
      let e' = {entry_type; atime=c.current_time } in
      (* new entry in cache_map *)
      let c = {c with cache_map=(m.add k e' c.cache_map) } in
      (* maybe remove existing entry from queue *)
      mark "cd";
      let c = 
        match e with
        | None -> c
        | Some e -> 
          {c with queue=(Queue.remove e.atime c.queue) } 
      in
      mark "de"; 
      (* add new entry *)
      let c = {c with queue=(Queue.add e'.atime k c.queue) } in
      mark "ef";
      get_evictees c |> fun (es,c) -> 
      mark "fg";
      (`Evictees es, `Cache_state c)
    in

    let insert k v c = perform k (Insert {value=v; dirty=true }) c in

    (* TODO make insert_many more efficient *)
    (* let insert_many k v kvs c = insert k v c |> fun c -> (kvs,c) in *)

    let delete k c = perform k (Delete {dirty=true}) c in


    let sync_key k c = 
      (* FIXME do we want to change the time? *)
      (* if present, change dirty bit and return entry type *)
      let m = c.cache_map_ops in
      m.find_opt k c.cache_map |> function
      | None -> `Not_present
      | Some e -> 
        let e' = { e with entry_type=Entry.mark_clean e.entry_type} in
        m.add k e' c.cache_map |> fun cache_map ->
        (* NOTE this returns the old entry *)
        `Present(e,{c with cache_map})
    in

    let _ = sync_key in

    fun kk -> kk ~find ~insert ~delete ~sync_key



  let _ : 
    unit ->
    (find:('k ->
           ('k, 'a, 'lru) cache_state ->
           [> `In_cache of 'a entry
           | `Not_in_cache of
                vopt_from_lower:'a option ->
                cache_state:('k, 'v, 'lru) cache_state ->
                'a option * [> `Evictees of ('k * 'v entry) list option ] *
                [> `Cache_state of ('k, 'v, 'lru) cache_state ] ]) ->
     insert:('k ->
             'v ->
             ('k, 'v, 'lru) cache_state ->
             [> `Evictees of ('k * 'v entry) list option ] *
             [> `Cache_state of ('k, 'v, 'lru) cache_state ]) ->
     delete:('k ->
             ('k, 'v, 'lru) cache_state ->
             [> `Evictees of ('k * 'v entry) list option ] *
             [> `Cache_state of ('k, 'v, 'lru) cache_state ]) ->
     sync_key:('b ->
               ('b, 'c, 'd) cache_state ->
               [> `Not_present | `Present of 'c entry * ('b, 'c, 'd) cache_state ]) ->
     'e) ->
    'e
    = make_cached_map



  (* export as record ------------------------------------------------- *)


  (** Package up the operations in a record *)
  let make_lru_in_mem () = 
    make_cached_map () @@ fun ~find ~insert ~delete ~sync_key -> 
    Lru_in_mem_ops.{ find; insert; delete; sync_key }
end

let make_lru_in_mem () : ('k,'v,'lru,'t)Lru_in_mem_ops.lru_in_mem_ops = Internal.make_lru_in_mem ()
