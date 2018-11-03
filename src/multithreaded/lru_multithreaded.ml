(** A multithreaded / concurrent-safe implementation of the LRU
   interface.

This extends the in-memory explicit-state-passing code to the monad.


We maintain the [lru_state]:

- cache state

- a map from k to a wait list (of threads that are waiting for a find
   operation to complete at the disk layer)



The lower "disk" layer is defined in {!module: Pcache_interface}.

NOTE Access to the lower layer is serialized.

NOTE Access to the [lru_state] is serialized.

For find: 
- We access the [lru_state]  (exlusively/serially).

- We check if the find would succeed without going to disk. If so, we are done.

- Otherwise, we check the wait list for the given key. If this is non-existent,   we create it, execute the lower call to find, and on return we alter the
  then-current cache state (updates are serialized), and notify all pending threads on the wait list, and delete the wait list.

For [Persist_later] insert and delete, we access the [lru_state], modify the cache state. If we have evictees, we need to flush these to lower. The problem is that we may not want to hold the [lru_state] during this eviction. At the moment, we hold the lock.

For [Persist_now] insert and delete, we do as above. If we have evictees, we can add the op to the evictees list (so, the size of the evictees should be 1 less than the amount that can safely fit in one block FIXME) and then call the lower layer. 

For sync_key (and sync_keys) we simply take the entry in the cache (if any) and mark it clean and flush to lower.




FIXME It may be that we want to prioritize non-evictee updates in the pcache over evictee writes. But this introduces much complication into the pcache.

- evictees may arise from the in-mem operation; however, no-one is
   waiting on these; so we just call the pcache to write evictees to
   disk; if we do not have an active pcache thread, this may block the
   pcache; is there a strategy to sync the evictees that doesn't block
   the pcache? could maintain an in-mem evictees map that is gradually
   pushed to disk whilst other requests are served;
    - on the other hand, we expect that the pcache can flush evictees to disk in 1 block write


- for in-mem, when we return with cache_state and evictees, we aim to
   unblock the cache state ASAP; but can we do this without ensuring
   that the evictees are on-disk? Yes (the evictees can be written to
   disk in a delayed fashion), but we must be careful that the
   evictees are taken into account when accessing the lower layer. The
   implementation of this is somewhat complicated. So a first version
   should just block until the evictees are known to be on disk.



- FIXME in order to handle evictees quickly, pcache should pre-allocate
   blocks (so we don't need toupdate the free map); perhaps each block
   should then be written with a next pointer if possible (even if not
   full)



 *)


let _ = ()
