(** The LRU provides a flushable map API. 

To support blocking/non-blocking modes, we can optionally provide a callback function. 

The following are developer notes.

{2 Sync behaviour }

Our main use for this module is as a frontend to an uncached KV store
(see tjr_kv). 

There we target the following API: FIXME perhaps move the api here?

{[
  type persist_mode = Persist_later | Persist_now

  type mode = persist_mode

  type ('k,'v,'t) flushable_map_ops = {
    find: 'k -> ('v option,'t) m;
    insert: mode -> 'k -> 'v -> (unit,'t) m;
    delete: mode -> 'k -> (unit,'t) m;
    sync_key: 'k -> (unit,'t) m;
    sync_all_keys: unit -> (unit,'t) m;
  }
end

]}

{2 Concurrency}

This API is expected to be used by many threads. In particular, for [sync_key] we may have the following actions:

- thread A: [insert_T(k,v); sync_key(k)]
- thread B: [insert_T(k,v'); sync_key(k)]

Then the insert of [v'] may be synced by thread A's [sync_key].


Similarly, for [sync_all_keys]:

- thread A: [insert_T(k,v); sync_all_keys]
- thread B: [insert_T(k,v')]

Thread A's call to [sync_all_keys] may result in [(k,v')] rather than [(k,v)].


{2 Non-blocking design}

We want sync operations to be non-blocking.

- For the thread that issues a sync, we want to have the option of avoiding waiting for the sync to complete. For this, we expect the thread doing the call to launch an async thread to invoke the call.

- Transient operations from other threads should not block while a
  sync is taking place. FIXME implement this

- A concurrent [sync_all_keys] operation (perhaps occuring halfway
  through an existing sync) should avoid re-syncing keys that have
  already been synced.


The design (for tjr_kv) has a single active thread servicing the
pcache. If this thread is solely responsible for [sync_all_keys], then
transient operations will block (till the thread completes and can
service another message from the LRU cache).

Thus, the question is how to service the [sync_all_keys] operation in
the pcache.

FIXME another issue: how to suspend a thread in the LRU till we
receive notification of the sync completion. For LWT we can just pass
some mbox var. For the API perhaps we have a callback function which
updates the system state.

NOTE LMDB has "There can be multiple simultaneously active read-only
transactions but only one that can write. Once a single read-write
transaction is opened, all further attempts to begin one will block
until the first one is committed or aborted."

{2 Consumed API}

The LRU provides a "syncable/blocking" map API.

FIXME terminology: perhaps persistent_mode should be sync_mode? Or
should we separate the sync operation from the durability and blocking
requirement?

FIXME sync is a bad word because it implies symmetrical operation,
whereas a sync is really an operation at the API which forces changes
downwards (flush has this connotation).

We build the LRU on top of an API which exposes map-like operations
together with a sync operation. So the LRU is really just an LRU cache
of a syncable map (ie, it consumes a syncable map and produces a syncable map).

A non-blocking operation can put a msg on the queue and return
immediately. A blocking operation has to listen for the reply before
returning. There is a question of whether to implement a reply queue
or use some simpler mechanism (callback or mbox var). Probably best to
use a simple callback.


{2 Marking entries clean}

On a [sync_all_keys call], we can dispatch to the lower layer, and mark
all entries clean at that point, without waiting for the return. We
only need to wait for the return if the [sync_all_keys] call is blocking
(but we can still mark the entries clean... the blocking behaviour is
a notification thing, but marking entries clean is just to record in
memory that these entries no longer need to be flushed).

{%html: <hr/> %}


*)
let dummy = ()
