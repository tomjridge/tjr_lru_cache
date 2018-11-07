(** A calling mode can be "later" (ie, perform immediately in mem
    and return; persist sometime later) or "now", in which case we
    can optionally supply a callback. A call can be blocking or
    non-blocking. For non-blocking, the calling thread is expected to
    launch an async promise that resolves without blocking the
    caller. Thus, we implement only blocking versions of calls. *)
type persist_mode = Persist_later | Persist_now

type mode = persist_mode

