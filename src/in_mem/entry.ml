(** The cache maintains an internal clock. *)
type time = int

(** Entries are marked using a bool; true means "this is dirty". *)
type dirty = bool

type 'v entry_type = 
  | Insert of { value: 'v; dirty:dirty }
  | Delete of { dirty:dirty }
  | Lower of 'v option

type 'v entry = { entry_type: 'v entry_type; atime: time }
