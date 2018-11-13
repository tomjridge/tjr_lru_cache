
module Types = struct

  (** The cache maintains an internal clock. *)
  type time = int

  (** Entries are marked using a bool; true means "this is dirty". *)
  type dirty = bool


(** Cache map entries; values in the map are tagged with a last-accessed time and a dirty flag

Entries in the cache for key k:

- Insert v (dirty=true/false)
    {ul {li this occurs on insert}}
- Delete   (dirty=true/false)
- Lower vopt 
    {ul {li this occurs when we check the lower layer for a non-existing entry in cache}}
- (No entry)
    {ul {li for a key that hasn't been seen before}}

Additionally, each entry has a last-accessed time

 *)
  type 'v entry_type = 
    | Insert of { value: 'v; dirty:dirty }
    | Delete of { dirty:dirty }
    | Lower of 'v option

  type 'v entry = { entry_type: 'v entry_type; atime: time }

end

include Types

(* fns on entries and entry_types ------------------------------------ *)

let is_Lower = function Lower _ -> true | _ -> false 

let mark_clean = function
  | Insert {value;dirty} -> Insert {value;dirty=false}
  | Delete {dirty} -> Delete {dirty=false}
  | Lower vopt -> Lower vopt

let entry_type_is_dirty = function
  | Insert {value;dirty} -> dirty
  | Delete {dirty} -> dirty
  | Lower vopt -> false


