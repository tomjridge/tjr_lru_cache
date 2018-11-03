(* Polymorphic map ops courtesy of extlib *)

(* following needs polymorphic map - 
   batteries? no, only Pervasives.compare
   extlib? yes, allows parameterization by compare 

FIXME can also use first class modules to produce polymorphic ops
*)


type ('k,'v) t = (*ExtLib.*) ('k,'v)PMap.t
let mem : 'a -> ('a, 'b) t -> bool = PMap.mem
let map: ('v -> 'u) -> ('k,'v) t -> ('k,'u) t = PMap.map
(* FIXME bindings and cardinal should be in ExtLib; also others *)
let bindings: ('k,'v) t -> ('k * 'v) list = (fun m -> 
    let bs = ref [] in
    PMap.iter (fun k v -> bs:=(k,v)::!bs) m;
    List.rev !bs)
let empty: ('k -> 'k -> int) -> ('k,'v) t = PMap.create
let cardinal: ('k,'v) t -> int = (fun m ->
    let x = ref 0 in
    PMap.iter (fun k v -> x:=!x+1) m;
    !x)
let iter: ('k -> 'v -> unit) -> ('k,'v) t -> unit = PMap.iter
let find: 'k -> ('k,'v) t -> 'v = PMap.find
let find_opt: 'k -> ('k,'v) t -> 'v option = fun k t -> 
  try 
    Some(find k t)
  with Not_found -> None
let remove: 'k -> ('k,'v) t -> ('k,'v) t = PMap.remove
let add: 'k -> 'v -> ('k,'v) t -> ('k,'v) t = PMap.add
let foldi: ('a -> 'b -> 'c -> 'c) -> ('a , 'b) t -> 'c -> 'c = PMap.foldi
