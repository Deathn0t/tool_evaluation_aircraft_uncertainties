type xy = float * float

type map

val create : Widget.canvas Widget.widget -> int -> int -> float -> map

val cv_xy : map -> xy -> int * int

val world_xy : map -> int * int -> xy

val tag_id : string -> int -> string

val get_id : string list -> int

val draw_sep : map -> unit

val draw_route : map -> ('a * Xy.xy) array -> int -> unit

val draw_dev : map -> ('a * Xy.xy) array -> 'b -> unit

val draw_acft : map -> Acft.t array -> int -> unit

val draw_conflict_lines :
  map -> (int * (Xy.xy * Xy.xy)) list array -> int -> unit

val eval : string array -> string
