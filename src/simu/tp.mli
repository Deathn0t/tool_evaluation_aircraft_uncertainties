type time = float  (* sec *)
type speed = float  (* knots *)
type xy = float * float  (* Nm x Nm *)
type seg = xy * xy
type t

val create: ?dspeed:float -> speed -> time -> xy array -> t
val start_t: t -> time
val end_t: t -> time
val trace: t -> xy array

val turn: t -> time -> float -> time -> t
val dev: t -> time -> xy -> t

val detect: t -> t -> time * time  (* conflict begining time, end time *)
val seg_detect: t -> t -> (seg list) * (seg list)

val save: (time * t) array -> string -> unit  (* (t0min, t) array *)
val load: string -> (time * t) array




