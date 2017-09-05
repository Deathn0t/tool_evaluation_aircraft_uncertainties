type time = float  (* sec *)
type speed = float  (* Nm / sec *)
type heading = float  (* radians *)

type xy = float * float  (* Nm x Nm *)
type t = (time * xy) array

val step: time
val sep: float

val create: speed -> time -> xy list -> t

val t_start: t -> time
val t_end: t -> time
val delay: t -> speed -> time
val pos_heading: t -> time -> xy * heading
val comet: t -> time -> xy array

val predict:  t -> speed -> time -> t

val turn: t -> speed -> time -> float -> time -> t
val dev: t -> speed -> time -> xy -> t
val apply: t -> t -> speed -> time -> t

val detect: t -> t -> time -> (time * time) list
val segments: t -> t -> time -> ((xy * xy) list) * ((xy * xy) list)

val roundabout: float -> int -> (speed * t) array

val save: string -> (time * speed * t) array -> unit
val load: string -> (time * speed * t) array
 
