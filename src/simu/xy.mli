(* Float coords 2D --------------------------------------------------------- *)

type xy = float * float

val mul: float -> xy -> xy
val add: xy -> xy -> xy
val sub: xy -> xy -> xy

val sca: xy -> xy -> float
val det: xy -> xy -> float

val norm2: xy -> float
val norm: xy -> float

val bary: float * xy -> float * xy -> float -> xy

val pi: float
val twopi: float
val radians: float
val degrees: float
val angle: xy -> float
val polar: float -> xy
val mod_twopi: float -> float
val add_angle: float -> float -> float
val sub_angle: float -> float -> float

val dist_seg_seg: xy * xy -> xy * xy-> float
